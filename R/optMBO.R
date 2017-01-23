
mboSaveMode = TRUE

#' @title mbo backend configuration
#' 
#' @description
#' Create an \code{AutomlrBackendConfig} object that can be fed to
#' \code{\link{automlr}} to perform optimization with the "mbo" backend.
#' 
#' @param focussearch.restarts [\code{integer(1)}]\cr
#'   number of restarts to perform in focussearch surrogate model optimizer
#' @param focussearch.maxit [\code{integer(1)}]\cr
#'   number of iterations for one focussearch round
#' @param focussearch.points [\code{integer(1)}]\cr
#'   number of points to sample in focussearch.
#' @param mbo.save.mode [\code{logical(1)}]\cr
#'   Simplify search space for mbo backend. You should probably not change the
#'   default.
#' @param resampling [\code{ResampleDesc}]\cr
#'   resampling to evaluate model performance.
#' @export
makeBackendconfMbo = registerBackend("mbo",
    function(focussearch.restarts = 1, focussearch.maxit = 5,
        focussearch.points = 100, mbo.save.mode = TRUE, resampling = hout) {
      assertCount(focussearch.restarts)
      assertCount(focussearch.maxit)
      assertCount(focussearch.points)
      assertClass(resampling, "ResampleDesc")
      argsToList()
    })

amaddprior.ammbo = function(env, prior) {
  NULL
}

amgetprior.ammbo = function(env) {
  NULL
}

amsetup.ammbo = function(env, opt, prior, learner, task, measure, verbosity) {
  requirePackages("mlrMBO", why = "optMBO", default.method = "load")
  requirePackages("smoof", why = "optMBO", default.method = "load")
  # FIXME things that could be variable:
  #  resampling: holdout, cv, or something adaptive?
  #  infill control: focussearch, something else? how many points?
  env$runtimeEnv = environment()
  
  zeroWalltime = 0
  zeroEvals = 0
  
  # the following must be set here since mbo() creates the initial design,
  # which queries the budget an numcpus.
  numcpus = parallelGetOptions()$settings$cpus
  numcpus[is.na(numcpus)] = 1
  
  budget = 0
  
  learner = adjustLearnerVerbosity(learner, verbosity)
  
  isOutOfBudget = function(opt.state) {
    stopcondition(budget, spentBudget(opt.state, parent.env(environment())))
  }
  
  objectiveFun = function(x) {
    if (mboSaveMode) {
      x = complicateParams(x, getSearchspace(learner))
    } else {
      x = removeMissingValues(x)
    }
    l = setHyperPars(learner, par.vals = x)

    hardTimeoutRemaining = hardTimeout - proc.time()[3]

    rwt = runWithTimeout(
        resample(l, task, resDesc, list(measure), show.info = FALSE)$aggr,
        hardTimeoutRemaining, throwError = TRUE)
    rwt$result
  }
  
  usedParset = getSearchspace(learner)
  if (mboSaveMode) {
    usedParset = simplifyParams(usedParset)
  }
  
  for (p in usedParset$pars) {
    if (isDiscrete(p) && length(p$values) > 53) {
      stopf(paste("Parameter '%s' has more than 53 possible (in fact %s)",
              "values. Since mbo uses pencil and paper to calculate things,",
              "it can't handle numbers that big.%s"), p$id, length(p$values),
          ifelse(p$id != "selected.learner", "",
              " Try to use searchspace = mlrLightweight[NoWrap]."))
    }
  }
  
  resDesc = makeResampleDesc("Holdout")
  objective = smoof::makeSingleObjectiveFunction(
      name = "automlr learner optimization",
      id = "automlr.objective",
      has.simple.signature = FALSE,
      vectorized = FALSE,
      noisy = TRUE,
      minimize = measure$minimize,
      par.set = usedParset,
      fn = objectiveFun)
  
  imputeval = generateRealisticImputeVal(measure, learner, task)
  imputefun = function(x, y, opt.path) imputeval
  
  control = mlrMBO::makeMBOControl(impute.y.fun = imputefun)
  control = mlrMBO::setMBOControlInfill(control, opt = "focussearch",
      opt.focussearch.points = mbo.focussearch.points,
      opt.focussearch.maxit = mbo.focussearch.maxit,
      opt.restarts = mbo.focussearch.restarts)
  control = mlrMBO::setMBOControlTermination(control, iters = NULL,
      more.termination.conds = list(function(opt.state) {
            if (isOutOfBudget(opt.state)) {
              list(term = TRUE, message = "automlr term", code = "iter")
            } else {
              list(term = FALSE, message = NA_character_, code = "iter")
            }
          }))
  
  
  
  mboLearner = mlrMBO:::checkLearner(NULL, usedParset, control, objective)
  mboLearner$config = list(on.learner.error = "stop",
      on.learner.warning = "warn", show.learner.output = TRUE)
  mboLearner = makePreprocWrapperAm(mboLearner, ppa.impute.factor = "distinct",
      ppa.impute.numeric = "median")
  
  myMBO = mlrMBO::mbo
  environment(myMBO) = new.env(parent = asNamespace("mlrMBO"))
  environment(myMBO)$mboFinalize2 = identity
  env$opt.state = myMBO(objective, learner = mboLearner, control = control,
      show.info = TRUE)
  # clean up environment, it is used in objectiveFun().
  rm(myMBO, prior, env)
}


amresult.ammbo = function(env) {
  mboResult = mlrMBO:::mboFinalize2(env$opt.state)
  list(learner = env$learner,
      opt.point = removeMissingValues(mboResult$x),
      opt.val = mboResult$y,
      opt.path = mboResult$opt.path,
      result = mboResult)
}

amoptimize.ammbo = function(env, stepbudget, verbosity, deadline) {
  # initialize for spent budget computation
  zero = env$runtimeEnv
  
  # FIXME: right now, the infill crit optimization does not respect the
  # deadline. It is possible to change this, by changing the termination
  # criterion of the mbo run so that only one iteration gets performed per
  # call, and additionally creating backups of the opt.state before each call.
  # I will choose the elegant over the correct solution here though.
  zero$hardTimeout = proc.time()[3] + deadline
  
  zero$budget = stepbudget

  zero$learner = adjustLearnerVerbosity(zero$learner, verbosity)
  
  result = runWithTimeout(mlrMBO:::mboTemplate.OptState(env$opt.state),
      deadline, backend = "native")
  spent = spentBudget(env$opt.state, zero)
  zero$zeroWalltime %+=% spent["walltime"]
  zero$zeroEvals %+=% spent["evals"]
  
  spent
}

spentBudget = function(opt.state, zero) {
  spent = numeric(0)
  totalWallTime = as.numeric(opt.state$time.used, units = "secs")
  spent["walltime"] = totalWallTime - zero$zeroWalltime
  spent["evals"] = getOptPathLength(opt.state$opt.path) - zero$zeroEvals
  spent
}



