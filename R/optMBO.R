
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
      checkCount(focussearch.restarts)
      checkCount(focussearch.maxit)
      checkCount(focussearch.points)
      checkClass(resampling, "ResampleDesc")
      argsToList()
    })



amaddprior.ammbo = function(env, prior) {
  NULL
}

amgetprior.ammbo = function(env) {
  NULL
}

amsetup.ammbo = function(env, prior, learner, task, measure, verbosity) {
  requirePackages("mlrMBO", why = "optMBO", default.method = "load")
  # fix the dumb mlrMBO bug
  mlrMBO:::.onAttach()
  requirePackages("smoof", why = "optMBO", default.method = "load")
  # FIXME things that could be variable:
  #  resampling: holdout, cv, or something adaptive?
  #  infill control: focussearch, something else? how many points?
  env$runtimeEnv = environment()
  
  zeroWalltime = 0
  zeroModeltime = 0
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
      x = complicateParams(x, learner$searchspace)
    } else {
      x = removeMissingValues(x)
    }
    l = setHyperPars(learner, par.vals = x)
    resample(l, task, resDesc, list(measure), show.info = FALSE)$aggr
  }
  
  usedParset = learner$searchspace
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
      more.stop.conds = list(function(opt.state) {
            if (isOutOfBudget(opt.state)) {
              list(term = TRUE, message = "automlr term", code = "iter")
            } else {
              list(term = FALSE, message = NA_character_, code = "iter")
            }
          }))
  
  
  
  mboLearner = mlrMBO:::checkLearner(NULL, usedParset, control)
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

amoptimize.ammbo = function(env, stepbudget, verbosity) {
  # initialize for spent budget computation
  zero = env$runtimeEnv
  zero$numcpus = parallelGetOptions()$settings$cpus
  zero$numcpus[is.na(zero$numcpus)] = 1
  zero$budget = stepbudget
  
  zero$learner = adjustLearnerVerbosity(zero$learner, verbosity)
  
  env$opt.state = mlrMBO:::mboTemplate.OptState(env$opt.state)
  
  spent = spentBudget(env$opt.state, zero)
  zero$zeroWalltime %+=% spent["walltime"]
  zero$zeroModeltime %+=% spent["modeltime"]
  zero$zeroEvals %+=% spent["evals"]
  
  spent
}

spentBudget = function(opt.state, zero) {
  spent = numeric(0)
  totalWallTime = as.numeric(opt.state$time.used, units = "secs")
  totalModelTime = sum(getOptPathExecTimes(opt.state$opt.path))
  spent["walltime"] = totalWallTime - zero$zeroWalltime
  spent["cputime"] = spent["walltime"] * zero$numcpus
  spent["modeltime"] = totalModelTime - zero$zeroModeltime
  spent["evals"] = getOptPathLength(opt.state$opt.path) - zero$zeroEvals
  spent
}

# convert discrete to discrete-string parameters
# also convert logical to discrete parameters
untypeParams = function(parset) {
  parset$pars = lapply(parset$pars, function(par) {
        if (!is.null(par$values)) {
          par$values = as.list(names(par$values))
          names(par$values) = par$values
          par$type = switch(par$type,
              logical = "discrete",
              logicalvector = "discretevector",
              par$type)
        }
        par
      })
  parset
}

# convert vector parameters to multiple nonvector parameters
unvectorParams = function(parset) {
  parset$pars = unlist(recursive = FALSE, lapply(parset$pars, function(par) {
            if (!ParamHelpers:::isVector(par)) {
              return(list(par))
            }
            lapply(seq_len(par$len), function(i) {
                  parcpy = par
                  parcpy$len = 1L
                  parcpy$id = paste0(parcpy$id, i)
                  if (!is.null(parcpy$lower)) {
                    parcpy$lower = parcpy$lower[i]
                  }
                  if (!is.null(parcpy$upper)) {
                    parcpy$upper = parcpy$upper[i]
                  }
                  parcpy$type = switch(parcpy$type,
                      integervector = "integer",
                      numericvector = "numeric",
                      discretevector = "discrete",
                      stopf("Unsupported parameter type '%s'", parcpy$type))
                  parcpy
                })
          }))
  parset
}

simplifyParams = function(parset) {
  parset = mboRequirements(parset)
  parset = untypeParams(parset)
  parset = unvectorParams(parset)
  parset
}

# undo the 'simplifyParams' operation
complicateParams = function(params, origparset) {
  simpleTypeOrig = untypeParams(origparset)
  
  types = getParamTypes(simpleTypeOrig, df.cols = TRUE)
  for (parin in seq_along(params)) {
    params[[parin]] = switch(types[parin],
        integer = as.integer,
        numeric = as.numeric,
        factor = as.character,
        stop("complicateParam got bad type"))(params[[parin]])
  }
  
  params = as.data.frame(params, stringsAsFactors = FALSE)
  params = dfRowsToList(params, simpleTypeOrig)[[1]]
  
  params = removeMissingValues(params)
  
  ret = lapply(names(params), function(parname) {
        vals = origparset$pars[[parname]]$values
        type = origparset$pars[[parname]]$type
        par = params[[parname]]
        if (is.null(vals)) {
          # don't change anything
          return(par)
        }
        switch(type,
            logicalvector = unlist(vals[unlist(par)]),
            logical = vals[[par]],
            discretevector = lapply(par, function(x) vals[[x]]),
            discrete = vals[[par]])
      })
  names(ret) = names(params)
  ret
}

# adapt requirements to parameter simplification we are doing above.
mboRequirements = function(searchspace) {
  replacements = list()
  for (param in searchspace$pars) {
    type = param$type
    if (type %in% c("numeric", "integer") ||
        (type == "discrete" &&
          all(sapply(param$values, test_character, any.missing = FALSE)))) {
      # int, num and character nonvector were not affected
      next
    }
    replaceStr = "c(%s)"
    if (ParamHelpers:::isVector(param)) {
      if (!test_integer(param$len, len = 1, lower = 1, any.missing = FALSE)) {
        stopf("Parameter '%s' is a vector param with undefined length'",
            param$id)
      }
      replaceStr = sprintf(replaceStr,
          paste0(param$id, seq_len(param$len), collapse = ", "))
    } else {
      replaceStr = sprintf(replaceStr, param$id)
    }
    replaceQuote = asQuoted(replaceStr)
    if (isDiscrete(param)) {
      objectText = capture.output(dput(param$values))
      fullObject = try(asQuoted(collapse(objectText, sep = "\n")),
          silent = TRUE)
      if (is.error(fullObject)) {
        fullObject = substitute(stop(sprintf(
                    "Parameter %s cannot be used in requirements.", pname)),
            list(pname = param$id))
      }
      insert =  list(fullObject = fullObject, index = replaceQuote)
      template = switch(type,
          # we index into fullObject to get a list
          discretevector = quote(fullObject[index]),
          # unlike for 'discretevector', we want to get a vector out of this.
          logicalvector =  quote(unlist(fullObject[index])),
          # index is a single element, and we want to get a single element.
          quote(fullObject[[index]]))
      replaceQuote = do.call(substitute, list(template, insert))
    }
    replacements[[param$id]] = replaceQuote
  }
  for (param in names(searchspace$pars)) {
    req = searchspace$pars[[param]]$requires
    if (is.null(req)) {
      next
    }
    newreq = replaceRequires(req, replacements)
    searchspace$pars[[param]]$requires = deExpression(newreq)
  }
  searchspace
}

