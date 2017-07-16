
#' @title random backend configuration
#' 
#' @description
#' Create an \code{AutomlrBackendConfig} object that can be fed to
#' \code{\link{automlr}} to perform optimization with the "random" backend.
#' 
#' @param max.iters.per.round [\code{integer(1)}]\cr
#'   Number of iterations to perform between timeout checks. Do not set too
#'   small; you probably don't want to change this.
#' @param resampling [\code{ResampleDesc}]\cr
#'   resampling to evaluate model performance.
#' @export
makeBackendconfRandom = registerBackend("random",
    function(max.iters.per.round = 100, resampling = cv5) {
      assertCount(max.iters.per.round)
      assertClass(resampling, "ResampleDesc")
      argsToList()
    })

# error message to put in opt.path
timeout.string = "timeout"

# error message to put in opt.path
out.of.budget.string = "out of budget"

# 'random' has no prior, so do nothing here
amaddprior.amrandom = function(env, prior) {
  NULL
}

# 'random' has no prior, so do nothing here
amgetprior.amrandom = function(env) {
  NULL
}

# save all the relevant variables in env
amsetup.amrandom = function(env, opt, prior, learner, task, measure,
    verbosity) {
  env$learner = addClasses(learner, "amrandomWrapped")
  env$rdesc = opt$resampling
  env$task = task
  env$measure = measure
  env$opt.path = NULL
  env$max.iters.per.round = opt$max.iters.per.round
  invisible()
}

amresult.amrandom = function(env) {
  res = mlr:::makeTuneResultFromOptPath(env$learner,
      getSearchspace(env$learner), list(env$measure),
      makeTuneControlRandom(maxit = 100), env$opt.path)
  list(learner = env$learner,
      opt.point = removeMissingValues(res$x),
      opt.val = res$y,
      opt.path = res$opt.path,
      result = res)
}

# now this is where the fun happens
amoptimize.amrandom = function(env, stepbudget, verbosity, deadline) {
  # so the strategy is as following:
  # we build a model wrapper around the learner, which, before checking, checks
  # the budget. if that was exceeded, we change the mlr settings so that errors
  # exit the tuner. then we throw an error, which we catch here.

  learner = env$learner

  am.env = new.env(parent = emptyenv())
  am.env$cpus = parallelGetOptions()$settings$cpus
  am.env$cpus[is.na(am.env$cpus)] = 1

  # am.env is the environment that will be attached to the learner. During the
  # evaluation it will be used to check the remaining budget.
  am.env$starttime = Sys.time()
  am.env$usedbudget = c(walltime = 0, evals = 0)
  am.env$stepbudget = stepbudget
  am.env$outofbudget = FALSE
  am.env$hardTimeout = proc.time()[3] + deadline
  # unfortunately, when doing parallelMap parallelization, the evaluating
  # function will get a *copy* of the environment, so will not be able to
  # communicate back. The `untouched` flag is used to detect this. Whenever
  # `untouched` is TRUE, we know that the modifications done by trainLearner and
  # predictLearner were not propagated.
  am.env$untouched = TRUE
  learner$am.env = am.env
  
  # the output function for tuneParams depends on the verbosity option.
  if (verbosity.traceout(verbosity)) {
    if (verbosity.memtraceout(verbosity)) {
      # the default output gives memory trace info.
      log.fun = "memory"
    } else {
      log.fun = "default"
    }
  } else {
    log.fun = logFunQuiet
  }
  
  while (!checkoutofbudget(learner$am.env)) {
    # chop up "evals" budget into small bunches so we can stop if and when
    # time runs out
    iterations = env$max.iters.per.round
    if ("walltime" %in% names(stepbudget)) {
      iterations = min(iterations, stepbudget["walltime"])
    }
    if ("evals" %in% names(stepbudget)) {
      iterations = min(stepbudget["evals"] - learner$am$usedbudget["evals"],
          iterations)
    }

    ctrl = makeTuneControlRandom(maxit = iterations, log.fun = log.fun)

    tuneresult = tuneParams(learner, env$task, env$rdesc, list(env$measure),
        par.set = getSearchspace(learner), control = ctrl,
        show.info = verbosity.traceout(verbosity))
    # we want to ignore all the 'out of budget' evals
    errorsvect = getOptPathErrorMessages(tuneresult$opt.path)
    notOOB = (is.na(errorsvect)) | (errorsvect != out.of.budget.string)
    learner$am.env$usedbudget["evals"] %+=% sum(notOOB)
    subsetOptPath(tuneresult$opt.path, notOOB)
    
    parent.env(tuneresult$opt.path$env) = emptyenv()

    if (is.null(env$opt.path)) {
      env$opt.path = tuneresult$opt.path
    } else {
      appendOptPath(env$opt.path, tuneresult$opt.path)
    }
    learner$am.env$outofbudget = FALSE
  }

  learner$am.env$usedbudget
}

oobErr = addClasses(out.of.budget.string, c("outOfBudgetError", "error"))
timeoutErr = addClasses(timeout.string, c("timeoutError", "error"))

#' @export
trainLearner.amrandomWrapped = function(.learner, ...) {
  env = .learner$am.env
  hardTimeoutRemaining = env$hardTimeout - proc.time()[3]

  
  # We want to test whether the first run of a resampling was over budget.
  # We can sometimes but not always communicate between runs of the same
  # resampling. Therefore we always need to check which iteration we are *AND*
  # whether the first iteration was actually able to communicate with us.
  # 1) if we are the first iteration, check whether we are over budget. also
  #    set a flag in am.env (that we touched it) and a flag whether we were over
  #    budget.
  # 2) if we are not the first iteration and the over-budget flag is set, or
  #    [1] we-touched-it-flag not set and [2] we are over budget, return the
  #    error.
  if (isFirstResampleIter()) {
    env$untouched = FALSE
    checkBudget = TRUE
  } else {
    # env$untouched is only TRUE on the non-first iteration when communication
    # between invocation failed. 
    checkBudget = env$untouched
  }

  if (env$outofbudget || (checkBudget && checkoutofbudget(.learner$am.env))) {
    # stop("out of budget"), but since we are inside try() anyways we function's
    # skip some overhead by just *return*ing an error.
    return(oobErr)
  }
  # its kind of amazing that NextMethod works like this.
  rwt = runWithTimeout(NextMethod("trainLearner"), hardTimeoutRemaining, backend = "native")
  
  if (rwt$timeout) {
    return(timeoutErr)
  }
  rwt$result
}

# need to pacify checkPredictLearnerOutput, so create an object that won't be
# admonished by it while still counting as an error.
createDummyError = function(.learner, .model) {
  if (.learner$type == "classif") {
    lx = .model$task.desc$class.levels
    if (.learner$predict.type == "response") {
      dummy.result = factor(lx[1], levels=lx)
    } else {
      dummy.result = matrix(0, ncol = length(lx))
      colnames(dummy.result) = lx
    }
  } else{
    result = list()
    if (.learner$predict.type == "response") {
      result = list(cluster=0L, multilabel=FALSE)
    }
    dummy.result = coalesce(result[[.learner$type]], 0.0)
    if (.learner$type %in% c("cluster", "regr") &&
        .learner$predict.type == "response") {
      # in this case, the first member of the class needs to be
      # numeric / integer
      class(dummy.result) = c(class(dummy.result),
          c("timeoutError", "error"))
      return(dummy.result)
    } else {
      dummy.result = matrix(dummy.result, ncol=2)
    }
  }
  addClasses(dummy.result, c("timeoutError", "error"))
}

#' @export
predictLearner.amrandomWrapped = function(.learner, .model, ...) {
  # if we reach this point, the run started before anything went out of budget,
  # so we finish what we started. Exception: The hardTimeout must be respected.
  env = .learner$am.env
  hardTimeoutRemaining = env$hardTimeout - proc.time()[3]

  rwt = runWithTimeout(NextMethod("predictLearner"), hardTimeoutRemaining)
  if (rwt$timeout) {
    return(createDummyError(.learner, .model))
  }
  rwt$result
}

# ModelMultiplexer does not consider the possibility of trainLearner() to
# throw an error; therefore the created ModelMultiplexerModel object handles
# it poorly. What we do here is we remove the ModelMultiplexerModel class
# from the resulting object in case there is an error.
#
# If one of the models themselves throw an error, the ModelMultiplexer won't
# have the type "FailureModel".
# FIXME: this can go when ModelMultiplexer is fixed upstream (?)
#' @export
makeWrappedModel.amrandomWrapped = function(learner, learner.model, task.desc,
    subset, features, factor.levels, time) {
  res = NextMethod("makeWrappedModel")
  if ("FailureModel" %in% class(res)) {
    class(res) = c("FailureModel", "WrappedModel")
  }
  res
}

# since the as.character.error function doesn't do what we want.
#' @export
as.character.outOfBudgetError = function(x, ...) {
  out.of.budget.string
}

# since the as.character.error function doesn't do what we want.
#' @export
as.character.timeoutError = function(x, ...) {
  timeout.string
}

# update walltime and check whether the learner attached environment `env` is
# out of budget. 
checkoutofbudget = function(env) {
  if (!env$outofbudget) {
    env$usedbudget["walltime"] = as.numeric(difftime(Sys.time(), env$starttime,
            units = "secs"))
    env$outofbudget = stopcondition(env$stepbudget, env$usedbudget)
  }
  return(env$outofbudget)
}
