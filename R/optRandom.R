
#' @title random backend configuration
#' 
#' @description
#' Create an \code{AutomlrBackendConfig} object that can be fed to
#' \code{\link{automlr}} to perform optimization with the "random" backend.
#' 
#' @param iters.per.round [\code{integer(1)}]\cr
#'   Number of iterations to perform between timeout checks. Do not set too
#'   small; you probably don't want to change this.
#' @param resampling [\code{ResampleDesc}]\cr
#'   resampling to evaluate model performance.
#' @export
makeBackendconfRandom = registerBackend("random",
    function(iters.per.round = 100, resampling = cv5) {
      assertCount(iters.per.round)
      assertClass(resampling, "ResampleDesc")
      argsToList()
    })

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
  env$iters.per.round = opt$iters.per.round
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

  learner = adjustLearnerVerbosity(env$learner, verbosity)

  am.env = new.env(parent = emptyenv())
  am.env$cpus = parallelGetOptions()$settings$cpus
  am.env$cpus[is.na(am.env$cpus)] = 1

  # am.env is the environment that will be attached to the learner. During the
  # evaluation it will be used to check the remaining budget.
  am.env$starttime = Sys.time()
  am.env$usedbudget = c(walltime = 0, cputime = 0, modeltime = 0, evals = 0)
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
  
  # we count the modeltime that mlr gives us
  mlrModeltime = 0
  
  numcpus = parallelGetOptions()$settings$cpus
  
  # the output function for tuneParams depends on the verbosity option.
  if (verbosity.traceout(verbosity)) {
    if (verbosity.memtraceout(verbosity)) {
      # the default output gives memory trace info.
      log.fun = NULL
    } else {
      log.fun = logFunTune
    }
  } else {
    log.fun = logFunQuiet
  }
  
  while (!checkoutofbudget(learner$am.env)) {
    # chop up "evals" budget into 100s so we can stop when time runs out
    iterations = env$iters.per.round
    if ("evals" %in% names(stepbudget)) {
      iterations = min(stepbudget["evals"] - learner$am$usedbudget["evals"],
          iterations)
    }

    ctrl = makeTuneControlRandom(maxit = iterations, log.fun = log.fun)

    tuneresult = tuneParams(learner, env$task, env$rdesc, list(env$measure),
        par.set = getSearchspace(learner), control = ctrl, show.info = FALSE)
    # we want to ignore all the 'out of budget' evals
    errorsvect = getOptPathErrorMessages(tuneresult$opt.path)
    notOOB = (is.na(errorsvect)) | (errorsvect != out.of.budget.string)
    learner$am.env$usedbudget["evals"] %+=% sum(notOOB)
    mlrModeltime %+=% sum(getOptPathExecTimes(tuneresult$opt.path)[notOOB],
        na.rm = TRUE)
    if (is.null(env$opt.path)) {
      env$opt.path = tuneresult$opt.path
    } else {
      appendOptPath(env$opt.path, tuneresult$opt.path)
    }
    learner$am.env$outofbudget = FALSE
  }
  # the following updates walltime and cputime.
  # It also sets modeltime to cputime if am.env$untouched is TRUE (i.e. if
  # we know learner$am.env was not communicated back to us due to
  # parallelization).
  # The obvious objection would be that we know the modeltime from the
  # opt.path. however, that is not the modeltime that is used inside the
  # amrandomWrapped-models, so we shouldn't use it here, otherwise we could
  # get infinite loops!
  if (learner$am.env$untouched) {
    if ("modeltime" %in% names(stepbudget)) {
      mlrModeltime = max(mlrModeltime, stepbudget["modeltime"])
    }
    learner$am.env$usedbudget["modeltime"] = mlrModeltime
    if (!stopcondition(stepbudget, learner$am.env$usedbudget)) {
      # if "modeltime" is the decisive constraint, we need to lie a little here,
      # otherwise amoptimize() gets called again and we are in an endless loop.
      learner$am.env$usedbudget["modeltime"] = stepbudget["modeltime"]
    }
  }
  learner$am.env$usedbudget
}

timeoutErr = addClasses(out.of.budget.string, c("outOfBudgetError", "error"))

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
    # stop("out of budget"), only we don't use the try() function's way of
    # telling us what we already know
    return(timeoutErr)
  }
  rwt = runWithTimeout(res <- NextMethod("trainLearner"), hardTimeoutRemaining)
  if (!rwt) {
    return(timeoutErr)
  }
  evaltime = attr(rwt, "elapsed")
  .learner$am.env$usedbudget["modeltime"] %+=% evaltime[3]
  res
}

#' @export
predictLearner.amrandomWrapped = function(.learner, ...) {
  env = .learner$am.env
  hardTimeoutRemaining = env$hardTimeout - proc.time()[3]

  rwt = runWithTimeout(res <- NextMethod("predictLearner"),
      hardTimeoutRemaining)
  if (!rwt) {
    return(timeoutErr)
  }
  evaltime = attr(rwt, "elapsed")

  .learner$am.env$usedbudget["modeltime"] %+=% evaltime[3]
  res
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
  res = NextMethod()
  if ("FailureModel" %in% class(res)) {
    class(res) = c("FailureModel", "WrappedModel")
  }
  res
}

# since the as.character.error function doesn't do what we want.
#' @export
as.character.outOfBudgetError = function(x, ...) {
  x
}

# check whether the learner attached environment `env` is out of budget. This
# respects the possibility that `env` may or may not be able to communicate
# between runs and falls back to more or less good proxy values.
# @param evaltime the time of the current run that has not yet been added to
#   env$usedbudget["modeltime"] but should be respected when evaluating budget.
checkoutofbudget = function(env, evaltime = 0) {
  if (env$outofbudget) {
    return(env$outofbudget)
  }
  env$usedbudget["walltime"] = as.numeric(difftime(Sys.time(), env$starttime,
          units = "secs"))
  
  env$usedbudget["cputime"] = env$usedbudget["walltime"] * env$cpus
  modeltime = env$usedbudget["modeltime"] + evaltime
  
  # when doing parallel stuff, this is unknowable.
  if (env$untouched) {
    # either this is the first execution of predictLearner, or the whole thing
    # is being parallelized and the state of env is forgotten. We make the
    # assumption that the modeltime budget is not smaller than the time required
    # from initialization until here.
    env$usedbudget["modeltime"] = env$usedbudget["cputime"]
  } else {
    env$usedbudget["modeltime"] = modeltime
  }
  if (stopcondition(env$stepbudget, env$usedbudget)) {
    env$outofbudget = TRUE
  }
  
  env$usedbudget["modeltime"] = modeltime
  env$outofbudget
  
}
