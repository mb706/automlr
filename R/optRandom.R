
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
amsetup.amrandom = function(env, prior, learner, task, measure) {
  env$learner = addClasses(learner, "amrandomWrapped")
  env$rdesc = do.call(makeResampleDesc, resampleOptions)
  env$task = task
  env$measure = measure
  env$opt.path = NULL
  invisible()
}

amresult.amrandom = function(env) {
  res = mlr:::makeTuneResultFromOptPath(env$learner, env$learner$searchspace,
      list(env$measure), makeTuneControlRandom(maxit = 100), env$opt.path)
  list(learner = env$learner,
      opt.point = removeMissingValues(res$x),
      opt.val = res$y,
      opt.path = res$opt.path,
      result = res)
}

# now this is where the fun happens
amoptimize.amrandom = function(env, stepbudget) {
  # so the strategy is as following:
  # we build a model wrapper around the learner, which, before checking, checks
  # the budget. if that was exceeded, we change the mlr settings so that errors
  # exit the tuner. then we throw an error, which we catch here.
  
  # Save old mlr options, since predictLearner.amrand.wrapped may change it.
  oldOpts = getMlrOptions()
  on.exit(do.call(configureMlr, oldOpts))
  
  learner = env$learner
  
  am.env = new.env(parent = emptyenv())
  am.env$cpus = parallelGetOptions()$settings$cpus
  if (is.na(am.env$cpus)) {
    am.env$cpus = 1
  }
  am.env$starttime = Sys.time()
  am.env$usedbudget = c(walltime = 0, cputime = 0, modeltime = 0, evals = 0)
  am.env$stepbudget = stepbudget
  am.env$outofbudget = FALSE
  am.env$untouched = TRUE
  learner$am.env = am.env
  
  mlrModeltime = 0  # we count the modeltime that mlr gives us
  
  numcpus = parallelGetOptions()$settings$cpus
  
  while (!checkoutofbudget(learner$am.env, numcpus, il = TRUE)) {
    # chop up 'evals' budget into 100s so we can stop when time runs out
    iterations = 100
    if ('evals' %in% names(stepbudget)) {
      iterations = min(stepbudget['evals'] - learner$am$usedbudget['evals'],
          iterations)
    }
    ctrl = makeTuneControlRandom(maxit = iterations, log.fun = logFunQuiet)
    
    tuneresult = tuneParams(learner, env$task, env$rdesc, list(env$measure),
        par.set = learner$searchspace, control = ctrl, show.info = FALSE)
    # we call configureMLR here, in case we loop around. Whenever the error is
    # not an 'out of budget' error we want the usual behaviour.
    do.call(configureMlr, oldOpts)  
    # we want to ignore all the 'out of budget' evals
    errorsvect = getOptPathErrorMessages(tuneresult$opt.path)
    notOOB = (is.na(errorsvect)) | (errorsvect != out.of.budget.string)
    learner$am.env$usedbudget['evals'] %+=% sum(notOOB)
    mlrModeltime %+=% sum(getOptPathExecTimes(tuneresult$opt.path)[notOOB],
        na.rm = TRUE)
    if (is.null(env$opt.path)) {
      env$opt.path = tuneresult$opt.path
    } else {
      appendOptPath(env$opt.path, tuneresult$opt.path)
    }
    # the following updates walltime and cputime.
    # It also sets modeltime to cputime if am.env$untouched is TRUE.
    # The obvious objection would be that we know the modeltime from the
    # opt.path. however, that is not the modeltime that is used inside the
    # amrandomWrapped-models, so we shouldn't use it here, otherwise we could
    # get infinite loops!
    learner$am.env$outofbudget = FALSE
  }
  if (learner$am.env$untouched) {
    if ('modeltime' %in% names(stepbudget)) {  
      mlrModeltime = max(mlrModeltime, stepbudget['modeltime'])
    }
    learner$am.env$usedbudget['modeltime'] = mlrModeltime
    if (!stopcondition(stepbudget, learner$am.env$usedbudget)) {
      # if 'modeltime' is the decisive constraint, we need to lie a little here,
      # otherwise amoptimize() gets called again and we are in an endless loop.
      learner$am.env$usedbudget['modeltime'] = stepbudget['modeltime']
    }
  }
  learner$am.env$usedbudget
}

#' @export
trainLearner.amrandomWrapped = function(.learner, ...) {
  if (checkoutofbudget(.learner$am.env)) {
    # stop("out of budget"), only we don't use the try() function's way of
    # telling us what we already know
    return(addClasses(out.of.budget.string, c("outOfBudgetError", "error")))
  }
  evaltime = system.time(result <- NextMethod("trainLearner"), gcFirst = FALSE)
  .learner$am.env$usedbudget['modeltime'] %+=% evaltime[3]
  result
}

#' @export
makeWrappedModel.amrandomWrapped = function(learner, learner.model, task.desc,
    subset, features, factor.levels, time) {
  res = NextMethod()
  if ("FailureModel" %in% class(res)) {
    # ModelMultiplexer does not consider the possibility of trainLearner() to
    # throw an error; therefore the created ModelMultiplexerModel object handles
    # it poorly. What we do here is we remove the ModelMultiplexerModel class
    # from the resulting object in case there is an error.
    #
    # If one of the models themselves throw an error, the ModelMultiplexer won't
    # have the type "FailureModel".
    # FIXME: this can go when ModelMultiplexer is fixed upstream (?)
    class(res) = c("FailureModel", "WrappedModel")
  }
  res
}

#' @export
as.character.outOfBudgetError = function(x, ...) {
  # since the as.character.error function doesn't do what we want.
  x
}

#' @export
predictLearner.amrandomWrapped = function(.learner, ...) {
  env = .learner$am.env
  # TODO: what is this?
#  if (checkoutofbudget(env, numcpus)) {
#    configureMlr(on.learner.error = "quiet")
#    stop(out.of.budget.string)
#  }
  evaltime = system.time(result <- NextMethod("predictLearner"),
      gcFirst = FALSE)
  
  if (checkoutofbudget(env, evaltime[3])) {
    # TODO: check this hack
    configureMlr(on.learner.error = "quiet")
  }
  env$untouched = FALSE
  result
}

checkoutofbudget = function(env, evaltime = 0, il = FALSE) {
  if (env$outofbudget) {
    return(env$outofbudget)
  }
  env$usedbudget['walltime'] = as.numeric(difftime(Sys.time(), env$starttime,
          units = "secs"))
  
  env$usedbudget['cputime'] = env$usedbudget['walltime'] * env$cpus
  modeltime = env$usedbudget['modeltime'] + evaltime
  
  # when doing parallel stuff, this is unknowable.
  if (env$untouched) {
    # either this is the first execution of predictLearner, or the whole thing
    # is being parallelized and the state of env is forgotten. We make the
    # assumption that the modeltime budget is not smaller than the time required
    # from initialization until here.
    env$usedbudget['modeltime'] = env$usedbudget['cputime']
  } else {
    env$usedbudget['modeltime'] = modeltime
  }
  if (stopcondition(env$stepbudget, env$usedbudget)) {
    env$outofbudget = TRUE
  }
  
  env$usedbudget['modeltime'] = modeltime
  env$outofbudget
  
}
