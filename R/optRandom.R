
out.of.budget.string = "out of budget"

# 'random' has no prior, so do nothing here
amcombinepriors.amrandom = function(prior, newprior) {
  NULL
}

# 'random' has no prior, so do nothing here
amgetprior.amrandom = function(env) {
  NULL
}

# no bells and whistles here either
amsetup.amrandom = function(env, prior, learner, task) {
  class(learner) = c("amrandomWrapped", class(learner))
  env$learner = learner
  env$rdesc = do.call(makeResampleDesc, resampleOptions)
  env$task = task
  env$opt.path = NULL
  invisible()
}

# TODO: return whatever the user might be interested in
amresult.amrandom = function(env) {
  res = mlr:::makeTuneResultFromOptPath(env$learner, env$learner$searchspace, list(getDefaultMeasure(env$task)),
      makeTuneControlRandom(maxit=100), env$opt.path)
  list(resultstring="Result was generated, find it as $res.", res=res)
}

# now this is where the fun happens
amoptimize.amrandom = function(env, stepbudget) {
  # so the strategy is as following:
  # we build a model wrapper around the learner, which, before checking, checks the budget. if that was exceeded,
  # we change the mlr settings so that errors exit the tuner. then we throw an error, which we catch here.
  oldOpts = getMlrOptions()  # save this, since predictLearner.amrand.wrapped may change it.
  on.exit(do.call(configureMlr, oldOpts))

  learner = env$learner
  learner$am.env = new.env(parent=emptyenv())

  learner$am.env$starttime = Sys.time()
  learner$am.env$usedbudget = c(walltime=0, cputime=0, modeltime=0, evals=0)
  learner$am.env$stepbudget = stepbudget
  learner$am.env$outofbudget = FALSE
  learner$am.env$untouched = TRUE

  mlrModeltime = 0  # we count the modeltime that mlr gives us
  
  numcpus = parallelGetOptions()$settings$cpus

  while (!checkoutofbudget(learner$am.env, numcpus, il=TRUE)) {
    iterations = min(ifelse('evals' %in% names(stepbudget), stepbudget['evals'] - learner$am.env$usedbudget['evals'], Inf), 100)
    ctrl = makeTuneControlRandom(maxit=iterations, log.fun=logFunTune)  # chop up 'evals' budget into 1000s
    tuneresult = tuneParams(learner, env$task, env$rdesc, par.set=learner$searchspace, control=ctrl)
    do.call(configureMlr, oldOpts)  # we call this here, in case we loop around. Whenever the error is not an 'out of budget' error we want the usual behaviour.
    # we want to ignore all the 'out of budget' evals
    errorsvect = getOptPathErrorMessages(tuneresult$opt.path)
    wasOOB = (!is.na(errorsvect)) & (errorsvect == out.of.budget.string)
    performediterations = sum(!wasOOB)
    learner$am.env$usedbudget['evals'] = learner$am.env$usedbudget['evals'] + performediterations
    mlrModeltime = mlrModeltime + sum(getOptPathExecTimes(tuneresult$opt.path)[!wasOOB])
    if (is.null(env$opt.path)) {
      env$opt.path = tuneresult$opt.path
    } else {
      # TODO: this fails catastrophically in case the search space changes.
      appendOptPath(env$opt.path, tuneresult$opt.path)
    }
    # the following updates walltime and cputime.
    # It also sets modeltime to cputime if am.env$untouched is TRUE.
    # The obvious objection would be that we know the modeltime from the opt.path. however, that is
    # not the modeltime that is used inside the amrandomWrapped-models, so we shouldn't use it here,
    # otherwise we could get infinite loops!
    learner$am.env$outofbudget = FALSE
  }
  if (learner$am.env$untouched) {
    if ('modeltime' %in% names(stepbudget)) {  
      max(mlrModeltime, stepbudget['modeltime'])
    }
    learner$am.env$usedbudget['modeltime'] = mlrModeltime
    if (!stopcondition(stepbudget, learner$am.env$usedbudget)) {
      # if 'modeltime' is the decisive constraint, we need to lie a little here, otherwise amoptimize() gets called again and we are in an endless loop.
      learner$am.env$usedbudget['modeltime'] = stepbudget['modeltime']
    }
  }
  learner$am.env$usedbudget
}

#' @export
trainLearner.amrandomWrapped = function(.learner, ...) {
  if (checkoutofbudget(.learner$am.env, parallelGetOptions()$settings$cpus)) {
    return(addClasses(out.of.budget.string, c("outOfBudgetError", "error")))  # stop("out of budget"), only we don't use the try() function's way of telling us what we already know
  }
  evaltime = system.time(result <- NextMethod("trainLearner"), gcFirst=FALSE)
  .learner$am.env$usedbudget['modeltime'] = .learner$am.env$usedbudget['modeltime'] + evaltime[3]
  result
}

#' @export
makeWrappedModel.amrandomWrapped = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  res = NextMethod()
  if ("FailureModel" %in% class(res)) {
    # ModelMultiplexer does not consider the possibility of trainLearner() to throw an error; therefore the
    # created ModelMultiplexerModel object handles it poorly. What we do here is we remove the ModelMultiplexerModel class
    # from the resulting object in case there is an error.
    #
    # If one of the models themselves throw an error, the ModelMultiplexer won't have the type "FailureModel".
    class(res) = c("FailureModel", "WrappedModel")
  }
  res
}

#' @export
as.character.outOfBudgetError = function(x, ...) {  # since the as.character.error function doesn't do what we want.
  x
}

#' @export
predictLearner.amrandomWrapped = function(.learner, ...) {
  numcpus = parallelGetOptions()$settings$cpus
  env = .learner$am.env
#  if (checkoutofbudget(env, numcpus)) {
#    configureMlr(on.learner.error = "quiet")
#    stop(out.of.budget.string)
#  }
  evaltime = system.time(result <- NextMethod("predictLearner"), gcFirst=FALSE)

  if (checkoutofbudget(env, numcpus, evaltime[3])) {
    configureMlr(on.learner.error = "quiet")
  }
  env$untouched = FALSE
  result
}

checkoutofbudget = function(env, numcpus, evaltime=0, il=FALSE) {
  if (env$outofbudget) {
    return(env$outofbudget)
  }
  env$usedbudget['walltime'] = as.numeric(difftime(Sys.time(), env$starttime, units = "secs"))
  #print(env$usedbudget['walltime'])
  
  if (is.na(numcpus)) {
    numcpus = 1
  }
  env$usedbudget['cputime'] = env$usedbudget['walltime'] * numcpus
  modeltime = env$usedbudget['modeltime'] + evaltime
  #print(modeltime)
  
  # when doing parallel stuff, this is unknowable.
  if (env$untouched) {
    # either this is the first execution of predictLearner, or the whole thing is being parallelized
    # and the state of env is forgotten.
    # we make the assumption that the modeltime budget is not smaller than the time required from
    # initialization until here.
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
