
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
  class(learner) = c("amrandom.wrapped", class(learner))
  env$learner = learner
  env$rdesc = do.call(makeResampleDesc, resampleOptions)
  env$task = task
  env$opt.path = NULL
  invisible()
}

# TODO: return whatever the user might be interested in
amresult.amrandom = function(env) {
  res = mlr:::makeTuneResultFromOptPath(env$learner, env$learner$searchspace, list(getDefaultMeasure(env$task)),
      makeTuneControlRandom(maxit=1000), env$opt.path)
  list(resultstring="Result was generated, find it as $res.", res=res)
}

# now this is where the fun happens
amoptimize.amrandom = function(env, stepbudget) {
  # so the strategy is as following:
  # we build a model wrapper around the learner, which, before checking, checks the budget. if that was exceeded,
  # we change the mlr settings so that errors exit the tuner. then we throw an error, which we catch here.
  oldOLE = getMlrOptions()$on.learner.error  # save this, since predictLearner.amrand.wrapped may change it.

  learner = env$learner
  learner$am.env = new.env(parent=emptyenv())

  learner$am.env$starttime = Sys.time()
  learner$am.env$usedbudget = c(walltime=0, cputime=0, modeltime=0, evals=0)
  learner$am.env$stepbudget = stepbudget
  learner$am.env$outofbudget = FALSE

  while (!stopcondition(stepbudget, learner$am.env$usedbudget)) {
    iterations = min(ifelse('evals' %in% names(stepbudget), stepbudget['evals'] - learner$am.env$usedbudget['evals'], Inf), 1000)
    ctrl = makeTuneControlRandom(maxit=iterations)  # chop up 'evals' budget into 1000s
    tuneresult = tuneParams(learner, env$task, env$rdesc, par.set=learner$searchspace, control=ctrl)
    learner$am.env$usedbudget['evals'] = learner$am.env$usedbudget['evals'] + iterations  # this is not very precise. Should count the errors in the OptPath.
    if (is.null(env$opt.path)) {
      env$opt.path = tuneresult$opt.path
    } else {
      appendOptPath(env$opt.path, tuneresult$opt.path)
    }
  }
  configureMlr(on.learner.error = oldOLE)  # TODO: this should be in some kind of 'finally' block
  learner$am.env$usedbudget
}

trainLearner.amrandom.wrapped = function(.learner, ...) {
  if (learner$am.env$outofbudget) {
    stop("out of budget")
  }
  evaltime = system.time(result <- NextMethod("trainLearner"))
  learner$am.env$usedbudget['modeltime'] = learner$am.env$usedbudget['modeltime'] + evaltime[3]
  result
}

predictLearner.amrandom.wrapped = function(.learner, ...) {
  evaltime = system.time(result <- NextMethod("predictLearner"))

  env = .learner$am.env

  env$usedbudget['walltime'] = as.numeric(difftime(env$starttime, Sys.time(), units = "secs"))

  numcpus = parallelGetOptions()$settings$cpus
  if (is.na(numcpus)) {
    numcpus = 1
  }
  env$usedbudget['cputime'] = env$usedbudget['walltime'] * numcpus  # TODO this is pretty hackish

  env$usedbudget['modeltime'] = env$usedbudget['modeltime'] + evaltime[3]

  if (stopcondition(env$stepbudget, env$usedbudget)) {
    configureMlr(on.learner.error = "quiet")
    env$outofbudget = TRUE
  }

  result
}