

# 'random' has no prior, so do nothing here
amaddprior.amirace = function(env, prior) {
  NULL
}

# 'random' has no prior, so do nothing here
amgetprior.amirace = function(env) {
  NULL
}


amsetup.amirace = function(env, prior, learner, task, measure) {
  requirePackages("irace", why = "optIrace", default.method = "load")
  env$learner = learner
  env$task = task
  env$measure = measure
  env$rdesc = do.call(makeResampleDesc, resampleOptions)
  
  env$ctrl = makeTuneControlIrace(maxExperiments = 1)  # maxExperiments will be overridden by our wrapper
  ## we do the following to imitade mlr::tuneParams()
  env$ctrl = mlr:::setDefaultImputeVal(env$ctrl, list(measure))
  env$opt.path = mlr:::makeOptPathDFFromMeasures(env$learner$searchspace, list(env$measure), include.extra = (env$ctrl$tune.threshold))
  ## end of imitation
  
  ## the following generates the wrapper around irace::irace that checks our budget constraints and ensures continuation
  iraceFunction = irace::irace
  numcpus = parallelGetOptions()$settings$cpus  # this is assuming we don't use the irace package's parallel functionality.
  numcpus[is.na(numcpus)] = 1
  # we use some dark magic to run irace with our custom budget
  iraceWrapper = function(tunerConfig, parameters, ...) {
    evals.zero = 0
    modeltime.zero = 0
    if (exists("tunerResults", envir=env)) {  # this is the 'env' of amsetup.amirace fame
      # if tunerResults is in the environment then we are continuing, so we load the optimization state into the recover file
      tunerResults = env$tunerResults
      save(tunerResults, tunerConfig$logFile)
      tunerConfig$recoveryFile = tunerConfig$logFile
      evals.zero = tunerResults$experimentsUsedSoFar
      modeltime.zero = tunerResults$timeUsedSoFar
    } else {
      tunerConfig$timeBudget = 1e-20
      tunerConfig$timeEstimate = 1e-24  # end after one round
    }
    while (TRUE) {
      res = iraceFunction(tunerConfig, parameters, ...)
      load(tunerConfig$logFile)
      env$usedbudget['evals'] = tunerResults$experimentsUsedSoFar - evals.zero
      env$usedbudget['modeltime'] = tunerResults$timeUsedSoFar - modeltime.zero
      env$usedbudget['walltime'] = as.numeric(difftime(Sys.time(), env$starttime, units = "secs"))
      env$usedbudget['cputime'] = env$usedbudget['walltime'] * numcpus
      if (stopcondition(env$stepbudget, env$usedbudget)) {
        return(res)  # TODO tuneIrace is too smart for its own good; make sure even with an empty result, something gets returned that doesn't trigger the `stop()`.
      }
      if (tunerResults$timeUsedSoFar <= 0) {
        # make extra sure we are not in an endless loop in case no time gets reported.
        tunerResults$timeUsedSoFar = 1e-10
      }
      tunerResults$remainingBudget = .Machine$integer.max  # in the first round, act like there is unlimited budget
      tunerResults$timeBudget = tunerResults$timeUsedSoFar / 2  # end after one round, since time budget check is at the /end/ of the loop :)
      save(tunerResults, tunerConfig$logFile)
    }
  }
  env$iraceWrapper = iraceWrapper
  env$iraceOriginal = iraceFunction
  invisible()
}

amresult.amirace = function(env) {
  res = env$tuneresult
  list(opt.point=removeMissingValues(res$x),
      opt.val=res$y,
      opt.path=res$opt.path,
      result=res)
}

# now this is where the fun happens
amoptimize.amirace = function(env, stepbudget) {
  env$starttime = Sys.time()
  env$stepbudget = stepbudget
  env$usedbudget = c(walltime=0, cputime=0, modeltime=0, evals=0)
  # install the wrapper and make sure it gets removed as soon as we exit
  on.exit(assignInNamespace("irace", env$iraceOriginal, ns="irace"))
  assignInNamespace("irace", env$iraceWrapper, ns="irace")

  env$tuneresult = mlr:::tuneIrace(env$learner, env$task, env$rdesc, list(env$measure), env$learner$searchspace, env$ctrl, env$opt.path, TRUE)
  env$opt.path = env$tuneresult$opt.path
  env$usedbudget
}