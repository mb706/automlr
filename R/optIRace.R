

# 'random' has no prior, so do nothing here
amaddprior.amirace = function(env, prior) {
  NULL
}

# 'random' has no prior, so do nothing here
amgetprior.amirace = function(env) {
  NULL
}

irace.nbIterations = 10
irace.newpopulation = 30

amsetup.amirace = function(env, prior, learner, task, measure) {
  requirePackages("irace", why = "optIrace", default.method = "load")
  env$learner = learner
  env$task = task
  env$measure = measure
  env$rdesc = makeResampleDesc("Holdout")
  
  dimParams = getParamNr(learner$searchspace, TRUE)
  
  minNbSurvival = as.integer(2 + log2(dimParams))  # the 'default', but I'm not taking chances
  mu = 5L
  firstTest = 5L
  expPerIter = as.integer((irace.newpopulation + minNbSurvival + 1) * (max(firstTest, mu) + 5))
  
  
  env$ctrl = makeTuneControlIrace(
      timeBudget = 0,
      timeEstimate = 0,
      minNbSurvival = minNbSurvival,
      mu = mu,
      nbCandidates = 0L,
      softRestart = TRUE,
      firstTest = firstTest,
      eachTest = 1L,
      testType = "friedman",
      confidence=0.95,
      nbIterations = -1L,  # so that we enter the main loop even though remainingBudget is 0
      maxExperiments = 100000L,  # so that remainingBudget is 0 in the beginning
      nbExperimentsPerIteration = expPerIter,
      impute.val = generateRealisticImputeVal(measure, learner, task),
      n.instances = 100,
      show.irace.output = TRUE,
      log.fun=logFunQuiet)  # make our life easy for now
  
  # measure the model time and give it to irace, which will give it back to us.
  hookRunWrapper = function(originalHookRun) {
    force(originalHookRun)
    function(experiment, config = list()) {
      time = system.time(result <- originalHookRun(experiment, config), gcFirst=FALSE)[3]
      c(result, time)
    }
  }
  

  ## we do the following to imitade mlr::tuneParams()
  env$opt.path = mlr:::makeOptPathDFFromMeasures(env$learner$searchspace, list(env$measure), include.extra = (env$ctrl$tune.threshold))
  
  ## the following generates the wrapper around irace::irace that checks our budget constraints and ensures continuation
  iraceFunction = irace::irace
  env$iraceOriginal = iraceFunction
  numcpus = parallelGetOptions()$settings$cpus  # this is assuming we don't use the irace package's parallel functionality.
  numcpus[is.na(numcpus)] = 1
  # we use some dark magic to run irace with our custom budget
  iraceWrapper = function(tunerConfig, parameters, ...) {
    if (exists("tunerResults", envir=env)) {  # this is the backendprivatedata env
      # if tunerResults is in the environment then we are continuing, so we load the optimization state into the recover file
      tunerResults = env$tunerResults
      tunerConfig = tunerResult$tunerConfig
      evals.zero = tunerResults$state$experimentsUsedSoFar
      modeltime.zero = tunerResults$state$timeUsedSoFar
    } else {
      tunerConfig$hookRun = hookRunWrapper(tunerConfig$hookRun)  # measure the time
      # this is the first round.
      assert(tunerConfig$maxExperiments == 100000)
      assert(tunerConfig$timeBudget == 0)
      assert(tunerConfig$timeEstimate == 0)
      assert(tunerConfig$nbIterations == -1)
      tunerConfig$recoveryFile = NULL

      iraceFunction(tunerConfig, parameters, ...)  # we start but immediately stop because maxExperiments == 0

      load(tunerConfig$logFile)  # .... and load the state file to play with it
      tunerResults$state$nbIterations = irace.nbIterations # influences how discrete probability weights are scaled
      tunerResults$tunerConfig$nbIterations = 0  # this needs to be 0 or irace thinks this is a stopping criterion.
      tunerConfig$nbIterations = 0  # in theory this is loaded from the recovery file, but you never know
      tunerResults$state$timeUsedSoFar = 1 # arbitrary positive number
      tunerResults$state$timeBudget = 0.5  # arbitrary positive number smaller than timeUsedSoFar --> we abort after one loop

      evals.zero = 0
      modeltime.zero = 1  # because timeUsedSoFar was initialized as 1
    }
    while (TRUE) {

      
      
      tunerResults$state$remainingBudget = 1  # arbitrary positive number
      save(tunerResults, file=tunerConfig$logFile)
      tunerConfig$recoveryFile = tunerConfig$logFile
      
      res = iraceFunction(tunerConfig, parameters, ...)
      
      load(tunerConfig$logFile)
      env$usedbudget['evals'] = tunerResults$state$experimentsUsedSoFar - evals.zero
      env$usedbudget['modeltime'] = tunerResults$state$timeUsedSoFar - modeltime.zero
      env$usedbudget['walltime'] = as.numeric(difftime(Sys.time(), env$starttime, units = "secs"))
      env$usedbudget['cputime'] = env$usedbudget['walltime'] * numcpus
      
      env$tunerResults = tunerResults
      
      if (stopcondition(env$stepbudget, env$usedbudget)) {
        return(res)  # TODO since we are guaranteed to have finished one iteration, this is never empty *I hope*
      }
    }
  }

  env$iraceWrapper = iraceWrapper

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
