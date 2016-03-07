

# 'random' has no prior, so do nothing here
amaddprior.amirace = function(env, prior) {
  NULL
}

# 'random' has no prior, so do nothing here
amgetprior.amirace = function(env) {
  NULL
}

irace.nbIterations = 10
irace.newpopulation = 2

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
      confidence = 0.95,
      nbIterations = -1L,  # so that we enter the main loop even though remainingBudget is 0
      maxExperiments = 100000L,  # some seed matrix is generated in the beginning, so this limits the number of experiments possible with this object absolutely.
      nbExperimentsPerIteration = expPerIter,
      impute.val = generateRealisticImputeVal(measure, learner, task),
      n.instances = 100,
      show.irace.output = TRUE,
      log.fun = logFunQuiet)  # make our life easy for now
  
  ## we do the following to imitade mlr::tuneParams()
  env$opt.path = mlr:::makeOptPathDFFromMeasures(env$learner$searchspace, list(env$measure), include.extra = (env$ctrl$tune.threshold))
  
  ## the following generates the wrapper around irace::irace that checks our budget constraints and ensures continuation
  iraceFunction = irace::irace
  env$iraceOriginal = iraceFunction
  
  environment(iraceFunction) = new.env(parent = asNamespace("irace"))
  environment(iraceFunction)$recoverFromFile = iraceRecoverFromFileFix  # breaking more rules than a maths teacher with anger management problems
  
  numcpus = parallelGetOptions()$settings$cpus  # this is assuming we don't use the irace package's parallel functionality.
  numcpus[is.na(numcpus)] = 1
  # we use some dark magic to run irace with our custom budget
  iraceWrapper = function(tunerConfig, parameters, ...) {
    modeltime.zero = sum(getOptPathExecTimes(env$opt.path), na.rm = TRUE)
    if (exists("tunerResults", envir = env)) {  # this is the backendprivatedata env
      # if tunerResults is in the environment then we are continuing, so we load the optimization state into the recover file
      tunerResults = env$tunerResults
      tunerResults$tunerConfig$logFile = tunerConfig$logFile  # use the newly generated file
      tunerResults$tunerConfig$hookRun = tunerConfig$hookRun  # take this round's hookRun, not the one from file.
      tunerConfig = tunerResults$tunerConfig  # everything else stays the same from last round
      tunerResults$tunerConfig$hookRun = NULL  # don't carry the heavyweight to the savefile. see `iraceRecoverFromFileFix()`
      evals.zero = tunerResults$state$experimentsUsedSoFar
    } else {
      # this is the first round.
      assert(tunerConfig$maxExperiments == 100000)
      assert(tunerConfig$timeBudget == 0)
      assert(tunerConfig$timeEstimate == 0)
      assert(tunerConfig$nbIterations == -1)
      tunerConfig$recoveryFile = NULL

      iraceFunction(tunerConfig, parameters, ...)  # we start but immediately stop because maxExperiments == 0

      load(tunerConfig$logFile)  # .... and load the state file to play with it
      tunerResults$tunerConfig$hookRun = tunerConfig$hookRun  # since the hookRun's environment gets broken up by saving and restoring
      tunerConfig = tunerResults$tunerConfig   # the above assignment is only for us to keep the hookRun in our tunerConfig
      tunerResults$tunerConfig$hookRun = NULL  # this is a heavyweight object which would get saved every iteration, even though we also give it as argument

      tunerResults$state$nbIterations = irace.nbIterations # influences how discrete probability weights are scaled
      tunerResults$tunerConfig$nbIterations = 0
      tunerConfig$nbIterations = 0  # in theory this is loaded from the recovery file, but you never know
      tunerResults$state$timeUsedSoFar = 1 # arbitrary positive number
      tunerResults$state$timeBudget = 0.5  # arbitrary positive number smaller than timeUsedSoFar --> we abort after one loop
      evals.zero = 0
    }

    tunerConfig$recoveryFile = tunerConfig$logFile
    while (TRUE) {
      tunerResults$state$remainingBudget = 1  # arbitrary positive number
      save(tunerResults, file = tunerConfig$logFile)
      
      res = iraceFunction(tunerConfig, parameters, ...)
      
      load(tunerConfig$logFile)
      env$usedbudget['evals'] = tunerResults$state$experimentsUsedSoFar - evals.zero
      env$usedbudget['modeltime'] = sum(getOptPathExecTimes(env$opt.path), na.rm = TRUE) - modeltime.zero
      env$usedbudget['walltime'] = as.numeric(difftime(Sys.time(), env$starttime, units = "secs"))
      env$usedbudget['cputime'] = env$usedbudget['walltime'] * numcpus
      
      env$tunerResults = tunerResults
      
      if (stopcondition(env$stepbudget, env$usedbudget)) {
        return(res)  # since we are guaranteed to have finished one iteration, this is never empty *I hope*
      }
    }
  }

  env$iraceWrapper = iraceWrapper

  invisible()
}

amresult.amirace = function(env) {
  res = env$tuneresult
  list(opt.point = removeMissingValues(res$x),
      opt.val = res$y,
      opt.path = res$opt.path,
      result = res)
}

# now this is where the fun happens
amoptimize.amirace = function(env, stepbudget) {
  env$starttime = Sys.time()
  env$stepbudget = stepbudget
  env$usedbudget = c(walltime = 0, cputime = 0, modeltime = 0, evals = 0)
  # install the wrapper and make sure it gets removed as soon as we exit
  on.exit(assignInNamespace("irace", env$iraceOriginal, ns = "irace"))
  assignInNamespace("irace", env$iraceWrapper, ns = "irace")
  
  myTuneIrace = mlr:::tuneIrace
  environment(myTuneIrace) = new.env(parent = asNamespace("mlr"))
  environment(myTuneIrace)$convertParamSetToIrace = function(par.set) {  # I stopped caring long ago
    irace::readParameters(text = convertParamSetToIrace(iraceRequirements(par.set), as.chars = TRUE), digits = .Machine$integer.max)
  }
  
  env$tuneresult = myTuneIrace(env$learner, env$task, env$rdesc, list(env$measure), env$learner$searchspace, env$ctrl, env$opt.path, TRUE)
  env$opt.path = env$tuneresult$opt.path
  env$usedbudget
}

# irace treats everything that is not a number as a string, which breaks our requirements.
# here we repair these requirements by turning <logical> into (<logical> == TRUE) and
# <discrete> into discreteValuesList[<discrete>]. We also treat vectors.
iraceRequirements = function(searchspace) {
  replacements = list()
  for (param in searchspace$pars) {
    if (param$type %in% c("discrete", "discretevector")) {
      if (param$type == "discrete" && all(sapply(param$values, test_character, any.missing = FALSE))) {
        # irace handles character discrete vectors well, so go right through
        next
      }
      assertList(param$values, names = "named")
      fullObject = asQuoted(collapse(capture.output(dput(param$values)), sep = ""))
      if (param$type == "discrete") {
        replacements[[param$id]] = substitute(fullObject[[index]], list(fullObject = fullObject, index = asQuoted(param$id)))
      } else { # discretevector
        if (!test_numeric(param$len, len = 1, lower = 1, any.missing = FALSE)) {
          stopf("Parameter '%s' is a vector param with undefined length'", param$id)
        }
        paramvec = asQuoted(sprintf("c(%s)", paste0(param$id, seq_len(param$len), collapse = ", ")))
        replacements[[param$id]] = substitute(fullObject[index], list(fullObject = fullObject, index = paramvec))
      }
    } else {
      replacePattern = switch(param$type,
          numeric = NULL,
          numericvector = "c(%s)",
          integer = "as.integer(%s)",
          integervector = "as.integer(c(%s))",
          logical = "(%s == TRUE)",
          logicalvector = "(c(%s) == TRUE)",
                                   # the following code should not be reachable as of yet
          charactervector = "c(%s)", # since we don't generate character vectors
          if (param$type %nin% c("function", "untyped")) { # ditto, hopefully
            stopf("Unknown type '%s' of parameter '%s'.", param$type, param$id)
          })
      if (is.null(replacePattern)) {
        # nothing to do
        next
      }
      if (param$type %in% c("numericvector", "integervector", "logicalvector", "charactervector")) {
        if (!test_numeric(param$len, len = 1, lower = 1, any.missing = FALSE)) {
          stopf("Parameter '%s' is a vector param with undefined length'", param$id)
        }
        replaceStr = sprintf(replacePattern, paste0(param$id, seq_len(param$len), collapse = ", "))
      } else {
        replaceStr = sprintf(replacePattern, param$id)
      }
      replacements[[param$id]] = asQuoted(replaceStr)
    }
  }
  for (param in names(searchspace$pars)) {
    req = searchspace$pars[[param]]$requires
    if (is.null(req)) {
      next
    }
    searchspace$pars[[param]]$requires = deExpression(replaceRequires(req, replacements))
  }
  searchspace
}


# this works since irace 1.05
iraceRecoverFromFileFix = function(file)  {
  # we don't want to overwrite our tunerConfig$hookRun with the saved one.
  eval.parent(substitute({
        load(file)
        for (name in names(tunerResults$state)) {
          pos = if (name == ".Random.seed") .GlobalEnv else -1
          assign(name, tunerResults$state[[name]], pos)
        }
        hookRunTemp = tunerConfig$hookRun
        for (name in c("parameters", "allCandidates", "tunerConfig")) {
          assign(name, tunerResults[[name]])
        }
        tunerConfig$hookRun = hookRunTemp
        options(.race.debug.level = tunerConfig$debugLevel)
        options(.irace.debug.level = tunerConfig$debugLevel)
      }))
}
