

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
  learner$searchspace = iraceRequirements(learner$searchspace)
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
  
  ## we do the following to imitade mlr::tuneParams()
  env$opt.path = mlr:::makeOptPathDFFromMeasures(env$learner$searchspace, list(env$measure), include.extra = (env$ctrl$tune.threshold))
  
  ## the following generates the wrapper around irace::irace that checks our budget constraints and ensures continuation
  iraceFunction = irace::irace
  env$iraceOriginal = iraceFunction
  numcpus = parallelGetOptions()$settings$cpus  # this is assuming we don't use the irace package's parallel functionality.
  numcpus[is.na(numcpus)] = 1
  # we use some dark magic to run irace with our custom budget
  iraceWrapper = function(tunerConfig, parameters, ...) {
    modeltime.zero = sum(getOptPathExecTimes(env$opt.path), na.rm=TRUE)
    if (exists("tunerResults", envir=env)) {  # this is the backendprivatedata env
      # if tunerResults is in the environment then we are continuing, so we load the optimization state into the recover file
      tunerResults = env$tunerResults
      tunerConfig = tunerResult$tunerConfig
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
      tunerResults$state$nbIterations = irace.nbIterations # influences how discrete probability weights are scaled
      tunerResults$tunerConfig$nbIterations = 0  # this needs to be 0 or irace thinks this is a stopping criterion.
      tunerConfig$nbIterations = 0  # in theory this is loaded from the recovery file, but you never know
      tunerResults$state$timeUsedSoFar = 1 # arbitrary positive number
      tunerResults$state$timeBudget = 0.5  # arbitrary positive number smaller than timeUsedSoFar --> we abort after one loop
      evals.zero = 0
    }
    while (TRUE) {

      
      
      tunerResults$state$remainingBudget = 1  # arbitrary positive number
      save(tunerResults, file=tunerConfig$logFile)
      tunerConfig$recoveryFile = tunerConfig$logFile
      
      res = iraceFunction(tunerConfig, parameters, ...)
      
      load(tunerConfig$logFile)
      env$usedbudget['evals'] = tunerResults$state$experimentsUsedSoFar - evals.zero
      env$usedbudget['modeltime'] = sum(getOptPathExecTimes(env$opt.path), na.rm=TRUE) - modeltime.zero
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

# irace treats everything that is not a number as a string, which breaks our requirements.
# here we repair these requirements by turning <logical> into (<logical> == TRUE) and
# <discrete> into discreteValuesList[<discrete>]. We also treat vectors.
iraceRequirements = function(searchspace) {
  replacements = list()
  for (param in searchspace$pars) {
    if (param$type %in% c("discrete", "discretevector")) {
      if (param$type == "discrete" && all(sapply(param$values, test_character, any.missing=FALSE))) {
        # irace handles character discrete vectors well, so go right through
        next
      }
      assertList(param$values, names="named")
      fullObject = asQuoted(collapse(capture.output(dput(param$values)), sep=""))
      if (param$type == "discrete") {
        replacements[[param$id]] = substitute(fullObject[[index]], list(fullObject=fullObject, index=param$id))
      } else { # discretevector
        if (!test_numeric(param$len, len=1, lower=1, any.missing=FALSE)) {
          stopf("Parameter '%s' is a vector param with undefined length'", param$id)
        }
        paramvec = asQuoted(sprintf("c(%s)", paste0(param$id, seq_len(param$len), collapse=", ")))
        replacements[[param$id]] = substitute(fullObject[index], list(fullObject=fullObject, index=paramvec))
      }
    } else {
      replacePattern = switch(param$type,
          numeric=NULL,
          numericvector="c(%s)",
          integer="as.integer(%s)",
          integervector="as.integer(c(%s))",
          logical="(%s == TRUE)",
          logicalvector="(c(%s) == TRUE)",
                                   # the following code should not be reachable as of yet
          charactervector="c(%s)", # since we don't generate character vectors
          if (param$type %nin% c("function", "untyped")) { # ditto, hopefully
            stopf("Unknown type '%s' of parameter '%s'.", param$type, param$id)
          })
      if (is.null(replacePattern)) {
        # nothing to do
        next
      }
      if (param$type %in% c("numericvector", "integervector", "logicalvector", "charactervector")) {
        if (!test_numeric(param$len, len=1, lower=1, any.missing=FALSE)) {
          stopf("Parameter '%s' is a vector param with undefined length'", param$id)
        }
        replaceStr = sprintf(replacePattern, paste0(param$id, seq_len(param$len), collapse=", "))
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
