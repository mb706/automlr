
#' @title irace backend configuration
#' 
#' @description
#' Create an \code{AutomlrBackendConfig} object that can be fed to
#' \code{\link{automlr}} to perform optimization with the "irace" backend.
#' 
#' @param nbIterations [\code{integer(1)}]\cr
#'   Thinning of sampling distribution happens as if irace expected to run for
#'   \code{nbIterations} generations.
#' @param newpopulation [\code{integer(1)}]\cr
#'   Size of the population, \emph{additinal to} the \code{2 + log2(dimParams)}
#'   elite size.
#' @param resampling [\code{ResampleDesc}]\cr
#'   resampling to evaluate model performance.
#' @export
makeBackendconfIrace = registerBackend("irace",
    function(nbIterations = 10, newpopulation = 10,
        resampling = hout) {
      assertCount(nbIterations)
      assertCount(newpopulation)
      assertClass(resampling, "ResampleDesc")
      argsToList()
    })

amaddprior.amirace = function(env, prior) {
  NULL
}

amgetprior.amirace = function(env) {
  NULL
}

noResTimeout = "Premature timeout: No result in irace."

amsetup.amirace = function(env, opt, prior, learner, task, measure, verbosity) {
  requirePackages("irace", why = "optIrace", default.method = "load")
  #try(attachNamespace("irace"), silent = TRUE)
  env$learner = learner
  env$task = task
  env$measure = measure
  env$rdesc = makeResampleDesc("Holdout")
  
  dimParams = getParamNr(getSearchspace(learner), TRUE)
  
  # the 'default', but I'm not taking chances
  minNbSurvival = as.integer(2 + log2(dimParams))
  mu = 5L
  firstTest = 5L
  expPerIter = as.integer((opt$newpopulation + minNbSurvival + 1) *
          (max(firstTest, mu) + 5))

  env$ctrl = makeTuneControlIrace(
      maxTime = 0,
      minNbSurvival = minNbSurvival,
      mu = mu,
      nbConfigurations = 0L,
      softRestart = TRUE,
      firstTest = firstTest,
      eachTest = 1L,
      testType = "friedman",
      confidence = 0.95,
      # We need to enter the main loop even though remainingBudget is 0:
      nbIterations = -1L,
      # some seed matrix is generated in the beginning, so maxExperiments limits
      # the number of experiments possible with this object absolutely.
      maxExperiments = 100000L,
      nbExperimentsPerIteration = expPerIter,
      impute.val = generateRealisticImputeVal(measure, learner, task),
      n.instances = 100,
      show.irace.output = TRUE,
      log.fun = logFunQuiet)  # make our life easy for now
  
  # we do the following to imitade mlr::tuneParams()
  env$opt.path = mlr:::makeOptPathDFFromMeasures(getSearchspace(env$learner),
      list(env$measure), include.extra = env$ctrl$tune.threshold)

  # the following generates the wrapper around irace::irace that checks our
  # budget constraints and ensures continuation
  iraceFunction = irace::irace
  env$iraceOriginal = iraceFunction

  # breaking more rules than a maths teacher with anger management problems:
#  environment(iraceFunction) = new.env(parent = asNamespace("irace"))
#  environment(iraceFunction)$recoverFromFile = iraceRecoverFromFileFix
  
  # hard time limit, to be enforced even when progress may be lost. This will be
  # set by amoptimize.amirace
  env$hardTimeout = 0
  
  # we use some dark magic to run irace with our custom budget
  iraceWrapper = function(scenario, parameters, ...) {
    print('hier')
    if (exists("iraceResults", envir = env)) {
      # 'env' is the backendprivatedata env.
      # if iraceResults is in the environment then we are continuing, so we load
      # the optimization state into the recover file
      iraceResults = env$iraceResults
      # use the newly generated file
      iraceResults$scenario$logFile = scenario$logFile
      # take this round's hookRun, not the one from file.
      iraceResults$scenario$targetRunnerParallel =
          scenario$targetRunnerParallel
      # everything else stays the same from last round
      scenario = iraceResults$scenario
      # don't carry the heavyweight hookRun function's environment to the
      # savefile. cf iraceRecoverFromFileFix()
      iraceResults$scenario$targetRunnerParallel = NULL
      evals.zero = iraceResults$state$experimentsUsedSoFar
    } else {
      # this is the first round.
      assert(scenario$maxExperiments == 100000)
      assert(scenario$maxTime == 0)
      assert(scenario$nbIterations == -1)
      scenario$recoveryFile = NULL

      # we start but immediately stop because maxExperiments == 0
      iraceFunction(scenario, parameters, ...)

      # .... and load the state file to play with it
      load(scenario$logFile)
      # since the hookRun's environment gets broken up by saving and restoring
      iraceResults$scenario$targetRunnerParallel =
          scenario$targetRunnerParallel
      # the above assignment is for us to keep the hookRun in our scenario
      scenario = iraceResults$scenario
      # this is a heavyweight object which would get saved every iteration, even
      # though we also give it as argument
      iraceResults$scenario$targetRunnerParallel = NULL

      # influences how discrete probability weights are scaled
      iraceResults$state$nbIterations = opt$nbIterations
      iraceResults$scenario$nbIterations = 0
      # in theory this is loaded from the recovery file, but you never know
      scenario$nbIterations = 0
      # timeUsedSoFar: arbitrary positive number
      iraceResults$state$timeUsed = 1
      # timeBudget: arbitrary positive number smaller than timeUsedSoFar
      # --> we abort after one loop
      iraceResults$state$maxTime = 0.5
      evals.zero = 0
    }

    scenario$recoveryFile = scenario$logFile
    if ("logFile" %nin% irace:::.irace.params.recover) {
      myAssignInNamespace(".irace.params.recover",
          c(irace:::.irace.params.recover, "logFile"), "irace")
    }
    while (TRUE) {
      scenario$logFile = paste0(scenario$logFile, ".unused")
      
      iraceResults$state$remainingBudget = 1  # arbitrary positive number
      save(iraceResults, file = scenario$recoveryFile)
      
      # perform iraceFunction as a kind of transaction: If it is aborted due to
      # timeout, we return the old env$res and reinstate the old env$opt.path.
      optPathBackup = deepcopy(env$opt.path)
      res = runWithTimeout(iraceFunction(scenario, parameters, ...),
          env$hardTimeout - proc.time()[3])
      if (res$timeout) {
        env$opt.path = optPathBackup
        if (!exists("res", envir = env)) {
          stop(noResTimeout)
        }
        return(env$res)
      }
      env$res = res$result
      load(scenario$recoveryFile)
      env$usedbudget["evals"] =
          iraceResults$state$experimentsUsedSoFar - evals.zero
      env$usedbudget["walltime"] =
          as.numeric(difftime(Sys.time(), env$starttime, units = "secs"))
      
      env$iraceResults = iraceResults
      
      if (stopcondition(env$stepbudget, env$usedbudget)) {
        # since we are guaranteed to have finished one iteration, this is never
        # empty *I hope*
        return(env$res)
      }
    }
  }

  env$iraceWrapper = iraceWrapper

  invisible()
}

amresult.amirace = function(env) {
  res = env$tuneresult
  list(learner = env$learner,
      opt.point = removeMissingValues(res$x),
      opt.val = res$y,
      opt.path = res$opt.path,
      result = res)
}

# now this is where the fun happens
amoptimize.amirace = function(env, stepbudget, verbosity, deadline) {
  env$starttime = Sys.time()
  env$stepbudget = stepbudget
  env$usedbudget = c(walltime = 0, evals = 0)
  env$hardTimeout = deadline + proc.time()[3]
  # install the wrapper and make sure it gets removed as soon as we exit
  # patch mlr on CRAN
  originalTuneIrace = mlr:::tuneIrace
  
  on.exit(quickSuspendInterrupts({
            myAssignInNamespace("irace", env$iraceOriginal, "irace")
#            myAssignInNamespace("tuneIrace", originalTuneIrace, "mlr")
          }))

  myAssignInNamespace("irace", env$iraceWrapper, ns = "irace")
#  myAssignInNamespace("tuneIrace", workingTuneIrace, ns = "mlr")

  myTuneIrace = mlr:::tuneIrace
  environment(myTuneIrace) = new.env(parent = asNamespace("mlr"))
  environment(myTuneIrace)$convertParamSetToIrace = function(par.set) {
    irace::readParameters(
        text = convertParamSetToIrace(iraceRequirements(par.set),
            as.chars = TRUE),
        digits = .Machine$integer.max)
  }
  
  tryCatch({
      env$tuneresult = myTuneIrace(env$learner, env$task, env$rdesc,
          list(env$measure), getSearchspace(env$learner), env$ctrl,
          env$opt.path, TRUE)
    }, error = function(e) {
      if (conditionMessage(e) == noResTimeout) {
        # NOOP
      } else {
        stop(e)
      }
    })
  # FIXME: why was this here? It interferes with my 'rollback' mechanism in
  # iraceWrapper
  # env$opt.path = env$tuneresult$opt.path
  env$usedbudget
}

