

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
      checkCount(nbIterations)
      checkCount(newpopulation)
      checkClass(resampling, "ResampleDesc")
      argsToList()
    })


amaddprior.amirace = function(env, prior) {
  NULL
}

amgetprior.amirace = function(env) {
  NULL
}

amsetup.amirace = function(env, opt, prior, learner, task, measure, verbosity) {
  requirePackages("irace", why = "optIrace", default.method = "load")
  env$learner = learner
  env$task = task
  env$measure = measure
  env$rdesc = makeResampleDesc("Holdout")
  
  dimParams = getParamNr(learner$searchspace, TRUE)
  
  # the 'default', but I'm not taking chances
  minNbSurvival = as.integer(2 + log2(dimParams))
  mu = 5L
  firstTest = 5L
  expPerIter = as.integer((irace.newpopulation + minNbSurvival + 1) *
          (max(firstTest, mu) + 5))

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
  env$opt.path = mlr:::makeOptPathDFFromMeasures(env$learner$searchspace,
      list(env$measure), include.extra = env$ctrl$tune.threshold)

  # the following generates the wrapper around irace::irace that checks our
  # budget constraints and ensures continuation
  iraceFunction = irace::irace
  env$iraceOriginal = iraceFunction

  # breaking more rules than a maths teacher with anger management problems:
  environment(iraceFunction) = new.env(parent = asNamespace("irace"))
  environment(iraceFunction)$recoverFromFile = iraceRecoverFromFileFix

  # this is assuming we don't use the irace package's parallel functionality.
  numcpus = parallelGetOptions()$settings$cpus
  numcpus[is.na(numcpus)] = 1
  # we use some dark magic to run irace with our custom budget
  iraceWrapper = function(tunerConfig, parameters, ...) {
    modeltime.zero = sum(getOptPathExecTimes(env$opt.path), na.rm = TRUE)
    if (exists("tunerResults", envir = env)) {
      # 'env' is the backendprivatedata env.
      # if tunerResults is in the environment then we are continuing, so we load
      # the optimization state into the recover file
      tunerResults = env$tunerResults
      # use the newly generated file
      tunerResults$tunerConfig$logFile = tunerConfig$logFile
      # take this round's hookRun, not the one from file.
      tunerResults$tunerConfig$hookRun = tunerConfig$hookRun
      # everything else stays the same from last round
      tunerConfig = tunerResults$tunerConfig
      # don't carry the heavyweight hookRun function's environment to the
      # savefile. cf iraceRecoverFromFileFix()
      tunerResults$tunerConfig$hookRun = NULL
      evals.zero = tunerResults$state$experimentsUsedSoFar
    } else {
      # this is the first round.
      assert(tunerConfig$maxExperiments == 100000)
      assert(tunerConfig$timeBudget == 0)
      assert(tunerConfig$timeEstimate == 0)
      assert(tunerConfig$nbIterations == -1)
      tunerConfig$recoveryFile = NULL

      # we start but immediately stop because maxExperiments == 0
      iraceFunction(tunerConfig, parameters, ...)

      # .... and load the state file to play with it
      load(tunerConfig$logFile)
      # since the hookRun's environment gets broken up by saving and restoring
      tunerResults$tunerConfig$hookRun = tunerConfig$hookRun
      # the above assignment is for us to keep the hookRun in our tunerConfig
      tunerConfig = tunerResults$tunerConfig
      # this is a heavyweight object which would get saved every iteration, even
      # though we also give it as argument
      tunerResults$tunerConfig$hookRun = NULL

      # influences how discrete probability weights are scaled
      tunerResults$state$nbIterations = irace.nbIterations
      tunerResults$tunerConfig$nbIterations = 0
      # in theory this is loaded from the recovery file, but you never know
      tunerConfig$nbIterations = 0
      # timeUsedSoFar: arbitrary positive number
      tunerResults$state$timeUsedSoFar = 1
      # timeBudget: arbitrary positive number smaller than timeUsedSoFar
      # --> we abort after one loop
      tunerResults$state$timeBudget = 0.5
      evals.zero = 0
    }

    tunerConfig$recoveryFile = tunerConfig$logFile
    while (TRUE) {
      tunerResults$state$remainingBudget = 1  # arbitrary positive number
      save(tunerResults, file = tunerConfig$logFile)
      
      res = iraceFunction(tunerConfig, parameters, ...)
      
      load(tunerConfig$logFile)
      env$usedbudget["evals"] =
          tunerResults$state$experimentsUsedSoFar - evals.zero
      env$usedbudget["modeltime"] =
          sum(getOptPathExecTimes(env$opt.path), na.rm = TRUE) - modeltime.zero
      env$usedbudget["walltime"] =
          as.numeric(difftime(Sys.time(), env$starttime, units = "secs"))
      env$usedbudget["cputime"] = env$usedbudget["walltime"] * numcpus
      
      env$tunerResults = tunerResults
      
      if (stopcondition(env$stepbudget, env$usedbudget)) {
        # since we are guaranteed to have finished one iteration, this is never
        # empty *I hope*
        return(res)
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
amoptimize.amirace = function(env, stepbudget, verbosity) {
  env$starttime = Sys.time()
  env$stepbudget = stepbudget
  env$usedbudget = c(walltime = 0, cputime = 0, modeltime = 0, evals = 0)
  # install the wrapper and make sure it gets removed as soon as we exit
  on.exit(assignInNamespace("irace", env$iraceOriginal, ns = "irace"),
      add = TRUE)
  assignInNamespace("irace", env$iraceWrapper, ns = "irace")
  
  # patch mlr on CRAN
  originalTuneIrace = mlr:::tuneIrace
  on.exit(assignInNamespace("tuneIrace", originalTuneIrace, ns = "mlr"),
      add = TRUE)
  assignInNamespace("tuneIrace", workingTuneIrace, ns = "mlr")
  
  
  myTuneIrace = mlr:::tuneIrace
  environment(myTuneIrace) = new.env(parent = asNamespace("mlr"))
  environment(myTuneIrace)$convertParamSetToIrace = function(par.set) {
    irace::readParameters(
        text = convertParamSetToIrace(iraceRequirements(par.set),
            as.chars = TRUE),
        digits = .Machine$integer.max)
  }
  
  env$learner = adjustLearnerVerbosity(env$learner, verbosity)
  
  env$tuneresult = myTuneIrace(env$learner, env$task, env$rdesc,
      list(env$measure), env$learner$searchspace, env$ctrl, env$opt.path, TRUE)
  env$opt.path = env$tuneresult$opt.path
  env$usedbudget
}

# irace treats everything that is not a number as a string, which breaks our
# requirements. Here we repair these requirements by turning <logical> into
# (<logical> == TRUE) and <discrete> into discreteValuesList[<discrete>].
# We also treat vectors.
iraceRequirements = function(searchspace) {
  replacements = list()
  for (param in searchspace$pars) {
    type = param$type
    if (type == "discrete" &&
        all(sapply(param$values, test_character, any.missing = FALSE))) {
      # irace handles character discrete vectors well, so go right through
      next
    }
    replaceStr = switch(type,
        numeric = "%s",
        numericvector = "c(%s)",
        integer = "as.integer(%s)",
        integervector = "as.integer(c(%s))",
        logical = "(%s == 'TRUE')",
        logicalvector = "(c(%s) == 'TRUE')",
        discrete = "%s",
        discretevector = "c(%s)",
        stopf("Unsupported type '%s' of parameter '%s'.", type, param$id))
    if (ParamHelpers:::isVector(param)) {
      if (!test_numeric(param$len, len = 1, lower = 1, any.missing = FALSE)) {
        stopf("Parameter '%s' is a vector param with undefined length'",
            param$id)
      }
      replaceStr = sprintf(replaceStr,
          paste0(param$id, seq_len(param$len), collapse = ", "))
    } else {
      replaceStr = sprintf(replaceStr, param$id)
    }
    replaceQuote = asQuoted(replaceStr)

    if (type %in% c("discrete", "discretevector")) {
      assertList(param$values, names = "named")
      objectText = capture.output(dput(param$values))
      fullObject = try(asQuoted(collapse(objectText, sep = "")), silent = TRUE)
      if (!is.error(fullObject)) {
        
        
      }
      if (is.error(fullObject) || length(all.vars(fullObject)) > 0) {
        # irace does not like '\n' in their requirements.
        # but it DOES accept 'eval(parse("\\n"))'
        # this also fixes the problem that parameters in lists of functions show
        # up on 'all.vars' even though they shouldn't, for irace's sake.
        objectText = collapse(objectText, sep = "\n")
        fullObject = asQuoted(paste0("eval(parse(text = ",
                capture.output(dput(objectText)), "))"))
      }
      if (type == "discrete") {
        replaceQuote = substitute(fullObject[[index]],
            list(fullObject = fullObject, index = replaceQuote))
      } else { # discretevector
        replaceQuote = substitute(fullObject[index],
            list(fullObject = fullObject, index = replaceQuote))
      }
    }
    replacements[[param$id]] = replaceQuote
  }
  for (param in names(searchspace$pars)) {
    req = searchspace$pars[[param]]$requires
    if (is.null(req)) {
      next
    }
    newreq = replaceRequires(req, replacements)
    searchspace$pars[[param]]$requires = deExpression(newreq)
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


# The following function is a slightly modified version of the
# tuneIrace function in the mlr project (https://github.com/mlr-org/mlr).
#
# This file was distributed with a BSD 2 clause license as follows.
#
# Copyright (c) 2013-2016, Bernd Bischl
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
# 
#     Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in
#     the documentation and/or other materials provided with the
#     distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
workingTuneIrace = function(learner, task, resampling, measures, par.set, control, opt.path, show.info) {
  requirePackages("irace", why = "tuneIrace", default.method = "load")
  hookRun = function(experiment, config = list()) {
    rin = experiment$instance
    plen = getParamNr(par.set, devectorize = TRUE)
    x = experiment$candidate
    # FIXME: this is a bug in irace, where for 1 param only a scalar is returned
    # BB reported this already, maybe we can remove this later
    if (plen == 1L)
      x = makeDataFrame(1L, 1L, init = x, col.names = getParamIds(par.set))
    
    # now convert to list, we also need to convert col types, irace sometimes uses not what we need
    # - logicals are stored as as strings
    x = dfRowToList(x, par.set, 1L, enforce.col.types = TRUE)
    
    tunerFitnFun(x, learner = learner, task = task, resampling = rin, measures = measures,
              par.set = par.set, ctrl = control, opt.path = opt.path, show.info = show.info,
              convertx = identity, remove.nas = TRUE)
  }
  
  n.instances = control$extra.args$n.instances
  control$extra.args$n.instances = NULL
  show.irace.output = control$extra.args$show.irace.output
  control$extra.args$show.irace.output = NULL
  instances = lapply(seq_len(n.instances), function(i) makeResampleInstance(resampling, task = task))
  if (is.null(control$extra.args$digits)) {
    control$extra.args$digits = .Machine$integer.max
  } else {
    control$extra.args$digits = asInt(control$extra.args$digits)
  }
  
  parameters = convertParamSetToIrace(par.set)
  log.file = tempfile()
  tuner.config = c(list(hookRun = hookRun, instances = instances, logFile = log.file),
          control$extra.args)
  g = if (show.irace.output) identity else utils::capture.output
  g(or <- irace::irace(tunerConfig = tuner.config, parameters = parameters))
  unlink(log.file)
  if (nrow(or) == 0L)
    stop("irace produced no result, possibly the budget was set too low?")
  # get best candidate
  x1 = as.list(irace::removeCandidatesMetaData(or[1L,]))
  # we need chars, not factors / logicals, so we can match 'x'
  d = convertDfCols(as.data.frame(opt.path), logicals.as.factor = TRUE)
  d = convertDfCols(d, factors.as.char = TRUE)
  par.names = names(x1)
  # get all lines in opt.path which correspond to x and average their perf values
  j = vlapply(seq_row(d), function(i) isTRUE(all.equal(removeMissingValues(as.list(d[i, par.names, drop = FALSE])),
                      removeMissingValues(x1))))
  if (!any(j))
    stop("No matching rows for final elite candidate found in opt.path! This cannot be!")
  y = colMeans(d[j, opt.path$y.names, drop = FALSE])
  # take first index of mating lines to get recommended x
  e = getOptPathEl(opt.path, which.first(j))
  x = trafoValue(par.set, e$x)
  x = removeMissingValues(x)
  if (control$tune.threshold)
    # now get thresholds and average them
    threshold = getThresholdFromOptPath(opt.path, which(j))
  else
    threshold = NULL
  makeTuneResult(learner, control, x, y, threshold, opt.path)
}

