# the interface between frontend ('automlr()') and backend
# (inside the optXXX.R files)

aminterface = function(amstate, budget = NULL, prior = NULL, savefile = NULL,
    save.interval = default.save.interval, new.seed = FALSE,
    max.walltime.overrun, verbosity) {
  if (!is.null(amstate$finish.time)) {
    oldamstate = amstate
    oldamstate$backendprivatedata = NULL
    oldamstate$.interruptedBPD = NULL
    oldamstate$backendoptions = NULL
    oldamstate$backend = NULL
    oldamstate$task = NULL
    oldamstate$measure = NULL
    oldamstate$previous.versions = NULL
    oldamstate$prior.backlog = NULL
    oldamstate$prior = NULL
    oldamstate$searchspace = NULL
    oldamstate$max.learner.time = NULL
    class(oldamstate) = "list"
    amstate$previous.versions = c(amstate$previous.versions, list(oldamstate))
    amstate$creation.time = Sys.time()
  }

  # if amstate$spent contains NAs everything breaks...
  assert(!anyNA(amstate$spent))

  if (is.null(savefile) || save.interval == 0) {
    save.interval = Inf
  }
  # basename gives an informative filename in case savefile is a directory
  basename = paste0("automlr_", amstate$backend,
      format(Sys.time(), "_%F_%H-%M"))
  if (!is.null(savefile)) {
    savefile = checkfile(savefile, basename)
    savefile = gsub("(\\.rds|)$", ".rds", savefile)
    amstate$savefile = savefile
  }

  # update the amstate object

  # a deep copy of backendprivatedata is necessary so that an AMState object
  # always refers to a well defined optimizer state, even after it was used as
  # an argument of an automlr() call.
  amstate$backendprivatedata = deepcopy(amstate$backendprivatedata)

  amstate$prior.backlog = c(amstate$prior.backlog, list(prior))
  if (!amstate$isInitialized) {
    updatePriors(amstate)
  }

  amstate$measure = coalesce(amstate$measure, getDefaultMeasure(amstate$task))

  # check if budget is already exceeded. in this case we return the (updated)
  # amstate object
  if (!is.null(budget)) {
    amstate$budget = budget
  }
  if (stopcondition(amstate$budget, amstate$spent)) {
    amstate$finish.time = Sys.time()
    return(amstate)
  }

  if (!new.seed) {
    setSeed(amstate$seed)
  }
  amstate$seed = getSeed()

  # set backendprivatedata. This gets called once per amstate lifetime.
  if (!amstate$isInitialized) {
    handleInterrupts({
          objectiveLearner = buildLearners(amstate$searchspace, amstate$task,
              verbosity)
          objectiveLearner = makeTimeconstraintWrapper(objectiveLearner,
              amstate$max.learner.time, amstate$max.learner.time * 1.25)
          objectiveLearner = adjustLearnerVerbosity(objectiveLearner, verbosity)
          amsetup(amstate$backendprivatedata, amstate$backendoptions,
              amstate$prior.backlog[[1]], objectiveLearner, amstate$task,
              amstate$measure, verbosity)
          amstate$prior.backlog[[1]] = NULL
          amstate$isInitialized = TRUE
          updatePriors(amstate)
        }, stop("Ctrl-C Abort"))

    amstate$seed = getSeed()

    amstate$finish.time = Sys.time()
    if (!is.null(savefile)) {
      writefile(savefile, amstate, basename)
      # since writefile might use the rng:
      setSeed(amstate$seed)
    }

  }


  # the writing out of intermediate results to `savefile` is done here and not
  # delegated to the backend functions. We call the backend with timeout until
  # next write to disk. This greatly reduces complexity at some marginal
  # performance cost.
  while (!stopcondition(amstate$budget, amstate$spent)) {
    stepbudget = remainingbudget(amstate$budget, amstate$spent)
    if ("walltime" %in% names(amstate$budget) &&
        !is.null(max.walltime.overrun)) {
      deadline = stepbudget["walltime"] + max.walltime.overrun
    } else {
      deadline = Inf
    }
    nextstop = min(stepbudget["walltime"], save.interval, na.rm = TRUE)
    stepbudget["walltime"] = nextstop
    setSeed(amstate$seed)
    backupBPD = deepcopy(amstate$backendprivatedata)
    wasInterrupted = handleInterrupts({
          usedbudget = amoptimize(amstate$backendprivatedata, stepbudget,
              verbosity, deadline)
          FALSE
        }, TRUE)
    if (wasInterrupted) {
      amstate$.interruptedBPD[[length(amstate$.interruptedBPD) + 1]] = (
            amstate$backendprivatedata)
      amstate$backendprivatedata = backupBPD
      next
    }
    amstate$spent = amstate$spent + usedbudget[names(amstate$spent)]
    # if usedbudget does not contain all names that it should contain and the
    # backend is buggy, amstate$spent could contain NAs
    assert(!anyNA(amstate$spent))


    amstate$seed = getSeed()

    amstate$finish.time = Sys.time()
    if (!is.null(savefile)) {
      writefile(savefile, amstate, basename)
      # since writefile might use the rng:
      setSeed(amstate$seed)
    }
  }
  amstate$prior = amgetprior(amstate$backendprivatedata)
  amstate
}

updatePriors = function(amstate) {
  for (p in amstate$prior.backlog) {
    amaddprior(amstate$backendprivatedata, p)
  }
  amstate$prior.backlog = NULL
  amstate$prior = amgetprior(amstate$backendprivatedata)
}
