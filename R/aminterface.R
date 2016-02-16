# the interface between frontend ('automlr()') and backend (inside the optXXX.R files)

aminterface = function(amstate, budget=NULL, prior=NULL, savefile=NULL,
                       save.interval=default.save.interval, new.seed=FALSE, ...) {
  if (!is.null(amstate$finish.time)) {
    oldamstate = amstate
    oldamstate$backendprivatedata = NULL
    oldamstate$task = NULL
    oldamstate$measure = NULL
    oldamstate$previous.versions = NULL
    class(oldamstate) = "list"
    amstate$previous.versions = c(amstate$previous.versions, list(oldamstate))
    amstate$creation.time = Sys.time()
  }
  
  assert(!anyNA(amstate$spent))  # if amstate$spent contains NAs everything breaks...
  
  if (is.null(savefile) || save.interval == 0) {
    save.interval = Inf
  } else {
    savefile = gsub('(\\.rds|)$', '.rds', savefile)
  }
  # basename gives an informative filename in case savefile is a directory
  basename = paste0("automlr_", amstate$backend, format(Sys.time(), "_%F_%H-%M"))
  
  # optional TODO:
  # check for all backend-function presence
  
  # update the amstate object

  # a deep copy of backendprivatedata is necessary so that an AMState object always refers to a well defined
  # optimizer state, even after it was used as an argument of an automlr() call.
  amstate$backendprivatedata = deepcopy(amstate$backendprivatedata)
  
  allpriors = filterNull(list(amstate$prior, prior))
  if (length(allpriors > 1)) {
    amstate$prior = combinepriors(amstate$backendprivatedata, amstate$prior, prior)
  } else {
    amstate$prior = coalesce(amstate$prior, prior)
  }
  
  # check if budget is already exceeded. in this case we return the (updated) amstate object
  if (!is.null(budget)) {
    amstate$budget = budget
  }
  if (stopcondition(amstate$budget, amstate$spent)) {
    amstate$finish.time = Sys.time()
    return(amstate)
  }
  
  if (!new.seed) {
    .Random.seed = amstate$seed
  }
  
  ## TODO How does autoWEKA remember info about similar parameters? The answer might be 'not at all'.
  
  objectiveLearner = buildLearners(amstate$searchspace, amstate$task)
  
  # set up or change backendprivatedata. This gets called once whenever some part of the AMState object
  # may have changed.
  amsetup(amstate$backendprivatedata, amstate$prior, objectiveLearner, amstate$task)  # TODO: also give measure.
  
  # the writing out of intermediate results to `savefile` is done here and not delegated to the
  # backend functions. We call the backend with timeout until next write to disk. This greatly
  # reduces complexity at some marginal performance cost.
  while (!stopcondition(amstate$budget, amstate$spent)) {
    stepbudget = remainingbudget(amstate$budget, amstate$spent)
    stepbudget['walltime'] = min(stepbudget['walltime'], save.interval, na.rm=TRUE)
    usedbudget = amoptimize(amstate$backendprivatedata, stepbudget)
    amstate$spent = amstate$spent + usedbudget[names(amstate$spent)]
    assert(!anyNA(amstate$spent))  # this may happen if usedbudget does not contain all names that it should contain.
    amstate$seed = .Random.seed
    amstate$finish.time = Sys.time()
    if (!is.null(savefile)) {
      savefile = writefile(savefile, amstate, basename)
      .Random.seed = amstate$seed  # since writefile may use the rng.
    }
  }
  amstate
}

