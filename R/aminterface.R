# the interface between frontend ('automlr()') and backend (inside the optXXX.R files)

buildLearners = function(searchspace) {
  learners = searchspace[extractSubList(searchspace, "stacktype") == "learner"]
  makeModelMultiplexer(extractSubList(learners, "learner", simplify=FALSE))
}

aminterface = function(amstate, budget=NULL, searchspace=NULL, prior=NULL, savefile=NULL,
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
  
  if (is.null(savefile)) {
    save.interval = Inf
  } else {
    savefile = gsub('(\\.rds|)$', '.rds', savefile)
  }
  # basename gives an informative filename in case savefile is a directory
  basename = paste0("automlr_", amstate$backend, format(Sys.time(), "_%F_%H-%M"))
  
  # optional TODO:
  # check for all backend-function presence
  # check for budget vector validity
  
  # update the amstate object
  allpriors = filterNull(list(amstate$prior, prior))
  priorcombiner = ifelse(length(allpriors) == 2, paste0("combinepriors.", amstate$backend), "coalesce")
  amstate$prior = do.call(priorcombiner, allpriors)
  
  if (!is.null(searchspace)) {
    amstate$searchspace = searchspace
  }
  
  # a deep copy of backendprivatedata is necessary so that an AMState object always refers to a well defined
  # optimizer state, even after it was used as an argument of an automlr() call.
  amstate$backendprivatedata = deepcopy(amstate$backendprivatedata)
  
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
  
  ## So how do we go about this?
  ## I'ts probably a good idae to build a model-multiplexer out of the searchspace
  ## Mabe with some information about the dependencies of the model parameters.
  ## TODO How does autoWEKA remember info about similar parameters? The answer might be 'not at all'.
  
  objectiveLearner = buildLearners(amstate$searchspace)
  
  # set up or change backendprivatedata. This gets called once whenever some part of the AMState object
  # may have changed.
  callbackend("setup", amstate$backend, amstate$backendprivatedata, amstate$prior, objectiveLearner)
  
  # the writing out of intermediate results to `savefile` is done here and not delegated to the
  # backend functions. We call the backend with timeout until next write to disk. This greatly
  # reduces complexity at some marginal performance cost.
  while (!stopcondition(amstate$budget, amstate$spent)) {
    stepbudget = remainingbudget(amstate$budget, amstate$spent)
    stepbudget['walltime'] = min(stepbudget['walltime'], save.interval, na.rm=TRUE)
    usedbudget = callbackend("optimize", amstate$backend, amstate$backendprivatedata, stepbudget)
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

