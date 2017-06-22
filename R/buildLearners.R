
# what is missing?
# - giving defaults / fixes for vector params
# - 'inject'ed / dummy parameters always have feasibility region of the first
#   parameter given with this name
# - treatment of same-id-parameters

# short reference of important features to watch:
#  - taskdesc has:
#    - type (classif, regr, ...) that must match the learner
#    - n.feat with: numerics, factors, ordered, preprocessing may depend on this
#    - has.missings: preprocessing may depend on this
#    - has.weights: give an error, since we can't handle weights right now.
#    - length(class.levels) if twoclass or multiclass. This will reduce our
#      search space since some can't handle multi
#  - learner properties can be:
#    - what covariates can be handled?
#      - factors
#      - ordered
#      - numerics
#    - what target variables can be handled?
#      - oneclass
#      - twoclass
#      - multiclass
#    - missings -- NAs allowed?
#    - ** the following would be nice but is currently being ignored **
#      - weights -- weights can be handled
#      - class.weights -- class weights can be handled
#      - form of prediction
#        - prob -- probability can be predicted
#        - se -- standard error can be predicted


#' Take a list of autolearners and create a (big) mlr Learner object
#' 
#' @param searchspace [list of \code{Autolearner}]\cr
#'   List of autolearners.
#' @param task [\code{Task} | \code{TaskDesc}]\cr
#'   The task that the searchspace is being created. \code{buildLearners}
#'   respects the task type, presence of NAs and type of covariates. Currently
#'   not supported: weights, request of probability assignment instead of class.
#' @param verbosity [\code{numeric(1)}]\cr
#'   Give detailed warnings. See the \code{\link{automlr}} parameter.
#' @export
buildLearners = function(searchspace, task, verbosity = 0) {
  
  # searchspace contains learners, wrappers and requiredwrappers.
  learners = searchspace[extractSubList(searchspace, "stacktype") == "learner"]
  wrappers = searchspace[extractSubList(searchspace, "stacktype") %in%
          c("wrapper", "requiredwrapper")]
  
  learnerObjects = list()
  modelTuneParsets = list()
  # need to keep track of all the parameter names, even the "fix" ones that
  # won't be in the SS
  allParamNames = list()

  taskdesc = getTaskDesc(task)
  
  if (taskdesc$has.weights) {
    stop("Tasks with weights are currently not supported.")
  }
  
  allcovtypes = c("factors", "ordered", "numerics")
  allcovproperties = c(allcovtypes, "missings")
  presentprops = c(names(taskdesc$n.feat)[taskdesc$n.feat > 0],
      if (taskdesc$has.missings) "missings")
  maxcovtypes = setdiff(presentprops, "missings")
  classlvlcount = min(3, length(taskdesc$class.levels))
  requiredClassProperty = c("oneclass", "twoclass", "multiclass")[classlvlcount]
  requiredprops = presentprops
  wrapperList = list()
  for (w in wrappers) {
    wl = w$learner
    wl$searchspace = makeParamSet(params = lapply(w$searchspace,
            createParameter, learnerid = wl$name, forWrapper = TRUE))
    wl$required = w$stacktype == "requiredwrapper"
    wrapperList[[wl$name]] = wl
    if (identical(maxcovtypes, allcovtypes) && length(requiredprops) == 0) {
      next
    }
    if (w$stacktype != "requiredwrapper") {
      next
    }
    for (t in presentprops) {
      conv = wl$conversion(t)
      if ("" %in% conv) {
        requiredprops = setdiff(requiredprops, t)
      }
      maxcovtypes = union(maxcovtypes, setdiff(conv, c("", "missings")))
    }
  }
  
  handlerList = list()
  for (i in seq_along(learners)) {
    li = learners[[i]]
    tryResult = try(lil <- myCheckLearner(li$learner), silent = TRUE)
    if (is.error(tryResult)) {
      errcall = attr(tryResult, "condition")$call[[1]]
      errmsg = attr(tryResult, "condition")$message
      if (identical(errcall, quote(requirePackages))) {
        if (verbosity.sswarnings(verbosity)) {
          warningf("Package for learner '%s' is missing; skipping.",
              li$learner)
        }
        next
      } else {
        stopf("makeLearner gave unexpected error for learner '%s':\n%s",
            li$learner, errmsg)
      }
    }
    if (taskdesc$type != lil$type) {
      # skip this learner, it is not fit for the task
      if (verbosity.sswarnings(verbosity)) {
        messagef("Skipping learner '%s': Learner can not handle target type.",
            lil$id)
      }
      next
    }
    learnercovtypes = intersect(allcovproperties, getLearnerProperties(lil))
    if (length(setdiff(requiredprops, learnercovtypes)) != 0) {
      # there are feature types that no wrapper can remove that the learner
      # can't handle
      if (verbosity.sswarnings(verbosity)) {
        messagef("Skipping learner '%s': Learner can not handle feature types.",
            lil$id)
      }
      next
    }
    if (length(intersect(maxcovtypes, learnercovtypes)) == 0) {
      # we can't convert the features to any kind of feature that the learner
      # can handle
      if (verbosity.sswarnings(verbosity)) {
        messagef(paste("Skipping learner '%s':",
            "Task has no features the learner can handle."),
            lil$id)
      }
      next
    }
    for (canHandle in intersect(allcovproperties, getLearnerProperties(lil))) {
      handlerList[[canHandle]] %c=% lil$id
    }
    aux = buildTuneSearchSpace(li$searchspace, lil, info.env, verbosity)
    modelTuneParsets[[lil$id]] = aux$tss
    allParamNames[[lil$id]] = aux$nondefParamNames
    # updated learner object with fixed hyperparameters
    learnerObjects %c=% list(aux$l)
  }
  checkParamIds(modelTuneParsets, verbosity)

  if (length(learnerObjects) == 0) {
    warning("No model fits the given task, returning NULL.")
    return(NULL)
  }

  multiplexer = removeHyperPars(makeModelMultiplexer(learnerObjects),
      "selected.learner")

  tuneParamSet = makeModelMultiplexerParamSetEx(multiplexer, modelTuneParsets,
      allParamNames)
  multiplexer$searchspace = tuneParamSet
  allLearners = unlist(tuneParamSet$pars$selected.learner$values)
  am = makeAMExoWrapper(multiplexer, wrapperList, taskdesc, idRef, handlerList,
      allLearners)
  adjustLearnerVerbosity(am, verbosity)
}

checkParamIds = function(parsets, verbosity) {
  idRef = list()
  for (ps in parsets) {
    for (param in ps$pars) {
      if (!is.null(param$amlr.id)) {
        idRef[[param$amlr.id]] %c=% list(param)
      }
    }
  }
  # check that the IDs match.
  for (parid in names(idRef)) {
    if (length(idRef[[parid]]) == 1) {
      # next  # not implemented and therefore only annoying
      if (verbosity.sswarnings(verbosity)) {
        warningf(paste("Parameter '%s' of learner '%s' is the only one with",
                "parameter id '%s'."),
            idRef[[parid]][[1]]$id, idRef[[parid]][[1]]$amlr.lrnid, parid)
      }
      next
    }
    needstomatch = c("type", "len", "lower", "upper", "values", "allow.inf")
    protopar = idRef[[parid]][[1]]
    for (otherpar in idRef[[parid]]) {
      for (property in needstomatch) {
        prop1 = protopar[[property]]
        prop2 = otherpar[[property]]
        if (!identical(prop1, prop2)) {
          stopf(paste0("Prameter '%s' of learner '%s' has the same id '%s' as",
                  " param '%s' of learner '%s', but their '%s' property do not",
                  " match. ('%s' vs. '%s')"),
              protopar$id, protopar$amlr.lrnid, parid,
              otherpar$id, otherpar$amlr.lrnid, property,
              convertToShortString(prop1), convertToShortString(prop2))
        }
      }
    }
  }
}

#' @title Turn learner id string into learner object, if necessary
#' 
#' @description
#' If the given learner is a \code{character(1)} identifying a learner, the
#' corresponding learner is generated using \code{makeLearner}.
#' 
#' @param learner [\code{character(1)}|\code{Learner}]\cr
#'   A character scalar or learner object.
myCheckLearner = function (learner) {
  if (is.character(learner)) {
    learner = makeLearner(learner)
  } else {
    assertClass(learner, classes = "Learner")
  }
  learner
}

#' @title Like mlr's \code{makeModelMultiplexerParamSet}, but respecting
#' requirements.
#' 
#' @description
#' This is necessary since \code{makeModelMultiplexerParamSet} does not handle
#' parameter requirements correctly.  
#'
#' @param multiplexer [\code{ModelMultiplexer}]\cr
#'   The model multiplexer to use to create the ParamSet.
#' @param modelParsets [\code{ParamSet}]\cr
#'   The list of param sets that are used to create the mmps.
#' @param origParamNames [list of \code{character}]\cr
#'   Maps the IDs of the individual \code{Learner}s to the set of parameters
#'   that are actually used and need to be multiplexed.
#' 
#' @return [\code{ModelMultiplexer}]
#' The \code{ModelMultiplexer} with repaired requirements.
makeModelMultiplexerParamSetEx = function(multiplexer, modelParsets,
    origParamNames) {
  searchspace = do.call(makeModelMultiplexerParamSet, c(list(multiplexer),
          modelParsets, .check = FALSE))
  # now we need to deal with the bug that makeModelMultiplexer overrides
  # requirements
  for (modeliter in seq_along(modelParsets)) {
    origpars = modelParsets[[modeliter]]$pars
    modelid = names(modelParsets)[modeliter]
    # oldnames: the names as in the original model
    oldnames = origParamNames[[modeliter]]
    # newnames: the name that was assigned in mm$searchspace
    newnames = paste(modelid, oldnames, sep = ".")
    # substitution is a list(oldname = quote(newname))
    substitution = lapply(newnames, asQuoted)
    names(substitution) = oldnames
    for (paramiter in seq_along(origpars)) {
      # iterate over the parameters in each ParamSet
      cp = origpars[[paramiter]]
      cpname = names(origpars)[paramiter]
      cprequires = cp$requires
      # newname: the name fo the current Param within mm$searchspace
      newname = paste(modelid, cpname, sep = ".")
      newrequires = searchspace$pars[[newname]]$requires
      searchspace$pars[[newname]]$amlr.learnerName = modelid
      if (is.null(cprequires)) {
        # if there is no '$requires' we don't need to do anything
        next
      }
      cprequires = replaceRequires(cprequires, substitution)
      newrequires = substitute(a && b,
          list(a = newrequires, b = deExpression(cprequires)))
      # at this position, newrequires has the form
      # (new requires) && (old requires)
      # where the use of short-cirquiting && should solve any problems that we
      # might get when querying isFeasible.
      searchspace$pars[[newname]]$requires = newrequires
    }
  }
  searchspace
}
