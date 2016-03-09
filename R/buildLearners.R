
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
#' @param searchspace list of autolearners.
#' @param task the task that the searchspace is being created.
#'        \code{buildLearners} respects the task type, presence of NAs and type
#'        of covariates. Currently not supported: weights, request of
#'        probability assignment instead of class.
#' @export
buildLearners = function(searchspace, task) {

  # searchspace contains learners, wrappers and requiredwrappers.
  learners = searchspace[extractSubList(searchspace, "stacktype") == "learner"]
  wrappers = searchspace[extractSubList(searchspace, "stacktype") %in%
          c("wrapper", "requiredwrapper")]
  
  learnerObjects = list()
  modelTuneParsets = list()
  # need to keep track of all the parameter names, even the "fix" ones that
  # won't be in the SS
  allParamNames = list()  
  taskdesc = getTaskDescription(task)
  
  if (taskdesc$has.weights) {
    stop("Tasks with weights are currently not supported.")
  }
  
  info.env = new.env(parent = baseenv())
  info.env$info = taskdesc
  idRef = list()

  allcovtypes = c("factors", "ordered", "numerics")
  allcovproperties = c(allcovtypes, "missings")
  covtypes = c(names(taskdesc$n.feat)[taskdesc$n.feat > 0],
      if (taskdesc$has.missings) "missings")
  maxcovtypes = setdiff(covtypes, "missings")
  whichprops = min(3, length(taskdesc$class.levels))
  requiredClassProperty = c("oneclass", "twoclass", "multiclass")[whichprops]
  mincovtypes = covtypes
  wrapperList = list()
  handlerList = list()
  for (w in wrappers) {
    w$learner$searchspace = makeParamSet(params = lapply(w$searchspace,
            createParameter, info.env = info.env, learnerid = w$learner$name,
            forWrapper = TRUE))
    wrapperList[[w$learner$name]] = w$learner
    wrapperList[[w$learner$name]]$required = w$stacktype == "requiredwrapper"
    if (identical(maxcovtypes, allcovtypes) && length(mincovtypes) == 0) {
      next
    }
    if (w$stacktype != "requiredwrapper") {
      next
    }
    for (t in covtypes) {
      conv = w$learner$conversion(t)
      if ("" %in% conv) {
        mincovtypes = setdiff(mincovtypes, t)
      }
      maxcovtypes = union(maxcovtypes, setdiff(conv, c("", "missings")))
    }
  }

  for (i in seq_along(learners)) {
    l = myCheckLearner(learners[[i]]$learner)
    
    sslist = learners[[i]]$searchspace
    if (taskdesc$type != l$type) {
      # skip this learner, it is not fit for the task
      next
    }
    if (!hasLearnerProperties(l, requiredClassProperty)) {
      # can't handle the target variable type
      next
    }
    learnercovtypes = intersect(allcovproperties, getLearnerProperties(l))
    if (length(setdiff(mincovtypes, learnercovtypes)) != 0) {
      # there are feature types that no wrapper can remove that the learner
      # can't handle
      next
    }
    if (length(intersect(maxcovtypes, learnercovtypes)) == 0) {
      # we can't convert the features to any kind of feature that the learner
      # can handle
      next
    }
    for (canHandle in intersect(allcovproperties, getLearnerProperties(l))) {
      handlerList[[canHandle]] = c(handlerList[[canHandle]], l$id)
    }
    aux = buildTuneSearchSpace(sslist, l, info.env, idRef)
    modelTuneParsets[[l$id]] = aux$tss
    allParamNames[[l$id]] = aux$nondefParamNames
    # updated learner object with fixed hyperparameters
    learnerObjects = c(learnerObjects, list(aux$l))
    idRef = aux$idRef
  }
  checkParamIds(idRef)
  
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
  makeAMExoWrapper(multiplexer, wrapperList, taskdesc, idRef, handlerList,
      allLearners)
}

checkParamIds = function(idRef) {
  # check that the IDs match.
  for (parid in names(idRef)) {
    if (length(idRef[[parid]]) == 1) {
      warningf(paste0("Parameter '%s' of learner '%s' is the only one with",
              " parameter id '%s'."),
          idRef[[parid]][[1]]$param$id, idRef[[parid]][[1]]$learner$id, parid)
      next
    }
    needstomatch = c("type", "len", "lower", "upper", "values", "allow.inf")
    protopar = idRef[[parid]][[1]]$param
    for (otherpar in idRef[[parid]]) {
      for (property in needstomatch) {
        prop1 = protopar[[property]]
        prop2 = otherpar$param[[property]]
        if (!identical(prop1, prop2)) {
          stopf(paste0("Prameter '%s' of learner '%s' has the same id '%s' as",
                  " param '%s' of learner '%s', but their '%s' property do not",
                  " match. ('%s' vs. '%s')"),
              protopar$id, idRef[[parid]][[1]]$learner$id, parid,
              otherpar$param$id, otherpar$learner$id, property,
              if (is.null(prop1)) "NULL" else paste(prop1, collapse = ", "),
              if (is.null(prop2)) "NULL" else paste(prop2, collapse = ", "))
        }
      }
    }
  }
}

buildTuneSearchSpace = function(sslist, l, info.env, idRef) {
  lp = getParamSet(l)
  lpids = getParamIds(lp)
  lptypes = getParamTypes(lp, use.names = TRUE)
  allParams = extractSubList(sslist, "name")
  untouchedParams = setdiff(lpids, allParams)
  if (length(untouchedParams)) {
    warningf(paste0("Learner '%s' has parameters %s that are not mentioned in",
            " search space."),
        l$id, paste(untouchedParams, collapse = ", "))
  }
  tuneSearchSpace = list()
  canNotBeAMLRFIX = character(0)
  areAlreadyAMLRFIX = character(0)
  nondefParamNames = character(0)
  for (param in sslist) {
    if (param$type == "bool") {
      # useful since now we can automatically check for feasibility by checking
      # whether everything in param$values is feasible.
      param$values = c(TRUE, FALSE)
    }
    origParamName = amlrTransformName(param$name)
    if (param$name != origParamName) {
      if (is.null(param$req)) {
        stopf(paste0("Parameter '%s' for learner '%s' has an .AMLRFIX suffix",
                " but no requirements"),
            param$name, l$id)
      }
      if (param$type %in% c("fix", "def")) {
        stopf(paste0("Parameter '%s' for learner '%s' is of type '%s' but has",
                " an .AMLRFIX suffix."),
            param$name, l$id, param$type)
      }
      if (identical(param$special, "dummy")) {
        stopf(paste0("Parameter '%s' for learner '%s' has an .AMLRFIX suffix",
                " but is also a DUMMY parameter."),
            param$name, l$id, param$type)
      }
      if (origParamName %in% canNotBeAMLRFIX) {
        stopf(paste0("Parameter '%s' for learner '%s' cannot have an .AMLRFIX",
                " suffix. Possible Reasons:\nanother parameter with that name",
                " has type 'def' or 'fix', has no requirements or is a",
                " 'dummy'."),
            param$name, l$id)
      }
      areAlreadyAMLRFIX = union(areAlreadyAMLRFIX, origParamName)
    }
    if (is.null(param$req) || identical(param$special, "dummy") ||
        param$type %in% c("def", "fix")) {
      if (origParamName %in% areAlreadyAMLRFIX) {
        stopf(paste0("Parameter '%s' for learner '%s' already defined with",
                " .AMLRFIX suffix cannot be given without requirement or as",
                " dummy, def or fix."),
            param$name, l$id)
      }
      canNotBeAMLRFIX = union(canNotBeAMLRFIX, origParamName)
    }
    # inject --> is not in the learner already unless this is an AMLRPREFIX
    # case
    if (identical(param$special, "inject") &&
        (origParamName %nin% areAlreadyAMLRFIX ||
          origParamName %nin% getParamIds(getParamSet(l)))) {
      if (origParamName %in% getParamIds(getParamSet(l))) {
        stopf(paste0("Parameter '%s' is present in learner '%s' but is marked",
                " as 'inject' in search space."),
            param$name, l$id)
      }
      l$par.set = c(l$par.set, makeParamSet(createParameter(param, info.env,
                  learnerid = l$id, do.trafo = FALSE, facingOutside = FALSE)))
      lp = getParamSet(l)
      lpids = getParamIds(lp)
      # recreate lptypes here, since it may have changed 
      lptypes = getParamTypes(lp, use.names = TRUE)
    } else if (identical(param$special, "dummy")) {
      if (origParamName %in% getParamIds(getParamSet(l))) {
        stopf(paste0("Parameter '%s' is present in learner '%s' but is marked",
                " as 'dummy' in search space."),
            param$name, l$id)
      }
      if (param$type %in% c("def", "fix")) {
        stopf(paste0("Dummy parameter '%s' given for learner '%s' must not be",
                " of type '%s'."),
            param$name, l$id, param$type)
      }
    } else {
      if (origParamName %nin% lpids) {
        stopf(paste0("Parameter '%s' as listed in search space is not",
                " available for learner '%s'."),
            param$name, l$id)
      }
      if (!allfeasible(lp, param$values, origParamName, param$dim)) {
        # there is one 'special case': param$values might be names that index
        # into lp$pars[[param$name]]$values.
        vals = getValues(lp)[[origParamName]]
        if (isSubset(param$values, names(vals)) &&
            param$dim == getParamLengths(lp)[[origParamName]]) {
          param$values = vals[param$values]
          assert(allfeasible(lp, param$values, origParamName, param$dim))
        } else {
          stopf(paste0("Parameter '%s' as listed in search space has",
                  " infeasible bounds '%s' for learner '%s'."),
            param$name, paste(param$values, collapse = "', '"), l$id)
        }
      }
      partype = lptypes[[origParamName]]
      if ((partype %nin% c("numeric", "numericvector", "untyped")) &&
          (param$type == "real" || (param$type == "int" &&
              partype %nin% c("integer", "integervector")))) {
        stopf(paste0("Parameter '%s' as listed in search space has wrong type",
                " '%s' for learner '%s'"),
            param$name, param$type, l$id)
      }
      if ((partype != "untyped") &&
          ((param$type == "int" &&
              partype %nin% c("integer", "integervector")) ||
           (param$type == "bool" &&
             partype %nin% c("logical", "logicalvector")) ||
           (param$type == "cat" &&
             partype %nin% c("discrete", "discretevector", "character",
                 "charactervector")))) {
        warningf(paste0("Parameter '%s' for learner '%s' is of type '%s' and",
                " has different (but feasible) type '%s' listed in search",
                " space."),
            param$name, l$id, partype, param$type)
        if (param$type == "cat") {
          # if the underlying type is a VectorParam but not discrete, we will
          # have to unlist().
          param$amlr.isNotCat = TRUE
        }
      }
    }
    if (param$type == "def") {
      # check whether this is /actually/ the default
      truedefault = getDefaults(getParamSet(l))[[origParamName]]
      defaultcandidate = getHyperPars(l)[[origParamName]]
      if (!is.null(defaultcandidate)) {
        l = removeHyperPars(l, origParamName)
        # we try to use the default, but apparently the value is already set in
        # the learner object.
        if (is.null(truedefault) || truedefault != defaultcandidate) {
          warningf(paste0("Parameter '%s' for learner '%s' is of type",
                  " 'default', but the learner has it already set to a",
                  " different value.\nThis value hase been removed; the",
                  " default will be used."),
              param$name, l$id)
        }
      }
      if (is.null(truedefault) != is.null(param$values) ||
          (!is.null(truedefault) && truedefault != param$values)) {
        warningf(paste0("Parameter '%s' for learner '%s' is of type 'default'",
                " but its alleged default '%s' differs from the true default",
                " '%s'."),
            param$name, l$id,
            if (is.null(param$values)) "NULL" else param$values,
            if (is.null(truedefault)) "NULL" else truedefault)
        param$type = "fix"
      }
    }
    if (is.null(param$special) && param$type !="def" &&
        hasRequires(lp$pars[[origParamName]]) && is.null(param$req)) {
      warningf(paste0("Parameter '%s' for learner '%s' has a 'requires'",
              " argument but the one given in the search space has not."),
          param$name, l$id)
    }
    if (param$type != "def") {
      nondefParamNames = c(nondefParamNames, origParamName)
    }
    if (param$type == "fix") {
      if (!is.null(param$values)) {
        if (lptypes[[origParamName]] == "discretevector") {
          assignment = list(rep(list(param$values), param$dim))
        } else {
          assignment = list(
              if (param$dim > 1) rep(param$values, param$dim) else param$values)
        }
  
        names(assignment) = origParamName
        l = setHyperPars(l, par.vals = assignment)
      }
    } else {
      if (origParamName %in% names(getHyperPars(l))) {
        # make sure this is not set at a default.
        warningf(paste0("Parameter '%s' for learner '%s' was already set to a",
                " value; this value has been removed."),
            param$name, l$id)
        l = removeHyperPars(l, param$name)
      }
      if (param$type != "def") {  # variable parameter
        newparam = createParameter(param, info.env, learnerid = l$id)
        if (!is.null(param$id)) {
          idRef[[param$id]] = c(idRef[[param$id]],
              list(list(learner = l, param = newparam)))
        }

        newparam$amlr.isNotCat = !is.null(param$amlr.isNotCat)
        tuneSearchSpace = c(tuneSearchSpace, list(newparam))
      }
    }
  }
  list(tss = makeParamSet(params = tuneSearchSpace), l = l, idRef = idRef,
      nondefParamNames = unique(nondefParamNames))
}

allfeasible = function(ps, totest, name, dimension) {
  testlist = list(0)
  names(testlist) = name
  for (t in totest) {
    if (getParamTypes(ps, use.names = TRUE)[[name]] == "discretevector") {
      t = list(t)
    }
    testlist[[1]] = if (dimension > 1) rep(t, dimension) else t
    if (!isFeasible(ps$pars[[name]], testlist[[1]])) {
      # this would be easier if there was a way to disable requirements
      # checking.
      return(FALSE)
    }
  }
  TRUE
}

createParameter = function(param, info.env, learnerid, do.trafo = TRUE,
    facingOutside = TRUE, forWrapper = FALSE) {
  # facingOutside == FALSE means
  # 1) create LearnerParam instead of Param
  # 2) remove .AMLRFIX# suffix
  if (param$type %in% c("int", "real")) {
    pmin = param$values[1]
    pmax = param$values[2]
  }
  if (!facingOutside) {
    param$name = amlrTransformName(param$name)
  }
  if (!do.trafo) {
    if (!is.null(param$trafo)) {
      if (param$type %in% c("int", "real") && !identical(param$trafo, "exp")) {
        # if the transformation is not our own 'exp' trafo, then we can't say
        # for sure what the real bounds are, or even what the param type is.
        pmin = -Inf
        pmax = Inf
        param$type = "real"
      }
      param$trafo = NULL
    }
  }
  if (!is.null(param$trafo) && identical(param$trafo, "exp")) {
    if (param$type == "real") {
      aux = createTrafo(pmin, pmax, FALSE)
    } else {
      # param$type == "int"
      aux = createTrafo(pmin, pmax, TRUE)
    }
    ptrafo = aux$trafo
    pmin = aux$newmin
    pmax = aux$newmax
  } else {
    ptrafo = param$trafo  # will either be a function or NULL
  }
  surrogatetype = param$type
  if (param$type == "fix" && !forWrapper) {
    # don't warn for wrappers, for them fixed injects are the norm
      warningf(paste0("Parameter '%s' for learner '%s' is marked",
              " 'dummy/inject' and has type 'fix'; This usually does not make",
              " sense."),
          param$name, learnerid)
    if (!is.null(param$values)) {
      if (is.numeric(param$values[1])) {
        surrogatetype = "real"
        pmin = param$values[1]
        pmax = param$values[1]
      } else if (is.logical(param$values[1])) {
        surrogatetype = "bool"
      }
    }
  }
  if (param$dim > 1) {
    if (!facingOutside) {
      constructor = switch(surrogatetype,
          real = makeNumericVectorLearnerParam,
          int = makeIntegerVectorLearnerParam,
          cat = makeDiscreteVectorLearnerParam,
          bool = makeLogicalVectorLearnerParam,
          fix = makeDiscreteVectorLearnerParam,
          NULL)
    } else {
      constructor = switch(surrogatetype,
          real = makeNumericVectorParam,
          int = makeIntegerVectorParam,
          cat = makeDiscreteVectorParam,
          bool = makeLogicalVectorParam,
          fix = makeDiscreteVectorParam,
          NULL)
    }
    paramlist = list(id = param$name, len = param$dim, requires = param$req)
  } else {
    if (!facingOutside) {
      constructor = switch(surrogatetype,
          real = makeNumericLearnerParam,
          int = makeIntegerLearnerParam,
          cat = makeDiscreteLearnerParam,
          bool = makeLogicalLearnerParam,
          fix = makeDiscreteLearnerParam,
          NULL)
    } else {
      constructor = switch(surrogatetype,
              real = makeNumericParam,
              int = makeIntegerParam,
              cat = makeDiscreteParam,
              bool = makeLogicalParam,
              fix = makeDiscreteParam,
              NULL)
    }
    paramlist = list(id = param$name, requires = param$req)
  }
  paramlist = c(paramlist, switch(surrogatetype,
      int = list(lower = pmin, upper = pmax, trafo = ptrafo),
      real = list(lower = pmin, upper = pmax, trafo = ptrafo),
      cat = list(values = {
            x = param$values
            if (!test_named(x)) {
              names(x) = param$values
            }
            x}),
      bool = list(),
      fix = list(values = {
            x = param$values
            if (!test_named(x)) {
              names(x) = param$values
            }
            x}),
      def = stopf(paste0("Parameter '%s' for learner '%s' marked as 'inject'",
              " must not have type 'def'."),
          param$name, learnerid),
      stopf("Unknown type '%s'; parameter '%s', learner '%s'", param$type,
          param$name, learnerid)))
  if (!facingOutside) {
    paramlist$trafo = NULL
  }
  pobject = do.call(constructor, paramlist, quote = TRUE)
  if (!is.null(pobject$trafo)) {
    environment(pobject$trafo) = list2env(as.list(environment(pobject$trafo),
            all.names = TRUE), parent = info.env)
    if (identical(param$trafo, "exp")) {
      pobject$amlr.origValues = param$values
    }
  }
  pobject$amlr.isDummy = identical(param$special, "dummy")
  pobject
}

#' Turn learner id string into learner object, if necessary
#' 
#' @param learner a character scalar or learner object
myCheckLearner = function (learner) {
  if (is.character(learner)) {
    learner = makeLearner(learner)
  } else {
    assertClass(learner, classes = "Learner")
  }
  learner
}

createTrafo = function(min, max, isint) {
  if (isint) {
    assert(min >= 0)
    addzero = if (min == 0) 0
    if (min == 0) {
     min = 1
   }
    ratio = sqrt((min+1) / min)
    sequence = unique(c(addzero,
            round(min * ratio ^ (seq(from = 0,
                          to = floor(log(max/min, base = ratio))))),
            max))
    return(list(trafo = function(x) ifelse(is.na(x), NA, sequence[x]),
            newmin = 1, newmax = length(sequence)))
  } else {
    #  !!! We have to evaluate 'min' and 'max' here, otherwise they stay in the environment as 'promise' objects
    # and weird stuff happens! 
    assert(min > 0)
    assert(max > 0)

    return(list(trafo = function(x) min * (max / min)^x,
            newmin = 0, newmax = 1))
  }
}

#' Like mlr's mMMPS but respecting requirements.
#'
#' @param multiplexer the model multiplexer to use to create the ParamSet
#' @param modelParsets the list of param sets that are used to create the mmps.
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
    newnames = paste(modelid, oldnames, sep = '.')
    # substitution is a list(oldname = quote(newname))
    substitution = lapply(newnames, asQuoted)
    names(substitution) = oldnames
    for (paramiter in seq_along(origpars)) {
      # iterate over the parameters in each ParamSet
      cp = origpars[[paramiter]]
      cpname = names(origpars)[paramiter]
      cprequires = cp$requires
      # newname: the name fo the current Param within mm$searchspace
      newname = paste(modelid, cpname, sep = '.')
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

replaceRequires = function(cprequires, substitution) {
  # what we are going to do is substitute the variable names with their new
  # prefixed versions.
  # HOWEVER: R uses different scoping for function calls than for variables.
  # therefore e.g.
  # > c <- 1
  # > c(c, c)
  # doesn't give an error. This is a pain when trying to do what I'm doing here.
  # So we will manually substitute all function calls with different names.
  #
  # the width.cutoff may be a problem? I wouldn't assume so if deparse keeps
  # function name and opening parenthesis on the same line.
  parsed = deparse(as.expression(cprequires),
      control = c("keepInteger", "keepNA"), width.cutoff = 500)
  funcallmatch = paste0("(?:((?:[[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|",
      "(`)((?:[^`\\\\]|\\\\.)+`))(\\()")
  
  parsed = gsub(funcallmatch, "\\2.AUTOMLR_TEMP_\\1\\3\\4", parsed)
  #the following would be dumb:
  #parsed[1] = sub(".AUTOMLR_TEMP_expression(", "expression(", parsed[1],
  # fixed = TRUE) # NO!
  cprequires = asQuoted(paste(parsed, collapse = "\n"))
  # the following line is a bit of R magic. Use do.call, so that cprequires,
  # which is a 'quote' object, is expanded to its actual content. The
  # 'substitute' call will change all names of the old parameters to the new
  # parameters.
  cprequires = do.call(substitute, list(cprequires, substitution))
  
  funcallmatchReverse = paste0("(?:\\.AUTOMLR_TEMP_((?:[[:alpha:]]|",
      "[.][._[:alpha:]])[._[:alnum:]]*)|",
      "(`)\\.AUTOMLR_TEMP_((?:[^`\\\\]|\\\\.)+`))(\\()")
  parsed = deparse(cprequires,
      control = c("keepInteger", "keepNA"), width.cutoff = 500)
  parsed = gsub(funcallmatchReverse, "\\2\\1\\3\\4", parsed)
  eval(asQuoted(paste(parsed, collapse = "\n")))
}