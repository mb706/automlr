


#' Take a list of autolearners and create a (big) mlr Learner object
#' 
#' @param searchspace list of autolearners.
#' @param task the task that the searchspace is being created. \code{buildLearners} respects the task
#'        type, presence of NAs and type of covariates. Currently not supported: weights, request of
#'        probability assignment instead of class.
#' @export
buildLearners = function(searchspace, task) {
  # ok, this will be a big project. what do we need?
  # [x] load the mlr learners if an id is given. I think mlr has an aux function for that? Yes, but it is private :-/
  # [x] filter out learners by what is needed, e.g.
  #   [x] check if the learner can handle the input data format (numeric, factorial)
  #   [x] check if there is a wrapper that turns the input data into appropriate format (e.g. removes factors)
  #   [x] (advanced) filter out classif/reg by task type
  # [x] create the appropriate search space
  #   [x] for default, check if the given defaults are actually the builtin defaults; warn if not and add "our" default as fixed value
  #   [x] for fixed, set the fixed value
  #   [x] for variable search space, add the parameter to the search space
  #     [x] warn if there is a requirement in the builtin parameter but not ours?
  #     [x] do a simple hack for dummy variables (check it is actually a dummy!)
  #     [x] respect the transformation if required, respecting int / real types
  #     [x] check that given parameter is the right type as the builtin type
  #     [x] check that given range is within the builtin range / given categories are feasible
  #   [x] question? does makeModelMultiplexer keep hyperparameter settings? answer: yes, it does.
  # [ ] some magic
  #   [/] parameters that are somehow transformed to other parameters. Can we thus solve classif.bartMachine$mh_prob_steps?
  #   [/] maybe we hack mlr itself and make transformation functions that depend on other hyperparameters.
  #       - this cannot work because there may be circular dependencies.
  #       - so we need to somehow wrap classif.pamr to make threshold.predict = max(thresholds) * threshold.predict.fraction i guess
  #   [ ] i think i have to reimplement classif.LiblineaRXXX myself to get the broken parameter space back together.
  #   [x] wrappers have to be able to give some information about transformations they make
  #     [x] removing factors / removing numerics; respecting that factors/numerics might not be present to begin with.
  #     [x] removing / not removing NAs (in factors/numerics); respecting that NAs might not be present to begin with.
  #   [x] parameter dependency on number of columns etc. is easy by injecting an environment into the trafo function.
  # [ ] somehow link the same-id parameters
  #   [x] warning if id happens only once.
  
  learners = searchspace[extractSubList(searchspace, "stacktype") == "learner"]  # exclude e.g. wrappers
  wrappers = searchspace[extractSubList(searchspace, "stacktype") %in% c("wrapper", "requiredwrapper")]
  
  learnerObjects = list()
  modelTuneParsets = list()
  taskdesc = getTaskDescription(task)
  
  if (taskdesc$has.weights) {
    stop("Tasks with weights are currently not supported.")
  }
  
  info.env = new.env(parent=baseenv())
  info.env$info = taskdesc
  idRef = list()
  # short reference of important features to watch:
  #  - taskdesc has:
  #    - type (classif, regr, ...) that must match the learner
  #    - n.feat with: numerics, factors, ordered, preprocessing may depend on this
  #    - has.missings: preprocessing may depend on this
  #    - has.weights: give an error, since we can't handle weights right now.
  #    - length(class.levels) if twoclass or multiclass. This will reduce our search space since some can't handle multi
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
  #    - ** the following would be nice in the future but is currently being ignored **
  #      - weights -- weights can be handled
  #      - class.weights -- class weights can be handled
  #      - form of prediction
  #        - prob -- probability can be predicted
  #        - se -- standard error can be predicted

  # wrapper need additional field 'automlrInfoEx' containing:
  #  $conversion: function taking one of "factors", "ordered", "numerics", "missings" and returning a subset of these
  #   (the empty string if the input type can be removed).

  allcovtypes = c("factors", "ordered", "numerics", "missings")
  covtypes = c(names(taskdesc$n.feat)[taskdesc$n.feat > 0], if (taskdesc$has.missings) "missings")

  requiredClassProperty = c("oneclass", "twoclass", "multiclass")[min(3, length(taskdesc$class.levels))]
  mincovtypes = covtypes
  maxcovtypes = c(covtypes, "missings")  # absence of missings is never a problem
  wrapperList = list()
  handlerList = list()
  for (w in wrappers) {
    w$learner$searchspace = makeParamSet(params=lapply(w$searchspace, createParameter, info.env=info.env, learnerid=w$learner$name))
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
      maxcovtypes = union(maxcovtypes, setdiff(t, ""))
    }
  }

  for (i in seq_along(learners)) {
    l = myCheckLearner(learners[[i]]$learner)
    
    sslist = learners[[i]]$searchspace
    if (taskdesc$type != l$type) {  # skip this learner, it is not fit for the task
      next
    }
    if (!hasLearnerProperties(l, requiredClassProperty)) {
      # can't handle the target variable type
      next
    }
    learnercovtypes = intersect(allcovtypes, getLearnerProperties(l))
    if (length(setdiff(mincovtypes, learnercovtypes)) != 0) {
      # there are feature types that no wrapper can remove that the learner can't handle
      next
    }
    if (length(intersect(maxcovtypes, learnercovtypes)) == 0) {
      # we can't convert the features to any kind of feature that the learner can handle
      next
    }
    for (canHandle in intersect(allcovtypes, getLearnerProperties(l))) {
      handlerList[[canHandle]] = c(handlerList[[canHandle]], l$id)
    }
    aux = buildTuneSearchSpace(sslist, l, info.env, idRef)
    modelTuneParsets[[l$id]] = aux$tss
    learnerObjects = c(learnerObjects, list(aux$l))  # updated learner object with fixed hyperparameters
    idRef = aux$idRef
  }
  checkParamIds(idRef)
  
  if (length(learnerObjects) == 0) {
    warning("No model fits the given task, returning NULL.")
    return(NULL)
  }

  multiplexer = removeHyperPars(makeModelMultiplexer(learnerObjects), "selected.learner")
  
  
  # TODO: it remains to be seen whether the following is necessary and / or a good thing to do.
  #  - in favour: maybe the learner objects don't remember the fixed hyperparameters that are given to them.
  #  - against: maybe there will be complaints when a hyperparameter is set for a learner that isn't active b/c of requirements.
  for (l in learnerObjects) {
    hs = getHyperPars(l)
    vals = list()
    for (hname in names(hs)) {
      vals[[paste(l$id, hname, sep=".")]] = hs[[hname]]
    }
    multiplexer = setHyperPars(multiplexer, par.vals=vals)
  }
  tuneParamSet = makeModelMultiplexerParamSetEx(multiplexer, modelTuneParsets)
  multiplexer$searchspace = tuneParamSet
  allLearners = unlist(tuneParamSet$pars$selected.learner$values)
  makeAMExoWrapper(multiplexer, wrapperList, taskdesc, idRef, handlerList, allLearners)
}

checkParamIds = function(idRef) {
  # check that the IDs match.
  for (parid in names(idRef)) {
    if (length(idRef[[parid]]) == 1) {
      warningf("Parameter '%s' of learner '%s' is the only one with parameter id '%s'.",
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
          stopf("Prameter '%s' of learner '%s' has the same id '%s' as param '%s' of learner '%s', but their '%s' property do not match. ('%s' vs. '%s')",
              protopar$id, idRef[[parid]][[1]]$learner$id, parid, otherpar$param$id, otherpar$learner$id, property,
              if (is.null(prop1)) "NULL" else prop1, if (is.null(prop2)) "NULL" else prop2)
        }
      }
    }
  }
}

buildTuneSearchSpace = function(sslist, l, info.env, idRef) {
  lp = getParamSet(l)
  lpids = getParamIds(lp)
  lptypes = getParamTypes(lp)
  names(lptypes) = lpids
  allParams = extractSubList(sslist, "name")
  untouchedParams = setdiff(lpids, allParams)
  if (length(untouchedParams)) {
    warningf("Learner '%s' has parameters %s that are not mentioned in search space.",
        l$id, paste(untouchedParams, collapse=", "))
  }
  tuneSearchSpace = list()
  canNotBeAMLRFIX = character(0)
  areAlreadyAMLRFIX = character(0)
  for (param in sslist) {
    if (param$type == "bool") {
      # useful since now we can automatically check for feasibility by checking whether everything in param$values is feasible.
      param$values = c(TRUE, FALSE)
    }
    origParamName = amlrTransformName(param$name)
    if (param$name != origParamName) {
      if (is.null(param$req)) {
        stopf("Parameter '%s' for learner '%s' has an .AMLRFIX suffix but no requirements",
            param$name, l$id)
      }
      if (param$type %in% c("fix", "def")) {
        stopf("Parameter '%s' for learner '%s' is of type '%s' but has an .AMLRFIX suffix.",
            param$name, l$id, param$type)
      }
      if (identical(param$special, "dummy")) {
        stopf("Parameter '%s' for learner '%s' has an .AMLRFIX suffix but is also a DUMMY parameter.",
            param$name, l$id, param$type)
      }
      if (origParamName %in% canNotBeAMLRFIX) {
        stopf("Parameter '%s' for learner '%s' cannot have an .AMLRFIX suffix. Possible Reasons:
 another parameter with that name has type 'def' or 'fix', has no requirements or is a 'dummy'.",
            param$name, l$id)
      }
      areAlreadyAMLRFIX = union(areAlreadyAMLRFIX, origParamName)
    }
    if (is.null(param$req) || identical(param$special, "dummy") || param$type %in% c("def", "fix")) {
      canNotBeAMLRFIX = union(canNotBeAMLRFIX, origParamName)
    }
    # inject --> is not in the learner already unless this is an AMLRPREFIX case
    if (identical(param$special, "inject") && (origParamName %nin% areAlreadyAMLRFIX)) {
      if (origParamName %in% getParamIds(getParamSet(l))) {
        stopf("Parameter '%s' is present in learner '%s' but is marked as `inject` in search space.",
            param$name, l$id)
      }
      l$par.set = c(l$par.set, makeParamSet(createParameter(param, info.env, learnerid=l$id, do.trafo=FALSE, facingOutside=FALSE)))
      lp = getParamSet(l)
      lpids = getParamIds(lp)
      lptypes = getParamTypes(lp)  # recreate lptypes here, since it may have changed 
      names(lptypes) = lpids
    } else if (identical(param$special, "dummy")) {
      if (origParamName %in% getParamIds(getParamSet(l))) {
        stopf("Parameter '%s' is present in learner '%s' but is marked as `dummy` in search space.",
            param$name, l$id)
      }
    } else {
      if (origParamName %nin% lpids) {
        stopf("Parameter '%s' as listed in search space is not available for learner '%s'.",
            param$name, l$id)
      }
      if (!allfeasible(lp, param$values, origParamName, param$dim)) {
        # there is one 'special case': param$values might be names that index into lp$pars[[param$name]]$values.
        vals = getValues(lp)[[origParamName]]
        if (isSubset(param$values, names(vals))) {
          param$values = vals[param$values]
          assert(allfeasible(lp, param$values, origParamName, param$dim))
        } else {
          stopf("Parameter '%s' as listed in search space has infeasible bounds '%s' for learner '%s'.",
            param$name, paste(param$values, collapse="', '"), l$id)
        }
      }
      partype = lptypes[[origParamName]]
      if ((partype %nin% c("numeric", "numericvector", "untyped")) &&
          (param$type == "real" || (param$type == "int" && partype %nin% c("integer", "integervector")))) {
        stopf("Parameter '%s' as listed in search space has wrong type '%s' for learner '%s'",
            param$name, param$type, l$id)
      }
      if ((partype != "untyped") &&
          ((param$type == "int" && partype %nin% c("integer", "integervector")) ||
           (param$type == "bool" && partype %nin% c("logical", "logicalvector")) ||
           (param$type == "cat" &&
             partype %nin% c("discrete", "discretevector", "character", "charactervector")))) {
        warningf("Parameter '%s' for learner '%s' is of type '%s' and has different (but feasible) type '%s' listed in search space.",
            param$name, l$id, partype, param$type)
      }
    }
    if (param$type == "def") {
      # check whether this is /actually/ the default
      truedefault = getDefaults(getParamSet(l))[[origParamName]]
      defaultcandidate = getHyperPars(l)[[origParamName]]
      if (!is.null(defaultcandidate)) {
        # we try to use the default, but apparently the value is already set in the learner object.
        if (is.null(truedefault) || truedefault != defaultcandidate) {
          truedefault = 
          warningf("Parameter '%s' for learner '%s' is of type 'default', but the learner has it already set to '%s'.",
              param$name, l$id, defaultcandidate)
          truedefault = defaultcandidate
        }
      }
      if (is.null(truedefault) != is.null(param$values) ||
          (!is.null(truedefault) && truedefault != param$values)) {
        warningf("Parameter '%s' for learner '%s' is of type 'default' but its alleged default '%s' differs from the true default '%s'.",
            param$name, l$id,
            if (is.null(param$values)) "NULL" else param$values,
            if (is.null(truedefault)) "NULL" else truedefault)
        param$type = "fix"
      }
    }
    if (is.null(param$special) && param$type !="def" && hasRequires(lp$pars[[origParamName]]) && is.null(param$req)) {
      warningf("Parameter '%s' for learner '%s' has a 'requires' argument but the one given in the search space has not.",
          param$name, l$id)
    }
    
    if (param$type == "fix") {
      if (lptypes[[origParamName]] == "discretevector") {
        assignment = list(rep(list(param$values), param$dim))
      } else {
        assignment = list(if (param$dim > 1) rep(param$values, param$dim) else param$values)
      }

      names(assignment) = origParamName
      l = setHyperPars(l, par.vals=assignment)
    } else {
      if (origParamName %in% names(getHyperPars(l))) {  # make sure this is not set at a default.
        l = removeHyperPars(l, param$name)
      }
      if (param$type != "def") {  # variable parameter
        newparam = createParameter(param, info.env, learnerid=l$id)
        if (!is.null(param$id)) {
          idRef[[param$id]] = c(idRef[[param$id]], list(list(learner=l, param=newparam)))
        }
        newparam$amlr.isDummy = identical(param$special, "dummy")
        tuneSearchSpace = c(tuneSearchSpace, list(newparam))
      }
    }
  }
  list(tss=makeParamSet(params=tuneSearchSpace), l=l, idRef=idRef)
}

allfeasible = function(ps, totest, name, dimension) {
  testlist = list(0)
  names(testlist) = name
  for (t in totest) {
    testlist[[1]] = if (dimension > 1) rep(t, dimension) else t
    if (!isFeasible(ps$pars[[name]], testlist[[1]])) {  # this would be easier if there was a way to disable requirements checking.
      return(FALSE)
    }
  }
  TRUE
}

createParameter = function(param, info.env, learnerid, do.trafo=TRUE, facingOutside=TRUE) {
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
        # if the transformation is not our own 'exp' trafo, then we can't say for sure what the real bounds are, or even what the param type is.
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
  if (param$dim > 1) {
    if (!facingOutside) {
      constructor = switch(param$type,
          real=makeNumericVectorLearnerParam,
          int=makeIntegerVectorLearnerParam,
          cat=makeDiscreteVectorLearnerParam,
          bool=makeLogicalVectorLearnerParam,
          fix=makeDiscreteVectorLearnerParam,
          NULL)
    } else {
      constructor = switch(param$type,
          real=makeNumericVectorParam,
          int=makeIntegerVectorParam,
          cat=makeDiscreteVectorParam,
          bool=makeLogicalVectorParam,
          fix=makeDiscreteVectorParam,
          NULL)
    }
    paramlist = list(id=param$name, len=param$dim, requires=param$req)
  } else {
    if (!facingOutside) {
      constructor = switch(param$type,
          real=makeNumericLearnerParam,
          int=makeIntegerLearnerParam,
          cat=makeDiscreteLearnerParam,
          bool=makeLogicalLearnerParam,
          fix=makeDiscreteLearnerParam,
          NULL)
    } else {
      constructor = switch(param$type,
              real=makeNumericParam,
              int=makeIntegerParam,
              cat=makeDiscreteParam,
              bool=makeLogicalParam,
              fix=makeDiscreteParam,
              NULL)
    }
    paramlist = list(id=param$name, requires=param$req)
  }
  paramlist = c(paramlist, switch(param$type,
      int=list(lower=pmin, upper=pmax, trafo=ptrafo),
      real=list(lower=pmin, upper=pmax, trafo=ptrafo),
      cat=list(values={x = param$values; names(x) = param$values; x}),
      bool=list(),
      fix={
        warningf("Parameter '%s' for learner '%s is marked dummy and has type 'fix'; This usually does not make sense.",
            param$name, learnerid)
        list(values=param$values)
      },
      def=stopf("Parameter '%s' for learner '%s' is marked as dummy or inject must not have type 'def'.",
          param$name, learnerid),
      stopf("Unknown type '%s'; parameter '%s', learner '%s'", param$type, param$name, learnerid)))
  pobject = do.call(constructor, paramlist, quote=TRUE)
  if (!is.null(pobject$trafo)) {
    environment(pobject$trafo) = list2env(as.list(environment(pobject$trafo), all.names=TRUE), parent=info.env)
    if (identical(param$trafo, "exp")) {
      pobject$amlr.origValues = param$values
    }
  }
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
    sequence = unique(c(addzero, round(min * ratio ^ (seq(from=0, to=floor(log(max/min, base=ratio))))), max))
    return(list(trafo=function(x) sequence[x], newmin=1, newmax=length(sequence)))
  } else {
    #  !!! We have to evaluate 'min' and 'max' here, otherwise they stay in the environment as 'promise' objects
    # and weird stuff happens! 
    assert(min > 0)
    assert(max > 0)

    return(list(trafo=function(x) min * (max / min)^x, newmin=0, newmax=1))
  }
}

#' Like mlr's mMMPS but respecting requirements.
#'
#' @param multiplexer the model multiplexer to use to create the ParamSet
#' @param modelParsets the list of param sets that are used to create the mmps.
makeModelMultiplexerParamSetEx = function(multiplexer, modelParsets) {
  searchspace = do.call(makeModelMultiplexerParamSet, c(list(multiplexer), modelParsets, .check=FALSE))
  # now we need to deal with the bug that makeModelMultiplexer overrides requirements
  for (modeliter in seq_along(modelParsets)) {
    origpars = modelParsets[[modeliter]]$pars
    modelid = names(modelParsets)[modeliter]
    oldnames = names(origpars)  # the names as in the original model
    newnames = paste(modelid, oldnames, sep='.')  # the name that was assigned in mm$searchspace
    substitution = lapply(newnames, asQuoted)  # substitution is a list(oldname=quote(newname))
    names(substitution) = oldnames
    for (paramiter in seq_along(origpars)) {  # iterate over the parameters in each ParamSet
      searchspace$pars[[newname]]$amlr.learnerName = modelid
      cp = origpars[[paramiter]]
      cpname = names(origpars)[paramiter]
      cprequires = cp$requires
      newname = paste(modelid, cpname, sep='.') # the name fo the current Param within mm$searchspace
      newrequires = searchspace$pars[[newname]]$requires
      if (is.null(cprequires)) {
        # if there is no '$requires' we don't need to do anything
        next
      }
      cprequires = replaceRequires(cprequires, substitution)
      newrequires = substitute((a) && eval(b), list(a=newrequires, b=cprequires))
      # at this position, newrequires has the form
      # (new requires) && (old requires)
      # where the use of short-cirquiting && should solve any problems that we might get when querying isFeasible.
      searchspace$pars[[newname]]$requires = newrequires
    }
  }
  searchspace
}

replaceRequires = function(cprequires, substitution) {
  # what we are going to do is substitute the variable names with their new prefixed versions.
  # HOWEVER: R uses different scoping for function calls than for variables. therefore e.g.
  # > c <- 1
  # > c(c, c)
  # doesn't give an error. This is a pain when trying to do what I'm doing here. So we will
  # manually substitute all function calls with different names.
  #
  # the width.cutoff may be a problem? I wouldn't assume so if deparse keeps function name and opening parenthesis on the same line.
  parsed = deparse(as.expression(cprequires), control=c("keepInteger", "keepNA"), width.cutoff=500)
  funcallmatch = "(?:((?:[[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|(`)((?:[^`\\\\]|\\\\.)+`))(\\()"
  
  parsed = gsub(funcallmatch, "\\2.AUTOMLR_TEMP_\\1\\3\\4", parsed)
  #the following would be dumb: parsed[1] = sub(".AUTOMLR_TEMP_expression(", "expression(", parsed[1], fixed=TRUE) # NO!
  cprequires = asQuoted(parsed)
  # the following line is a bit of R magic. Use do.call, so that cprequires, which is a
  # 'quote' object, is expanded to its actual content. The 'substitute' call will change all
  # names of the old parameters to the new parameters.
  cprequires = do.call(substitute, list(cprequires, substitution))
  
  funcallmatchReverse = "(?:\\.AUTOMLR_TEMP_((?:[[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|(`)\\.AUTOMLR_TEMP_((?:[^`\\\\]|\\\\.)+`))(\\()"
  parsed = gsub(funcallmatchReverse, "\\2\\1\\3\\4", deparse(cprequires, control=c("keepInteger", "keepNA"), width.cutoff=500))
  eval(asQuoted(parsed))
}