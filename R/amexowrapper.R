


#' @param modelmultiplexer a modelmultiplexer object that should have a $searchspace element
#' @param wrappers a named list of wrappers that have a $required element;
#'        names must not contain $-character. Also: $conversion, $searchspace, $constructor
makeAMExoWrapper = function(modelmultiplexer, wrappers, taskdesc, idRef, canHandleX, allLearners) {
  # So what are we doing here?
  # The AMExoWrapper mostly handles parameter space magic. Specifically, this is:
  #  - introduce a parameter that chooses which wrapper is used, and in which sequence
  #  - introduce parameters that control whether the wrapper(s) are used to facilitate compatibility of
  #    learners with the data, i.e. remove missing values, remove or convert data types that are not supportet
  #  - set some special variable values that the `requires`-expressions of other parameters can use to specify
  #    that some parameters are only relevant in the presence of certain data types.
  #  - remove parameters that always take on a single value (maybe dependent on a `requires` from the search space
  #    and set them internally before calling the wrapped model.
  #    * bonus: if the fixed parameter name contains a suffix of .AMLRFIX#, where # is a number, it will be stripped
  #      from the parameter name. This way it is possible to define alternative "defaults" for a parameter in case of
  #      certain requirements being given.
  #
  # The parameters that are introduced and exposed to the outside are:
  #  - automlr.wrappersetup: which wrapper is used. It has the format outermostWrapper$wrapper...$wrapper$innermostwrapper.
  #  - automlr.remove.XXX where XXX is one of missings, factors, ordered. It controls whether one of the wrapper will
  #    be set up to remove the property in question even if the underlying learner is able to use the data type. 
  #    * NOTE: this may or may not be sensible for the data type in question and it is possible to set the respective value
  #      to FALSE via setHyperPars() & exclude it from the search space. Who is AMExoWrapper to decide?
  #    * NOTE2: Only wrappers marked as "requiredwrapper" can respond to this; This is because otherwise the search
  #      space gets too confusing.
  #  - automlr.wremoving.XXX where XXX is one of missings, factors, ordered. If more than one wrapper is present with the
  #    capability of removing XXX from the data, this parameter chooses which one wrapper is responsible for removing it.
  #
  # The following parameters can be used by wrappers and learners in their $requires-parameter; they will be replaced here:
  #  - automlr.remove.XXX: Only used by wrappers, may only be used if the respective $conversion() call with "XXX" as parameter
  #    returns a vector containing at least "numerics" or "". It indicates that this wrapper is responsible for removing
  #    the XXX type. If XXX does not occurr in the data, autmlr.remove.XXX is always FALSE.
  #  - automlr.has.XXX: May be used by all wrappers and all learners: Indicates that the XXX type is present in the data.
  #    Care is taken e.g. that when wrapperA comes before wrapperB which comes before wrapperC, and wrapperB removes missings, that inside
  #    wrapperA (and wrapperB) the value of automlr.has.missings is TRUE, but for wrapperC (and all the learners) automlr.has.missings
  #    evaluates to FALSE.
  
  covtypes = c(names(taskdesc$n.feat)[taskdesc$n.feat > 0], if (taskdesc$has.missings) "missings")

  completeSearchSpace = c(
      buildSearchSpace(wrappers, covtypes, canHandleX, allLearners),
      modelmultiplexer$searchspace)
  
  # what's missing is removing the singleton parameters from the search space and replacing them with 
  #   direct setting of parameter values internally.
  aux = extractStaticParams(completeSearchSpace)
  staticParams = aux$staticParams
  substitutions = aux$substitutions
  finalSubstitutions = aux$finalSubstitutions
  
  # replace the singleton values inside the requirements of other parameters.
  completeSearchSpace$pars = substituteParamList(completeSearchSpace$pars, substitutions, 3)
  completeSearchSpace$pars = substituteParamList(completeSearchSpace$pars, finalSubstitutions)
  staticParams = substituteParamList(staticParams, substitutions, 3)
  staticParams = substituteParamList(staticParams, finalSubstitutions)
  
  # transform into "LearnerParam" types. This is mostly dumb relabeling, except for one thing: The
  # limits / values of the parameters with "trafo" have to be reverse-transformed.
  learnerPars = makeLearnerPars(completeSearchSpace)
  
  # finally, create the learner object that will be returned!
  constructor = switch(taskdesc$type,
      classif=makeRLearnerClassif, regr=makeRLearnerRegr, surv=makeRLearnerSurv,
      multilabel=makeRLearnerMultilabel, stopf("Task type '%s' not supported.", taskdesc$type))
  learner = constructor(
      cl="AMExoWrapper",
      short.name="amlr",
      name="automlrlearner",
      properties=properties,
      par.set=learnerPars,
      par.vals=getHyperPars(modelmultiplexer),
      package="automlr")

  learner$learner = modelmultiplexer
  learner$staticParams = staticParams
  learner$fixedParams = getHyperPars(modelmultiplexer)
  learner$searchspace = completeSearchSpace
  learner$fix.factors.prediction = TRUE  # TODO: it seems like it is a bug that this doesn't happen automatically.
  learner$wrappers = extractSubList(wrappers, "constructor")
  learner
}

#' @export
trainLearner.AMExoWrapper = function(.learner, .task, .subset, .weights = NULL, automlr.wrappersetup, ...) {
  # train selected learner model and remove prefix from its param settings
  learner = .learner$learner
  if (length(.learner$wrappers) > 0) {
    if (length(.learner$wrappers) == 1) {  # in this case automlr.wrappersetup will be *missing*.
      automlr.wrappersetup = names(.learner$wrappers)
    }
    for (w in rev(unlist(strsplit(automlr.wrappersetup, "$")))) {
      learner = .learner$wrappers[[w]](learner)
    }
  }
  setupLearnerParams(learner, .learner$staticParams, list(...))
  train(learner, task = .task, subset = .subset, weights = .weights)
}

#' @export
predictLearner.AMExoWrapper = function(.learner, .model, .newdata, ...) {
  # we can't just call predictLearner() here, unless we also wrap the whole setHyperPars machinery, for which we would also need to 
  # be more diligent setting the LearnerParam$when = train / test value.
  getPredictionResponse(predict(.model$learner.model, newdata=.newdata))  # the learner.model we are given is just an mlr WrappedModel that we can use predict on.
}

setupLearnerParams = function(learner, staticParams, params) {
  learner = removeHyperPars(learner, names(getHyperPars(learner)))
  # learner = setHyperPars(learner, par.vals=dotLearner$fixedParams)  # TODO: do we need to do this?
  learner = setHyperPars(learner, par.vals=params)
  envir = getHyperPars(learner)
  extraParams = list()
  for (fp in staticParams) {
    if (!is.null(fp$requires) && isTRUE(eval(fp$requires, envir=envir))) {
      extraParams[[fp$id]] = fp$value
      if (fp$id %in% names(envir)) {
        stopf("Parameter '%s' is a static (internal) parameter but was also given externally.",
            fp$id)
      }
    }
  }
  setHyperPars(learner, par.vals=extraParams)
}

buildSearchSpace = function(wrappers, properties, canHandleX, allLearners) {
  # Introduce `automlr.wrappersetup` (in case there are any wrappers present at all).
  newparams = list()
  if (length(wrappers)) {
    wrapperSelectParam = makeDiscreteParam("automlr.wrappersetup", listWrapperCombinations(
            names(wrappers), unlist(extractSubList(wrappers, "required"))))
    newparams = list(wrapperSelectParam)
  }
  
  # Introduce other external parameters: automlr.remove.XXX and automlr.wremoving.XXX
  allTypes = c("missings", "factors", "ordered", "numerics")
  removers = list()  # maps type -> all wrappers that are able to remove the type
  for (type in allTypes) {
    if (type %nin% properties) {
      next
    }
    for (w in names(wrappers)[extractSubList(wrappers, "required")]) {
      if (any(c("numerics", "") %in% wrappers[[w]]$conversion(type))) {
        # the wrapper can delete the covariate in question
        removers[[type]] = c(removers[[type]], w)
      }
    }
    if (length(removers[[type]]) == 0) {
      next  # we can't remove the type at all
    }
    
    # if there is at least one remover, we introduce the external parameter telling whether to remove.
    # The requirement is that the active learner actually leaves a choice -- if it cannot handle the type
    #   to begin with, then the wrapper is obligated to remove the type anyways.
    amlrRemoveName = paste0("automlr.remove.", type)
    if (length(canHandleX[[type]]) > 0) {
      if (setequal(allLearners, canHandleX[[type]])) {
        requires = NULL  # if all learners can handle the type, the variable is always valid.
      } else {
        requires = substitute(selected.learner %in% x, list(x=canHandleX[[type]]))
      }
      newparams = c(newparams, list(makeLogicalParam(amlrRemoveName, req=requires)))
    }
      
    # if there are at least two removers, we need to introduce another external parameter telling which
    # wrapper should do the removing.
    if (setequal(allLearners, canHandleX[[type]])) {
      requires = asQuoted(paste(amlrRemoveName, "== TRUE"))  # need to do the silly ==TRUE thing bc the result isn't "call" class otherwise
    } else if (length(canHandleX[[type]] == 0)) {
      requires = NULL
    } else {
      requires = substitute(selected.learner %nin% x || amlrRemove,
          list(x=canHandleX[[type]],
              amlrRemove=asQuoted(amlrRemoveName)))
    }
    removingWrapperName = paste0("automlr.wremoving.", type)
    newparams = c(newparams, list(makeDiscreteParam(removingWrapperName, removers[[type]], req=requires)))
  }
  
  # Manipulate the wrapper's search space
  #  - substitute the pseudo variables (automlr.has.xxx, automlr.remove.xxx) with expressions
  #  - add the requirement that the wrapper is actually present
  for (w in names(wrappers)) {
    for (parname in getParamIds(wrappers[[w]]$searchspace)) {
      req = wrappers[[w]]$searchspace$pars[[parname]]$requires
      if (is.null(req)) {
        next
      }
      replaceList = list()
      for (type in allTypes) {
        if (type %nin% properties) {
          replaceList[[paste0("automlr.has.", type)]] = FALSE
          replaceList[[paste0("automlr.remove.", type)]] = FALSE
          next
        }
        if (length(removers[[type]]) == 0)  {# can not be deleted, therefore always present
          replaceList[[paste0("automlr.has.", type)]] = TRUE
          # by given constraint, no automlr.remove.XXX is present and therefore doesn't need to be replaced.
          next
        }
        amlrRemoveQuote = asQuoted(paste0("automlr.remove.", type))
        amlrWRemovingQuote = asQuoted(paste0("automlr.wremoving.", type))
          
        # ** substituting automlr.has.xxx here
        # when is 'autmlr.has.XXX' true for a given wrapper?
        #  - either the given learner can handle xxx AND xxx is not requested removed by external variable
        typeStaysPresentQuote = substitute(selected.learner %in% canHandleDelendum && !amlrRemove,
            list(canHandleDelendum=canHandleX[[type]],
                amlrRemove=amlrRemoveQuote))
        #  - OR this wrapper comes before the wrapper (or is the wrapper) that removes xxx.
        typeRemovedAfterwardQuote = substitute(
            which(unlist(strsplit(automlr.wrappersetup, "$")) == thisWrapper) <= which(unlist(strsplit(automlr.wrappersetup, "$")) == removingWrapper),
            list(thisWrapper=w, removingWrapper=amlrWRemovingQuote))
        replaceQuote = substitute(a || b, list(a=typeStaysPresentQuote, b=typeRemovedAfterwardQuote))
        replaceList[[paste0("automlr.has.", type)]] = replaceQuote
          
        # ** substituting automlr.remove.xxx here
        if (w %in% removers[[type]]) {  # only then this wrapper is allowed to use 'automlr.remove.xxx'
          replaceQuote = substitute(!typeStaysPresent && wremoving == ownName,
              list(typeStaysPresent=typeStaysPresentQuote, wremoving=amlrWRemovingQuote, ownName=w))
          replaceList[[paste0("automlr.remove.", type)]] = replaceQuote
        }
      }
        
      req = replaceRequires(req, replaceList)
      if (!wrappers[[w]]$required) {
        # For the wrappers that are not always present: need to add "wrapper is actually used" as a requirement.
        req = substitute((thisWrapper %in% unlist(strsplit(automlr.wrappersetup, "$"))) && eval(req),
            list(thisWrapper=w, restReq=req))
      }
      wrappers[[w]]$searchspace$pars[[parname]]$requires = req
    }
  }
  
  # combine all the ParamSets we have seen now
  c(makeParamSet(params=do.call(base::c, extractSubList(wrappers, "searchspace"))),
      makeParamSet(params=newparams))
}

listWrapperCombinations = function(ids, required) {
  combineNames = function(x) {
    if (all(requiredIDs %in% x) && all(!duplicated(x))) {
      paste(x, collapse="$")
    }
  }
  requiredIDs = ids[required]
  result = sapply(seq_along(ids), function(l) {
        apply(expand.grid(rep(list(ids), l)), 1, combineNames)
      })
  unlist(result)
}

substituteParamList = function(paramList, substitutions, cycles=1) {
  for (i in seq_len(cycles)) { # go `cycles` steps deep, in case one of the substituted variables itself requires another variable.
    for (pid in seq_along(paramList)) {
      req = paramList[[pid]]$requires
      if (!is.null(req)) {
        paramList[[pid]]$requires = replaceRequires(req, substitutions)
      }
    }
  }
  paramList
}

makeLearnerPars = function(learnerPars) {
  for (p in getParamIds(learnerPars)) {
    if (!is.null(learnerPars$pars[[p]]$trafo) &&  # there is a trafo --> need to change limits
        learnerPars$pars[[p]]$type %in% c("numeric", "numericvector", "integer", "integervector")) {
      if (is.null(learnerPars$pars[[p]]$origValues)) {
        learnerPars$pars[[p]]$lower = -Inf
        learnerPars$pars[[p]]$upper = Inf
      } else {
        learnerPars$pars[[p]]$lower = learnerPars$pars[[p]]$origValues[1]
        learnerPars$pars[[p]]$upper = learnerPars$pars[[p]]$origValues[2]
      }
      # convert type to "numeric(vector)", since after trafo we are not sure it is still an int
      learnerPars$pars[[p]]$type = switch(learnerPars$pars[[p]]$type,
          integer="numeric",
          integervector="numericvector",
          learnerPars$pars[[p]]$type)
    }
    learnerPars$pars[[p]]$trafo = NULL
    # as the things stand now we don't wrap setHyperPars and therefore all hyperpars need to be
    # given to the train function. If this ever changes (and we e.g. call predictLearner() instead
    # of predict(), and we wrap setHyperPars() also) we need to copy the $when property of the
    # corresponding LearnerParam inside the modelmultiplexer object.
    learnerPars$pars[[p]]$when = "train"
    class(learnerPars$pars[[p]]) = c("LearnerParam", class(learnerPars$pars[[p]]))
    # satisfying a weird constraint of mlr:
    req = learnerPars$pars[[p]]$requires
    if (!is.null(req) && is.expression(req)) {
      if (length(req) == 1) {
        learnerPars$pars[[p]]$requires = req[[1]]
      } else {
        learnerPars$pars[[p]]$requires = substitute(eval(x), list(x=req))
      } 
    }
  }
}

extractStaticParams = function(completeSearchSpace) {
  staticParams = list()  # all parameters that have only a single value
  substitutions = list()  # substitution that will be used instead of the param inside of other parameter's $requires.
  finalSubstitutions = list()
  for (param in getParamIds(completeSearchSpace)) {
    curpar = completeSearchSpace$pars[[param]]
    if ((curpar$type == "discrete" && length(curpar$values) == 1) ||  # this is a 'fixed' value
        (curpar$type %in% c("numeric", "integer") && curpar$lower == curpar$upper)) {  # valid interval is a point
      fixvalue = ifelse(curpar$type == "discrete", curpar$values[[1]], curpar$lower)
      completeSearchSpace$pars[[param]] = NULL
      parid = sub("\\.AMLRFIX[0-9]+$", "", curpar$id)
      if (!is.null(curpar$requires)) {
        # the following is a bit unfortunate, because it introduces a kind of recursive dependence. I don't see
        # a better way, however. The problem is that if we have a variable xyz, and a variable xyz.AMLRFIX1, then
        # we want to remove the xyz.AMLRFIX1 and replace it with its fixed value given the requirement. However,
        # if the requirement is not given, the parameter space given value must be used.
        # SOLUTION: append a suffix that prevents cycling in on itself.
        original = paste0(parid, ".AMLRFINAL")
        subst = substitute(if (eval(req)) value else original, list(req=as.expression(curpar$requires), value=fixvalue, original=asQuoted(original)))
        finalSubstitutions[[original]] = parid
      } else {
        subst = fixvalue
      }
      # staticParams is not a named list, because the same parid may occur multiple times (after stripping .AMLRFIX#)
      staticParams = c(staticParams, list(list(id=parid, value=fixvalue, requires=curpar$requires)))
      if (parid %in% names(substitutions)) {
        # yay, we already have this substitution. This is only allowed to happen if there are exclusive reuqirements,
        # so we are able to substitute the substitutions inside each other.
        assert(!is.null(curpar$requires))
        sl = list()
        sl[[original]] = substitutions[[parid]]
        substitutions[[parid]] = substitute(subst, sl)  # yo dawg, I heard you like substitutions...
      } else {
        substitutions[[parid]] = subst
      }
    }
  }
  list(staticParams=staticParams, substitutions=substitutions, finalSubstitutions=finalSubstitutions)
}