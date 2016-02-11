


#' @param modelmultiplexer a modelmultiplexer object that should have a $searchspace element
#' @param wrappers a named list of wrappers that have a $required element;
#'        names must not contain $-character. Also: $conversion, $searchspace, $constructor
makeAMExoWrapper = function(modelmultiplexer, wrappers, taskdesc, idRef, properties, canHandleX) {
  # Required parameters may have a dependency on special parameters:
  # automlr.remove.missings, automlr.remove.factors, automlr.remove.ordered
  # these are TRUE whenever the wrapper is expected to remove missings/factors/ordered.
  # automlr.remove.factors also removes ordered variables. because.
  # All wrappers may depend on
  # - automlr.has.missings, automlr.has.factors, autmlr.has.ordered
  wrapperSelectParam = if (length(wrappers)) makeDiscreteParam("automlr.wrappersetup", listWrapperCombinations(
            names(wrappers), extractSubList(wrappers, "required")))
  # wrappersetup has the format outermostWrapper$wrapper...$wrapper$innermostwrapper.
  # step 0: introduce the outside parameters that control this.
  allDelendum = c("missings", "factors", "ordered")
  deleters = list()
  newparams = c(list(), wrapperSelectParam)
  missingsVar = list(
      missings=quote(!automlr.has.missings),
      factors=quote(!automlr.has.factors),
      ordered=quote(!automlr.has.ordered))
  for (delendum in allDelendum) {
    if (delendum %nin% properties) {
      next
    }
    for (w in names(wrappers)[extractSubList(wrappers, "required")]) {
      if (any(c("numerics", "") %in% wrappers[[w]]$conversion(delendum))) {
        # the wrapper can delete the covariate in question
        deleters[[delendum]] = c(deleters[[delendum]], w)
      }
    }
    if (length(deleters[[delendum]]) == 0) {
      next  # we can't do anything here
    }
    amlrRemoveName = paste0("automlr.remove.", delendum)
    newparams = c(newparams, list(
            makeLogicalParam(amlrRemoveName,
                req=substitute((selected.learner %in% canHandleDelendum),
                    list(canHandleDelendum=canHandleX[[delendum]])))))  # it's almost lisp
    if (length(deleters[[delendum]]) == 1) {
      next  # we don't need another 'choosing' parameter.
    }
    newparams = c(newparams, list(
            makeDiscreteParam(paste0("automlr.wremoving.", delendum), deleters[[delendum]],
                req=substitute((selected.learner %nin% canHandleDelendum || amlrRemove),
                    list(canHandleDelendum=canHandleX[[delendum]],
                        amlrRemove=asQuoted(amlrRemoveName))))))
  }
  
  # step 1: replace the special variables with our complicated computation.
  for (w in names(wrappers)) {
    for (parname in getParamIds(wrappers[[w]]$searchspace)) {
      req = wrappers[[w]]$searchspace$pars[[parname]]$requires
      if (is.null(req)) {
        next
      }
      replaceList = list()
      for (delendum in allDelendum) {
        if (delendum %nin% properties) {
          next
        }
        if (length(deleters[[delendum]]) == 0)  {# can not be deleted, therefore always present
          replaceList[[paste0("automlr.has.", delendum)]] = TRUE
          next
        }
        amlrRemoveName = paste0("automlr.remove.", delendum)
        replaceQuote = substitute(((!(selected.learner %nin% canHandleDelendum || amlrRemove)) ||
                which(unlist(strsplit(automlr.wrappersetup, "$")) == thisWrapper) <= which(unlist(strsplit(automlr.wrappersetup, "$")) == removingWrapper)),
            list(canHandleDelendum=canHandleX[[delendum]],
                amlrRemove=asQuoted(amlrRemoveName),
                thisWrapper=w,
                removingWrapper=if(length(deleters[[delendum]]) > 1) asQuoted(paste0("automlr.wremoving.", delendum)) else deleters[[delendum]]))
        replaceList[[paste0("automlr.has.", delendum)]] = replaceQuote
        if (wrappers[[w]]$required) {  # this wrapper is allowed to use 'automlr.remove.xxx'
          if (w %in% deleters[[delendum]]) {
            replaceQuote = substitute((selected.learner %nin% canHandleDelendum || amlrRemove),
                list(canHandleDelendum=canHandleX[[delendum]],
                    amlrRemove=asQuoted(amlrRemoveName)))
            if (length(deleters[[delendum]]) > 1) {
              replaceQuote = substitute((simpleQuote && wremoving == wname),
                  list(simpleQuote=replaceQuote,
                      wremoving=asQuoted(paste0("automlr.wremoving.", delendum)),
                      wname=w))
            }
            replaceList[[paste0("automlr.remove.", delendum)]] = replaceQuote
          }
        }
      }
      print(replaceList)
      req = replaceRequires(req, replaceList)
      if (!wrappers[[w]]$required) {
        # Remember: also need to add "wrapper is actually used"
        req = substitute((thisWrapper %in% unlist(strsplit(automlr.wrappersetup, "$"))) && eval(req),
            list(thisWrapper=w, restReq=req))
      }
      wrappers[[w]]$searchspace$pars[[parname]]$requires = req
    }
  }
  
  completeSearchSpace = makeParamSet(params=do.call(base::c, extractSubList(wrappers, "searchspace")))
  completeSearchSpace = c(completeSearchSpace, makeParamSet(params=newparams), modelmultiplexer$searchspace)
  
  # what's missing is removing the singleton parameters and replacing them with direct setting of parameter values internally
  
  staticParams = list()
  substitutions = list()
  for (param in getParamIds(completeSearchSpace)) {
    curpar = completeSearchSpace$pars[[param]]
    if ((curpar$type == "discrete" && length(curpar$values) == 1) ||  # this is a 'fixed' value
        (curpar$type %in% c("numeric", "integer") && curpar$lower == curpar$upper)) {  # singular valid region
      fixvalue = ifelse(curpar$type == "discrete", curpar$values[[1]], curpar$lower)
      completeSearchSpace$pars[[param]] = NULL
      parid = sub("\\.AMLRFIX[0-9]+$", "", curpar$id)
      if (!is.null(curpar$requires)) {
        subst = substitute((if (eval(a)) value else original), list(a=as.expression(curpar$requires), value=curpar$values[[1]], original=asQuoted(parid)))
      } else {
        subst = fixvalue
      }
      staticParams = c(staticParams, list(list(id=parid, value=fixvalue, requires=curpar$requires)))
      substitutions[[parid]] = subst
    }
  }
  
  for (i in seq_len(3)) {  # go 3 steps deep...
    for (param in getParamIds(completeSearchSpace)) {
      if (!is.null(completeSearchSpace$pars[[param]]$requires)) {
        completeSearchSpace$pars[[param]]$requires = replaceRequires(completeSearchSpace$pars[[param]]$requires, substitutions)
      }
    }
    for (paridx in seq_along(staticParams)) {
      if (!is.null(staticParams[[paridx]]$requires)) {
        staticParams[[paridx]]$requires = replaceRequires(staticParams[[paridx]]$requires, substitutions)
      }
    }
  }
  learnerPars = completeSearchSpace
  for (p in getParamIds(learnerPars)) {
    if (!is.null(learnerPars$pars[[p]]$trafo) &&
        learnerPars$pars[[p]]$type %in% c("numeric", "numericvector", "integer", "integervector")) {
      if (is.null(learnerPars$pars[[p]]$origValues)) {
        learnerPars$pars[[p]]$lower = -Inf
        learnerPars$pars[[p]]$upper = Inf
      } else {
        learnerPars$pars[[p]]$lower = learnerPars$pars[[p]]$origValues[1]
        learnerPars$pars[[p]]$upper = learnerPars$pars[[p]]$origValues[2]
      }
      learnerPars$pars[[p]]$type = switch(learnerPars$pars[[p]]$type,
          integer="numeric",
          integervector="numericvector",
          learnerPars$pars[[p]]$type)
    }
    learnerPars$pars[[p]]$trafo = NULL
    # this is dumb but necessary:
    learnerPars$pars[[p]]$when = "train"
    class(learnerPars$pars[[p]]) = c("LearnerParam", class(learnerPars$pars[[p]]))
    # now this is very dumb and completely unnecessary:
    req = learnerPars$pars[[p]]$requires
    if (!is.null(req) && is.expression(req)) {
      if (length(req) == 1) {
        learnerPars$pars[[p]]$requires = req[[1]]
      } else {
        learnerPars$pars[[p]]$requires = substitute(eval(x), list(x=req))
      } 
    }
  }

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
  learner$searchspace = completeSearchSpace  # is of type 'param', not 'learnerparam', since some optimizers get bitchy otherwise
  learner$debug = list()
  learner$debug$mm = modelmultiplexer
  learner$fix.factors.prediction = TRUE  # TODO: it seems like it is a bug that this doesn't happen automatically.
  learner$wrappers = extractSubList(wrappers, "constructor")
  learner
}

#' @export
trainLearner.AMExoWrapper = function(.learner, .task, .subset, .weights = NULL, ...) {
  # train selected learner model and remove prefix from its param settings
  learner = .learner$learner
  if (length(.learner$wrappers) > 0) {
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