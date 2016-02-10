


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
  wrapperSelectParam = makeDiscreteParam("automlr.wrappersetup", listWrapperCombinations(
          names(wrappers), extractSubList(wrappers, "required")))
  # wrappersetup has the format outermostWrapper$wrapper...$wrapper$innermostwrapper.
  # step 0: introduce the outside parameters that control this.
  allDelendum = c("missings", "factors", "ordered")
  deleters = list()
  newparams = list()
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
                which(unlist(strsplit(automlr.wrappersetup)) == thisWrapper) <= which(unlist(strsplit(automlr.wrappersetup)) == removingWrapper)),
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
        req = substitute((thisWrapper %in% unlist(strsplit(automlr.wrappersetup))) && eval(req),
            list(thisWrapper=w, restReq=req))
      }
      wrappers[[w]]$searchspace$pars[[parname]]$requires = req
    }
  }
  
  completeSearchSpace = do.call(base::c, extractSubList(wrappers, "searchspace"))
  completeSearchSpace = c(completeSearchSpace, makeParamSet(params=newparams), modelmultiplexer$searchspace)
  
  # what's missing is removing the singleton parameters and replacing them with direct setting of parameter values internally
  
  staticParams = list()
  substitutions = list()
  for (param in getParamIds(completeSearchSpace)) {
    curpar = completeSearchSpace$pars[[param]]
    if ((curpar$type == "discrete" && length(curpar$values) == 1) ||  # this is a 'fixed' value
        (curpar$type %in% c("numeric", "integer") && curpar$values[1] == curpar$values[2])) {  # singular valid region
      completeSearchSpace$pars[[param]] = NULL
      parid = sub("\\.AMLRFIX[0-9]+$", "", curpar$id)
      if (!is.null(curpar$requires)) {
        subst = substitute((if (eval(a)) value else original), list(a=as.expression(curpar$requires), value=curpar$values[1], original=asQuoted(parid)))
      } else {
        subst = curpar$values[1]
      }
      staticParams = c(staticParams, list(id=parid, value=curpar$values[1], requires=curpar$requires))
      substitutions[parid] = subst
    }
  }
  
  for (i in seq_len(3)) {  # go 3 steps deep...
    for (param in getParamIds(completeSearchSpace)) {
      if (!is.null(comleteSearchSpace$pars[[param]]$requires)) {
        comleteSearchSpace$pars[[param]]$requires = replaceRequires(comleteSearchSpace$pars[[param]]$requires, substitutions)
      }
    }
    for (paridx in seq_along(staticParams)) {
      if (!is.null(staticParams[[paridx]]$requires)) {
        staticParams[[paridx]]$requires = replaceRequires(staticParams[[paridx]]$requires, substitutions)
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
      par.set=completeSearchSpace,
      par.vals=getHyperPars(modelmultiplexer),
      package=character(0))
  learner$staticParams = staticParams
  learner
}

#' @export
trainLearner.AMExoWrapper = function(.learner, .task, .subset, .weights = NULL, ...) {
  # train selected learner model and remove prefix from its param settings

  train(learner, task = .task, subset = .subset, weights = .weights)
}

#' @export
predictLearner.AMExoWrapper = function(.learner, .model, .newdata, ...) {

  predictLearner(learner, .model$learner.model$next.model, .newdata)
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