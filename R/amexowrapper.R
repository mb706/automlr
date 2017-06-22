

#' @title Create an mlr learner object that smoothes out the parameter space.
#'
#' @description
#' So what are we doing here?
#' The AMExoWrapper mostly handles parameter space magic. Specifically, this is:
#' \itemize{
#'   \item introduce a parameter that chooses which wrapper is used, and in
#'     which sequence
#'   \item introduce parameters that control whether the wrapper(s) are used to
#'     facilitate compatibility of learners with the data, i.e. remove missing
#'     values, remove or convert data types that are not supportet
#'   \item set some special variable values that the `requires`-expressions of
#'     other parameters can use to specify that some parameters are only
#'     relevant in the presence of certain data types.
#'   \item remove parameters that always take on a single value (maybe dependent
#'     on a `requires` from the search space and set them internally before
#'     calling the wrapped model.
#'   \itemize{
#'     \item bonus: if the fixed parameter name contains a suffix of .AMLRFIX#,
#'       where # is a number, it will be stripped from the parameter name. This
#'       way it is possible to define alternative "defaults" for a parameter in
#'       case of certain requirements being given.
#'   }
#' }
#'
#' The parameters that are introduced and exposed to the outside are:
#' \itemize{
#'   \item automlr.wrappersetup: which wrapper is used. It has the format
#'     \code{outermostWrapper$wrapper...$wrapper$innermostwrapper}.
#'   \item automlr.remove.XXX where XXX is one of missings, factors, ordered. It
#'     controls whether one of the wrapper will be set up to remove the property
#'     in question even if the underlying learner is able to use the data type.
#'    \itemize{
#'      \item NOTE: this may or may not be sensible for the data type in
#'        question and it is possible to set the respective value to FALSE via
#'        setHyperPars() & exclude it from the search space. Who is AMExoWrapper
#'        to decide?
#'      \item NOTE2: Only wrappers marked as "requiredwrapper" can respond to
#'        this; This is because otherwise the search space gets too confusing.
#'    }
#'   \item automlr.wremoving.XXX where XXX is one of missings, factors, ordered.
#'     If more than one wrapper is present with the capability of removing XXX
#'     from the data, this parameter chooses which one wrapper is responsible
#'     for removing it.
#' }
#'
#' The following parameters can be used by wrappers and learners in their
#' \code{$requires}-parameter; they will be replaced here:
#' \itemize{
#'   \item automlr.remove.XXX: Only used by wrappers, may only be used if the
#'     respective \code{$conversion} is present. It indicates that this
#'     wrapper is responsible for removing the XXX type. If XXX does not occurr
#'     in the data, autmlr.remove.XXX is always FALSE.
#'   \item automlr.has.XXX: May be used by all wrappers and all learners:
#'     Indicates that the XXX type is present in the data. Care is taken e.g.
#'     that when wrapperA comes before wrapperB which comes before wrapperC, and
#'     wrapperB removes missings, that inside wrapperA (and wrapperB) the value
#'     of automlr.has.missings is TRUE, but for wrapperC (and all the learners)
#'     automlr.has.missings evaluates to FALSE.
#'   \item automlr.may.have.XXX: May be used by all wrappers: Indicates that the
#'     XXX type may be generated from the data present at this stage. This is
#'     because the attached learner can handle the data, or because a later
#'     wrapper converts the data into something the learner can handle.
#' }
#' @param modelmultiplexer [\code{ModelMultiplexer}]\cr
#'   A modelmultiplexer object that should have a \code{$searchspace} slot.
#' @param wrappers [\code{list}]\cr
#'   A named list of wrappers that have a \code{$required} element; names must
#'   not contain \code{$}-character. Also: \code{$conversion},
#'   \code{$searchspace}, \code{$constructor}.
#' @param taskdesc [\code{TaskDesc}]\cr
#'   The taskDesc object of the task for which to build the learner.
#' @param canHandleX [\code{list}]\cr
#'   A named list that maps "missings", "factors", and "ordered" to a vector of
#'   learner names that can handle the respective data.
#' @param allLearners [\code{character}]\cr
#'   The list of all learner names.
#' 
#' @return [\code{AMExoWrapper}]
#' A \code{Learner} that incorporates the wrappers and learners suitable for mlr
#' learners.
#' 
#' The slot \code{$searchspace} should be used as \code{ParamSet} to tune
#' parameters over.
makeAMExoWrapper = function(modelmultiplexer, wrappers, taskdesc, canHandleX,
    allLearners) {

  presentprops = c(names(taskdesc$n.feat)[taskdesc$n.feat > 0],
      if (taskdesc$has.missings) "missings")

  classlvlcount = min(3, length(taskdesc$class.levels))
  properties = c(c("oneclass", "twoclass", "multiclass")[classlvlcount],
      presentprops)

  aux = buildSearchSpace(wrappers, presentprops, canHandleX, allLearners)
  completeSearchSpace = c(modelmultiplexer$searchspace,
      aux$completeSearchSpace)

  # automlr.hasXXX replaces the parameters that are external-only.
  propertiesReplace = aux$replaces
  shadowparams = c(
      aux$shadowparams,
      extractSubList(Filter(function(x) isTRUE(x$amlr.isDummy),
              completeSearchSpace$pars), "id"))
  # what's missing is removing the singleton parameters from the search space
  # and replacing them with direct setting of parameter values internally.
  # Easy to forget: parameters set for the modelmultiplexer via setHyperPars,
  # but not visible externally, also need to be treated like this.
  
  aux = extractStaticParams(completeSearchSpace, getHyperPars(modelmultiplexer))
  staticParams = aux$staticParams
  substitutions = aux$substitutions
  finalSubstitutions = c(aux$finalSubstitutions, propertiesReplace)
  completeSearchSpace = aux$completeSearchSpace
  
  # replace the singleton values inside the requirements of other parameters.
  
  completeSearchSpace$pars = substituteParamList(completeSearchSpace$pars,
      substitutions)
  completeSearchSpace$pars = substituteParamList(completeSearchSpace$pars,
      finalSubstitutions)
  staticParams = substituteParamList(staticParams, substitutions)
  staticParams = substituteParamList(staticParams, finalSubstitutions)
  # shadowparams are supposed to be only visible on the outside.
  # automlr.wrappersetup is handled separately.
  staticParams[extractSubList(staticParams, "id") %in%
          c("automlr.wrappersetup", shadowparams)] = NULL

  
  allNames = getParamIds(completeSearchSpace)
  paramReferenceStop = rep(list(quote(stop("AMLR VARREF STOP"))),
      length(allNames))
  names(paramReferenceStop) = allNames
# paramReferenceStop = list2env(paramReferenceStop, parent = baseenv())
  # Now after all the replacing going on, there might be parameters that have a
  # `requires` always TRUE or always FALSE.
  for (param in getParamIds(completeSearchSpace)) {
    curpar = completeSearchSpace$pars[[param]]
    # we test whether the requires is trivially TRUE or FALSE by evaluating it
    # in an empty environment.
    if (is.null(curpar$requires)) {
      next
    }
    
    # take the environment that would usually be present, replace all values
    # with stop("EXPECTED STOP"), and check if the expected stop happened. Is it
    # possible to stop on variable reference? apparently not, so we have to go
    # the long replaceRequires route. This would have the advantage to not
    # filter out trivial invocations of e.g. c(), and also that we could check
    # for the simplest of syntax errors by watching whether the error happening
    # is actually the error we expect.
    if (is.null(curpar$amlr.learnerName)) {
      paramReferenceStop$selected.learner = quote(stop("AMLR VARREF STOP"))
    } else {
      # maybe the requires is a mlr learner's requires that now only depends on
      # selected.learner being something. if it is FALSE even if
      # selected.learner equals the given learner, then we remove the parameter.
      # This would be the case e.g. if the parameter is only sensible if there
      # are NAs in the data and the current data set does not have NAs.
      paramReferenceStop$selected.learner = curpar$amlr.learnerName
    }
    req = replaceRequires(curpar$requires, paramReferenceStop)
    tryResult = try(reqValue <- eval(req, globalenv()), silent = TRUE)
    if (!is.error(tryResult)) {
      if (isTRUE(reqValue)) {
        # always true -> remove requirement
        if (is.null(curpar$amlr.learnerName) ||
            "selected.learner" %in% extractSubList(staticParams, "id")) {
          completeSearchSpace$pars[[param]]$requires = NULL
        } else {
          completeSearchSpace$pars[[param]]$requires = substitute(
              selected.learner == SL, list(SL = curpar$amlr.learnerName))
        }
      } else {
        # always false -> remove the parameter.
        completeSearchSpace$pars[[param]] = NULL
      }
    } else {
      errormsg = attr(tryResult, "condition")$message
      if (!identical(errormsg, "AMLR VARREF STOP")) {
        stopf("Error while evaluating requirement for parameter '%s'%s: '%s'.",
            param, ifelse(is.null(curpar$amlr.learnerName), "",
                paste0(" of learner ", curpar$amlr.learnerName)),
            errormsg)
      }
      if (class(curpar$requires) == "expression") {  # this apparently happens.
        newreq = deExpression(curpar$requires)
        completeSearchSpace$pars[[param]]$requires = newreq
      } else if (class(curpar$requires) != "call") {
        stopf("Parameter '%s' has broken requirement:\n%s", param,
            collapse(deparse(curpar$requires), sep = "\n"))
      }
    }
  }
  


  # transform into "LearnerParam" types. This is mostly dumb relabeling, except
  # for one thing: The limits / values of the parameters with "trafo" have to be
  # reverse-transformed.
  learnerPars = makeLearnerPars(completeSearchSpace)
  
  visibleHyperIndex = names(getHyperPars(modelmultiplexer)) %in%
      getParamIds(learnerPars)

  learner = wrapLearner("AMExoWrapper", "amlr", "automlrlearner",
      learner = modelmultiplexer,
      type = taskdesc$type,
      properties = properties,
      par.set = learnerPars,
      par.vals = getHyperPars(modelmultiplexer)[visibleHyperIndex],
      config = list(show.info = FALSE, on.learner.error = "quiet",
          on.learner.warning = "quiet", on.par.without.desc = "stop",
          on.par.out.of.bounds = "stop", show.learner.output = FALSE))

  learner$staticParams = staticParams
  learner$searchspace = completeSearchSpace
  learner$fix.factors.prediction = TRUE
  learner$wrappers = extractSubList(wrappers, "constructor")
  learner$shadowparams = shadowparams
  learner
}

#' @export
trainLearner.AMExoWrapper = function(.learner, .task, .subset, .weights = NULL,
    automlr.wrappersetup, ...) {
  # train selected learner model and remove prefix from its param settings
  learner = .learner$learner
  
  sl = list(...)$selected.learner
  if (is.null(sl)) {
    slIndex = which("selected.learner" ==
            extractSubList(.learner$staticParams, "id"))
    assert(length(slIndex) == 1)
    sl = .learner$staticParams[[slIndex]]$value
  }
  learner$properties = learner$base.learners[[sl]]$properties
  

  if (length(.learner$wrappers) > 0) {
    if (length(.learner$wrappers) == 1) {
      # in this case automlr.wrappersetup will be *missing*.
      automlr.wrappersetup = names(.learner$wrappers)
    }
    for (w in rev(unlist(strsplit(automlr.wrappersetup, "$", TRUE)))) {
      learner = .learner$wrappers[[w]](learner)
    }
  } else {
    automlr.wrappersetup = "$"
  }
  learner = setupLearnerParams(learner, .learner$staticParams,
      .learner$shadowparams, list(automlr.wrappersetup = automlr.wrappersetup,
          ...))

  .learner$learner = learner

  NextMethod("trainLearner")
}

#' @export
predictLearner.AMExoWrapper = function(.learner, .model, .newdata, ...) {
  on.exit(quickSuspendInterrupts(unpatchMlr()), add = TRUE)
  patchMlrPredict()
  NextMethod("predictLearner")
}

setupLearnerParams = function(learner, staticParams, shadowparams, params) {
  pnames = names(params)
  envir = insert(getHyperPars(learner), params)
  for (fp in staticParams) {
    if (is.null(fp$requires) || isTRUE(eval(fp$requires, envir = envir))) {
      if (fp$id %in% names(params)) {
        stopf(paste0("Parameter '%s' is a static (internal) parameter but was",
                " also given externally."),
            fp$id)
      }
      params[[fp$id]] = fp$value
    }
  }
  for (p in pnames) {
    tp = removeAmlrfix(p)
    if (tp != p) {
      if (tp %in% names(params)) {
        stopf(paste0("Parameter '%s' and '%s' both given although they should",
                " be exclusive."),
            tp, p)
      }
      params[[tp]] = params[[p]]
      params[[p]] = NULL
    }
  }
  params[c("automlr.wrappersetup", shadowparams)] = NULL
  setHyperPars(learner, par.vals = params)
}

buildSearchSpace = function(wrappers, properties, canHandleX, allLearners) {
  # Introduce `automlr.wrappersetup` (in case there are any wrappers present at
  # all).
  newparams = list()
  
  # Introduce other external parameters: automlr.remove.XXX and
  # automlr.wremoving.XXX
  allTypes = c("missings", "factors", "ordered", "numerics")
  # removers maps type -> all wrappers that are able to remove the type
  removers = list()
  for (type in allTypes) {
    if (type %nin% properties) {
      next
    }
    for (w in names(wrappers)[extractSubList(wrappers, "required")]) {
      if (type != "numerics" &&
          any(c("numerics", "") %in% wrappers[[w]]$conversion(type))) {
        # the wrapper can delete the covariate in question
        removers[[type]] = c(removers[[type]], w)
      }
    }
    if (length(removers[[type]]) == 0) {
      next  # we can't remove the type at all
    }
    
    # if there is at least one remover, we introduce the external parameter
    # telling whether to remove. The requirement is that the active learner
    # actually leaves a choice -- if it cannot handle the type to begin with,
    # then the wrapper is obligated to remove the type anyways.
    amlrRemoveName = paste0("automlr.remove.", type)
    if (length(canHandleX[[type]]) > 0) {
      if (setequal(allLearners, canHandleX[[type]])) {
        # if all learners can handle the type, the variable is always valid.
        requires = NULL
      } else {
        requires = substitute(selected.learner %in% x,
            list(x = canHandleX[[type]]))
      }
      newparams = c(newparams,
          list(makeLogicalParam(amlrRemoveName, requires = requires)))
    }
      
    # if there are at least two removers, we need to introduce another external
    # parameter telling which wrapper should do the removing.
    if (setequal(allLearners, canHandleX[[type]])) {
      # need to do the silly ==TRUE thing bc the result isn't "call" class
      # otherwise
      requires = asQuoted(paste(amlrRemoveName, "== TRUE"))
    } else if (length(canHandleX[[type]]) == 0) {
      requires = NULL
    } else {
      requires = substitute(selected.learner %nin% x || amlrRemove,
          list(x = canHandleX[[type]],
              amlrRemove = asQuoted(amlrRemoveName)))
    }
    removingWrapperName = paste0("automlr.wremoving.", type)
    newparams = c(newparams, list(makeDiscreteParam(removingWrapperName,
                removers[[type]], requires = requires)))
  }

  # Manipulate the wrapper's search space
  #  - substitute the pseudo variables (automlr.has.xxx, automlr.remove.xxx)
  #    with expressions
  #  - add the requirement that the wrapper is actually present

# the empty string for building replaceList for nonwrappers
  for (w in c(names(wrappers), "")) {
    replaceList = list()
    for (type in allTypes) {
      if (type %nin% properties) {
        replaceList[[paste0("automlr.has.", type)]] = FALSE
        replaceList[[paste0("automlr.remove.", type)]] = FALSE
        next
      }
      if (length(removers[[type]]) == 0)  {
        # can not be deleted, therefore always present
        replaceList[[paste0("automlr.has.", type)]] = TRUE
        # by given constraint, no automlr.remove.XXX is present and therefore
        # doesn't need to be replaced.
        next
      }
      amlrRemoveQuote = asQuoted(paste0("automlr.remove.", type))
      amlrWRemovingQuote = asQuoted(paste0("automlr.wremoving.", type))
      
      # ** substituting automlr.has.xxx here
      # when is 'autmlr.has.XXX' true for a given wrapper?
      #  - either the given learner can handle xxx AND xxx is not requested
      #    removed by external variable
      typeStaysPresentQuote = substitute(
          selected.learner %in% canHandleDelendum && !amlrRemove,
          list(canHandleDelendum = canHandleX[[type]],
              amlrRemove = amlrRemoveQuote))
      if (w != "") {
        #  - OR this wrapper comes before the wrapper (or is the wrapper) that
        #    removes xxx.
        typeRemovedAfterwardQuote = substitute(
            which(unlist(strsplit(automlr.wrappersetup, "$", TRUE)) ==
                    thisWrapper) <=
            which(unlist(strsplit(automlr.wrappersetup, "$", TRUE)) ==
                    removingWrapper),
            list(thisWrapper = w, removingWrapper = amlrWRemovingQuote))
        replaceQuote = substitute(a || b, list(a = typeStaysPresentQuote,
                b = typeRemovedAfterwardQuote))
        replaceList[[paste0("automlr.has.", type)]] = replaceQuote
        # ** substituting automlr.remove.xxx here
        if (w %in% removers[[type]]) {
          # only then this wrapper is allowed to use 'automlr.remove.xxx'
            replaceQuote = substitute(!typeStaysPresent && wremoving == ownName,
                    list(typeStaysPresent = typeStaysPresentQuote,
                        wremoving = amlrWRemovingQuote, ownName = w))
            replaceList[[paste0("automlr.remove.", type)]] = replaceQuote
        }
      } else {
        replaceList[[paste0("automlr.has.", type)]] = typeStaysPresentQuote
      }
    }
    if (w == "") {
      next
    }
    for (parname in getParamIds(wrappers[[w]]$searchspace)) {
      req = wrappers[[w]]$searchspace$pars[[parname]]$requires
      if (is.null(req)) {
        if (!wrappers[[w]]$required) {
          wrappers[[w]]$searchspace$pars[[parname]]$requires = substitute(
              thisWrapper %in%
                  unlist(strsplit(automlr.wrappersetup, "$", TRUE)),
              list(thisWrapper = w))
        }
      } else {
        req = replaceRequires(req, replaceList)
        if (!wrappers[[w]]$required) {
          # For the wrappers that are not always present: need to add "wrapper
          # is actually used" as a requirement.
          req = substitute(
              (thisWrapper %in%
                    unlist(strsplit(automlr.wrappersetup, "$", TRUE))) &&
                  restReq,
              list(thisWrapper = w, restReq = deExpression(req)))
        }
        wrappers[[w]]$searchspace$pars[[parname]]$requires = req
      }
    }
  }
  
  shadowparams = extractSubList(newparams, "id")
  
  if (length(wrappers)) {
    wrapperSelectParam = makeDiscreteParam("automlr.wrappersetup",
        listWrapperCombinations(
            names(wrappers), unlist(extractSubList(wrappers, "required"))))
    newparams = c(newparams, list(wrapperSelectParam))
  }
  
  # combine all the ParamSets we have seen now

  completeSearchSpace = c(
      do.call(base::c, extractSubList(wrappers, "searchspace",
              simplify = FALSE)),
      makeParamSet(params = newparams))

  list(shadowparams = shadowparams,
      completeSearchSpace = completeSearchSpace,
      replaces = replaceList)
}

listWrapperCombinations = function(ids, required) {
  combineNames = function(x) {
    if (all(requiredIDs %in% x) && all(!duplicated(x))) {
      paste(x, collapse = "$")
    }
  }
  requiredIDs = ids[required]
  result = sapply(seq_along(ids), function(l) {
        apply(expand.grid(rep(list(ids), l)), 1, combineNames)
      })
  if (all(!required)) {
    # all wrappers are optional --> add "no wrappers" option. The empty string
    # causes errors, however.
    result = c("$", result)
  }
  unlist(result)
}

substituteParamList = function(paramList, substitutions, maxCycles = 32) {
  for (dummy in seq_len(maxCycles)) {
    # go `cycles` steps deep, in case one of the substituted variables itself
    # requires another variable.
    dirty = FALSE
    for (pid in seq_along(paramList)) {
      req = paramList[[pid]]$requires
      if (!is.null(req)) {
        preReplace = as.expression(req)
        paramList[[pid]]$requires = replaceRequires(req, substitutions)
        if (!identical(paramList[[pid]]$requires, preReplace)) {
          dirty = TRUE
        }
      }
    }
    if (!dirty) {
      return(paramList)
    }
  }
  stop("Too much recursion when replacing requirements")
}

makeLearnerPars = function(learnerPars) {
  for (p in getParamIds(learnerPars)) {
    if (!is.null(learnerPars$pars[[p]]$trafo) &&
        learnerPars$pars[[p]]$type %in%
        c("numeric", "numericvector", "integer", "integervector")) {
      # there is a trafo --> need to change limits
      if (is.null(learnerPars$pars[[p]]$amlr.origValues)) {
        learnerPars$pars[[p]]$lower = -Inf
        learnerPars$pars[[p]]$upper = Inf
      } else {
        learnerPars$pars[[p]]$lower = learnerPars$pars[[p]]$amlr.origValues[1]
        learnerPars$pars[[p]]$upper = learnerPars$pars[[p]]$amlr.origValues[2]
      }
      # convert type to "numeric(vector)", since after trafo we are not sure
      # it is still an int
      learnerPars$pars[[p]]$type = switch(learnerPars$pars[[p]]$type,
          integer = "numeric",
          integervector = "numericvector",
          learnerPars$pars[[p]]$type)
    }
    learnerPars$pars[[p]]$trafo = NULL
    # as the things stand now we don't wrap setHyperPars and therefore all
    # hyperpars need to be given to the train function. If this ever changes
    # (and we e.g. call predictLearner() instead of predict(), and we wrap
    # setHyperPars() also) we need to copy the $when property of the
    # corresponding LearnerParam inside the modelmultiplexer object.
    learnerPars$pars[[p]]$when = "train"
    learnerPars$pars[[p]] = addClasses(learnerPars$pars[[p]], "LearnerParam")
    # satisfying a weird constraint of mlr:
    req = learnerPars$pars[[p]]$requires
    if (!is.null(req) && is.expression(req)) {
      if (length(req) == 1) {
        learnerPars$pars[[p]]$requires = req[[1]]
      } else {
        learnerPars$pars[[p]]$requires = substitute(eval(x), list(x = req))
      }
    }
  }
  learnerPars
}


extractStaticParams = function(completeSearchSpace, presetStatics) {
  # How the substitution mechanism works:
  # There are two distinct problems that this is supposed to solve:
  # 1) Some parameters have different feasible regions depending on other
  #    variables.
  # 2) Singleton parameters that only take on one value should not be visible
  #    outside
  #
  # The first problem is solved by letting different external parameters with
  # name `varname.AMLRFIX#` refer to the same parameter `varname` of the actual
  # learner. These different external parameters should all have mutually
  # exclusive requirements. The parameters have to be substituted at two places:
  # when setting the hyperparameters of the actual learners, and inside the
  # requirement definitions of the individual parameters. This way, one
  # parameter can refer to `varname` without needing to worry about different
  # .AMLRFIXes.
  #
  # The second problem is solved using a similar mechanism, since it follows the
  # same principle: We want to have different parameters visible to the outside
  # than the parameters we present to the learners.
  #
  # We have the following datastructures:
  # staticParams :: lists information about all the parameters that are set on
  #                 the inside
  #                 but not visible to the public.
  # substitutions :: substitutions that will be performed inside the
  #                  requirements of other parameters, as well as recursively on
  #                  the substitutions themselves
  # finalSubstitutions :: Substitutions that will be performed once after the
  #                       other substitutions were done. This is to prevent
  #                       endless recursion.
  # completeSearchSpace :: The search space that will be given externally.
  #

  # all parameters that have only a single value
  staticParams = lapply(names(presetStatics), function(n)
        list(id = n, value = presetStatics[[n]], requires = NULL))
  # substitution that will be used instead of the param inside of other
  # parameter's $requires.
  substitutions = list()
  finalSubstitutions = presetStatics
  for (param in getParamIds(completeSearchSpace)) {
    curpar = completeSearchSpace$pars[[param]]
    parid = removeAmlrfix(curpar$id)
    leaf = paste0(parid, ".AMLRFINAL")
    if ((curpar$type %in% c("discrete", "discretevector") &&
          length(curpar$values) == 1) ||  # this is a 'fixed' value
        (curpar$type %in%
          c("numeric", "integer", "numericvector", "integervector") &&
          all(curpar$lower == curpar$upper))) {  # valid interval is a point
      fixvalue = if (curpar$type == "discrete") {
        curpar$values[[1]]
      } else if (curpar$type == "discretevector") {
        if (!is.null(curpar$amlr.isNotCat) && curpar$amlr.isNotCat) {
          rep(unname(curpar$values[[1]]), curpar$len)
        } else {
          rep(unname(curpar$values[1]), curpar$len)
        }
      } else {
        assert(all(curpar$lower[1] == curpar$lower))
        curpar$lower
      }
      completeSearchSpace$pars[[param]] = NULL
      if (!is.null(curpar$requires)) {
        # the following is a bit unfortunate, because it introduces a kind of
        # recursive dependence. I don't see a better way, however. The problem
        # is that if we have a variable xyz, and a variable xyz.AMLRFIX1, then
        # we want to remove the xyz.AMLRFIX1 and replace it with its fixed value
        # given the requirement. However, if the requirement is not given, the
        # parameter space given value must be used.
        # SOLUTION: append a suffix that prevents cycling in on itself.
        
        subst = substitute(if (eval(req)) value else original,
            list(req = as.expression(curpar$requires), value = fixvalue,
                original = asQuoted(leaf)))
        if (parid == curpar$id) {
          # the value itself is fixed -> if there are references remaining in
          # a requirement after all substitutions were done, it is an error.
          # This can happen, if the requirements of the fixed values do not
          # cover the whole domain.
          finalSubstitutions[[leaf]] = substitute(
              stop(sprintf(paste("Parameter %s is fixed, but its reqs do not",
                               "cover the whole domain."), parname)),
                  list(parname = parid))
        }
        if (is.null(finalSubstitutions[[leaf]])) {
          # check whether it is null; we don't want to overwrite it if the
          # original is a fixed value.
          finalSubstitutions[[leaf]] = asQuoted(parid)
        }
      } else {
        subst = fixvalue
      }
      # staticParams is not a named list, because the same parid may occur
      # multiple times (after stripping .AMLRFIX#)
      staticParams = c(staticParams, list(list(id = parid, value = fixvalue,
                  requires = curpar$requires)))
      if (parid %in% names(substitutions)) {
        # yay, we already have this substitution. This is only allowed to happen
        # if there are exclusive reuqirements, so we are able to substitute the
        # substitutions inside each other.
        assert(!is.null(curpar$requires))
        sl = list()
        sl[[leaf]] = substitutions[[parid]]
        # FIXME: the following has the c() vs. c vulnerability
        substitutions[[parid]] = do.call(substitute, list(subst, sl))
      } else {
        substitutions[[parid]] = subst
      }
    } else {
      # FIXME: the following is half a copy of the code above. maybe it is
      # possible to clean it up at some point.
      if (parid != curpar$id) {
        # substituting .AMLRFIX
        assert(!is.null(curpar$requires))
        subst = substitute(if (eval(req)) thisfix else original,
            list(req = as.expression(curpar$requires),
                thisfix = asQuoted(curpar$id), original = asQuoted(leaf)))
        if (parid %in% names(substitutions)) {
          sl = list()
          sl[[leaf]] = substitutions[[parid]]
          substitutions[[parid]] = do.call(substitute, list(subst, sl))
        } else {
          if (is.null(finalSubstitutions[[leaf]])) {
              # check whether it is null; we don't want to overwrite it if the
              # original is a fixed value.
              finalSubstitutions[[leaf]] = asQuoted(parid)
          }
          substitutions[[parid]] = subst
        }
      }
    }
  }
  list(completeSearchSpace = completeSearchSpace,
      staticParams = staticParams,
      substitutions = substitutions,
      finalSubstitutions = finalSubstitutions)
}

getSearchspace.AMExoWrapper = function(learner) {
  learner$searchspace
}
