
#################################
# Creator                       #
#################################

#' @title Create an mlr learner object that smoothes out the parameter space.
#'
#' @description
#' So what are we doing here?
#' The AMExoWrapper mostly handles parameter space magic. Specifically, this is:
#' \itemize{
#'   \item introduce parameters that chooses which wrappers are used, and in
#'     which sequence
#'   \item introduce parameters that control whether the wrapper(s) are used to
#'     facilitate compatibility of learners with the data, i.e. remove missing
#'     values, convert data types that are not supportet
#'   \item set \dQuote{pseudoparameters}, which are some special variable
#'     values that the `requires`-expressions of other parameters can use to
#'     specify that some parameters are only relevant in the presence of
#'     certain data types.
#'   \item remove parameters that always take on a single value (maybe dependent
#'     on a `requires` from the search space) and set them internally before
#'     calling the wrapped model.
#'   \itemize{
#'     \item bonus: if a parameter name contains a suffix of .AMLRFIX#,
#'       where # is a number, it will be stripped from the parameter name. This
#'       way it is possible to define alternative ranges for a parameter,
#'       depending on requirements.
#'   }
#' }
#' 
#' In the following, \dQuote{XXX} and \dQuote{YYY} will always be one of
#' \dQuote{numerics}, \dQuote{ordereds}, \dQuote{factors}.
#' The parameters that are introduced and exposed to the outside are:
#' \itemize{
#'   \item automlr.convert: Whether to do any conversion of input data.
#'   \item automlr.convert.XXX: Whether to convert input data of type
#'     \dQuote{XXX}.
#'   \item automlr.convert.XXX.to: What type to convert \dQuote{XXX}-typed
#'     data into, if there is any choice.
#'   \item automlr.wrapafterconvert.XXX: Whether to apply preprocessing wrapper
#'     after conversion of data originally of type \dQuote{XXX}.
#'   \item automlr.wconverting.XXX.to.YYY: Which conversion wrapper to use for
#'     conversion of data from type \dQuote{XXX} to type \dQuote{YYY}.
#'   \item automlr.impute: Whether to do imputation of missing values.
#'   \item automlr.missing.indicators: Whether imputation is supposed to
#'     introduce \dQuote{factor} typed missing indicator variables.
#'   \item automlr.wimputing.XXX: Which imputation wrapper to use for
#'     imputation of \dQuote{XXX}-typed data, if there is any choice.
#'   \item automlr.preproc.XXX: Which  non-converting, non-imputing wrappers are
#'     used for preprocessing, with values of the format
#'     \code{outermostWrapper$wrapper...$wrapper$innermostwrapper}.
#' }
#'
#' The following parameters can be used by wrappers and learners in their
#' \code{$requires}-parameter; they will be replaced here:
#' \itemize{
#'   \item automlr.missing.indicators: Should be used by the imputing wrapper.
#'     Indicates whether imputation is supposed to introduce \dQuote{factor}
#'     typed missing indicator variables.
#'   \item automlr.has.XXX: May be used by learners:
#'     Indicates that the \dQuote{XXX} type is present in the data.
#'     Besides the types, mentioned above, \dQuote{XXX} may also be
#'     \dQuote{missings}.
#'   \item automlr.targettype: One of \dQuote{oneclass}, \dQuote{twoclass},
#'     \dQuote{multiclass}.
#' }
#' @param modelmultiplexer [\code{ModelMultiplexer}]\cr
#'   A modelmultiplexer object that should have a \code{$searchspace} slot.
#' @param wrappers [\code{list}]\cr
#'   A named list of wrappers. Names must not contain \code{$}-character.
#' @param taskDesc [\code{TaskDesc}]\cr
#'   The \dQuote{TaskDesc} object of the task to be optimized over.
#' @param missings [\code{logical}]\cr
#'   A logical, with names according to the \emph{present} feature data types
#'   (a subset of numerics, factors, ordered) indicating whether the columns
#'   in question have any missing values.
#' @param canHandleX [\code{list}]\cr
#'   A named list that maps "missings", "numerics", "factors", and "ordered"
#'   to a vector of learner names that can handle the respective data.
#' @param allLearners [\code{character}]\cr
#'   The list of all learner names.
#' @param modelTuneParsets [\code{list} of \code{ParamSet}]\cr
#'   List of each learner's ParamSet, indexed by learner ID.
#' 
#' @return [\code{AMExoWrapper}]
#' A \code{Learner} that incorporates the wrappers and learners suitable for mlr
#' learners.
#' 
#' The slot \code{$searchspace} should be used as \code{ParamSet} to tune
#' parameters over.
makeAMExoWrapper = function(modelmultiplexer, wrappers, taskDesc, missings,
    canHandleX, allLearners, modelTuneParsets) {

  aux = buildWrapperSearchSpace(wrappers, missings, canHandleX, allLearners)
  
  wrapperparnames = getParamIds(aux$wrapperps)

  completeSearchSpace = c(modelmultiplexer$searchspace, aux$wrapperps)

  # automlr.has.XXX replaces the parameters that are external-only.
  propertiesReplace = aux$replaces
  classlvlcount = min(3, length(taskdesc$class.levels))
  targettype = c("oneclass", "twoclass", "multiclass")[classlvlcount]
  propertiesReplace$automlr.targettype = targettype

  # all the transformations that need to be done before training, but after
  # the exact data is available.
  expressiontrafos = filterNull(extractSubList(completeSearchSpace$pars,
          "amlr.expressionTrafo", simplify = FALSE))
  fulltps = modelTuneParsets
  # a list 'param name' => 'which learner it belongs to'
  learners.by.params = list()
  for (l in names(fulltps)) {
    fullpnames = paste(l, getParamIds(fulltps[[l]]), sep = ".")
    learners.by.params[fullpnames] = l
  }
  # a list 'learner param' => 'collection of param names in the same learner'
  epsources = sapply(names(expressiontrafos), function(n) {
        l = learners.by.params[[n]]
        assert(!is.null(l))
        getParamIds(fulltps[[l]])
      }, simplify = FALSE)

  shadowparams = c(
      extractSubList(aux$wrapperparams, "id"),
      extractSubList(Filter(function(x) isTRUE(x$amlr.isDummy),
              completeSearchSpace$pars), "id"))
  # what's missing is removing the singleton parameters from the search space
  # and replacing them with direct setting of parameter values internally.
  # Easy to forget: parameters set for the modelmultiplexer via setHyperPars,
  # but not visible externally, also need to be treated like this.
  
  aux = extractStaticParams(completeSearchSpace)
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
  
  # dependency on parameters with expression bounds is forbidden.
  badreqs = names(expressiontrafos)
  checkBadreqs(completeSearchSpace$pars, badreqs)
  checkBadreqs(staticParams, badreqs)

#  # shadowparams are supposed to be only visible on the outside.
#  # automlr.wrappersetup is handled separately.
#  staticParams[extractSubList(staticParams, "id") %in%
#          c("automlr.wrappersetup", shadowparams)] = NULL
  
  completeSearchSpace = simplifyRequirements(completeSearchSpace)

  # transform into "LearnerParam" types. This is mostly dumb relabeling, except
  # for one thing: The limits / values of the parameters with "trafo" have to be
  # reverse-transformed.
  learnerPars = makeLearnerPars(completeSearchSpace)
  
  visibleHyperIndex = names(getHyperPars(modelmultiplexer)) %in%
      getParamIds(learnerPars)
  
  properties = c(names(taskdesc$n.feat)[taskdesc$n.feat > 0],
      if (taskdesc$has.missings) "missings")
  classlvlcount = min(3, length(taskdesc$class.levels))
  properties %c=% c("oneclass", "twoclass", "multiclass")[classlvlcount]

  learner = wrapLearner("AMExoWrapper", "amlr", "automlrlearner",
      learner = removeHyperPars(modelmultiplexer,
          setdiff(getHyperPars(modelmultiplexer), getParamIds(learnerPars))),
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
  learner$wrapperparnames = wrapperparnames
  
  learner$expressiontrafos = expressiontrafos
  learner$epsources = epsources
  learner$learners.by.params = learners.by.params
  learner
}

#################################
# Learner Interface             #
#################################

#' @export
trainLearner.AMExoWrapper = function(.learner, .task, .subset, .weights = NULL,
    automlr.wrappersetup, ...) {
  # train selected learner model and remove prefix from its param settings
  learner = .learner$learner

  args = getEffectiveHyperPars(learner, .learner$staticParams, list(...))

  wrapperargs = args[names(args) %in% c(.learner$wrapperparnames,
          .learner$shadowparams)]
  
  args = args[names(args) %nin% .learner$wrapperparnames]

  .task = .task %>>% buildCPO(handleAmlrfix(wrapperargs), .learner$wrappers)

  args = applyExpressionBoundTrafos(args, .learner$expressiontrafos,
      .learner$epsources, .learner$learners.by.params, .task)

  args = handleAmlrfix(args)

  sl = args$selected.learner
  learner$properties = learner$base.learners[[sl]]$properties

  learner = setHyperPars(learner,
      par.vals = dropNamed(args, .learner$shadowparams))

  .learner$learner = learner  # respect automlrWrappedLearner interface

  NextMethod("trainLearner")
}

# collect hyperparameters from 'staticParams', the given parameters, and the
# 
getEffectiveHyperPars = function(learner, staticParams, params) {
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
  params
}

handleAmlrfix = function(params) {
  pnames = names(params)
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
  params
}

applyExpressionBoundTrafos = function(args, expressiontrafos, epsources,
    learners.by.params, task) {
  pvs = handleAmlrfix(args)
  
  tohandle = intersect(names(expressiontrafos), names(args))
  env0 = list(n = getTaskSize(task), p = length(getTaskFeatureNames(task)))
  for (th in tohandle) {
    sources = epsources[[th]]
    lname = learners.by.params[[th]]
    sl = setNames(args[paste(lname, sources, sep = ".")],
        paste0("PARAM.", sources))
    args[[th]] = expressiontrafos[[th]](args[[th]], c(env0, sl))
  }
  args
}

#################################
# Searchspace                   #
#################################

extractStaticParams = function(completeSearchSpace) {
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
  staticParams = list()
  # substitution that will be used instead of the param inside of other
  # parameter's $requires.
  substitutions = list()
  finalSubstitutions = list()
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
        
        subst = substitute(if (isTRUE(req)) value else original,
            list(req = curpar$requires, value = fixvalue,
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
      staticParams %c=% list(list(id = parid, value = fixvalue,
              requires = curpar$requires))
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
        subst = substitute(if (isTRUE(req)) thisfix else original,
            list(req = curpar$requires,
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

substituteParamList = function(paramList, substitutions, maxCycles = 32) {
  for (dummy in seq_len(maxCycles)) {
    # go `cycles` steps deep, in case one of the substituted variables itself
    # requires another variable.
    dirty = FALSE
    for (pid in seq_along(paramList)) {
      req = paramList[[pid]]$requires
      if (!is.null(req)) {
        paramList[[pid]]$requires = replaceRequires(req, substitutions)
        if (!identical(paramList[[pid]]$requires, req)) {
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

# check that there is no dependency on 'badreqs'. This is currently only used
# for parameters that have expression bounds.
checkBadreqs = function(paramList, badreqs) {
  substs = sapply(badreqs, function(dummy) quote(stop()), simplify = FALSE)
  for (pid in seq_along(paramList)) {
    req = paramList[[pid]]$requires
    if (!is.null(req)) {
      if (!identical(req, replaceRequires(req, substs))) {
        stop("Parameter %s has requirement %s.", paramList[[pid]]$id,
            "depending on expression bound")
      }
    }
  }
}

# check if any requirements do not actually depend on parameters any more, e.g.
# because of fixed values. Remove parameters with unfulfilled requirements,
# simplify parameters with requirements only depending on 'selected.learner'
# and remove requirements that are always TRUE.
simplifyRequirements = function(completeParamSpace) {
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
#      if ((curpar$requires) == "expression") {  # this apparently happens.
#        newreq = deExpression(curpar$requires)
#        completeSearchSpace$pars[[param]]$requires = newreq
#      } else
      if (!is.call(curpar$requires)) {
        stopf("Parameter '%s' has broken requirement:\n%s", param,
            collapse(deparse(curpar$requires), sep = "\n"))
      }
    }
  }
}

getSearchspace.AMExoWrapper = function(learner) {
  learner$searchspace
}
