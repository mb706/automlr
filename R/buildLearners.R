

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
  # [ ] filter out learners by what is needed, e.g.
  #   [ ] check if the learner can handle the input data format (numeric, factorial)
  #   [ ] check if there is a wrapper that turns the input data into appropriate format (e.g. removes factors)
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
  #   [ ] parameters that are somehow transformed to other parameters. Can we thus solve classif.bartMachine$mh_prob_steps?
  #   [ ] maybe we hack mlr itself and make transformation functions that depend on other hyperparameters.
  #       - this cannot work because there may be circular dependencies.
  #       - so we need to somehow wrap classif.pamr to make threshold.predict = max(thresholds) * threshold.predict.fraction i guess
  #   [ ] i think i have to reimplement classif.LiblineaRXXX myself to get the broken parameter space back together.
  #   [ ] wrappers have to be able to give some information about transformations they make
  #     [ ] removing factors / removing numerics; respecting that factors/numerics might not be present to begin with.
  #     [ ] removing / not removing NAs (in factors/numerics); respecting that NAs might not be present to begin with.
  # [ ] somehow link the same-id parameters
  #   [ ] warning if id happens only once.
  learners = searchspace[extractSubList(searchspace, "stacktype") == "learner"]  # exclude e.g. wrappers
  learnerObjects = list()
  modelTuneParsets = list()
  for (i in seq_along(learners)) {
    l = myCheckLearner(learners[[i]]$learner)
    sslist = learners[[i]]$searchspace
    if (task$type != l$type) {  # skip this learner, it is not fit for the task
      next
    }
    # TODO: check whether the covariate type is supported
    aux = buildTuneSearchSpace(sslist, l)
    modelTuneParsets[[l$id]] = aux$tss
    learnerObjects = c(learnerObjects, aux$l)  # updated learner object with fixed hyperparameters
  }
  multiplexer = makeModelMultiplexer(learnerObjects)
  tuneParamSet = makeModelMultiplexerParamSetEx(multiplexer, modelTuneParsets)
  multiplexer$searchspace = tuneParamSet
  multiplexer
}

buildTuneSearchSpace = function(sslist, l) {
  tuneSearchSpace = list()
  for (j in seq_along(sslist)) {
    param = s[[j]]
    if (param$type == "bool") {
      # useful since now we can automatically check for feasibility by checking whether everything in param$values is feasible.
      param$values = c(TRUE, FALSE)
    }
    if (param$dummy) {  # dummy --> is not in the learner
      if (param$name %in% getParamIds(getParamSet(l))) {
        stopf("Parameter '%s' is present in learner '%s' but is marked as `dummy` in search space.",
            param$name, l$id)
      }
      l$par.set = c(l$par.set, makeParamSet(createParameter(param)))
    } else {
      lp = getParamSet(l)
      lpids = getParamIds(lp)
      lptypes = getParamTypes(lp)
      names(lptypes) = lpids
      if (param$name %nin% lpids) {
        stopf("Parameter '%s' as listed in search space is not available for learner '%s'.",
            param$name, l$id)
      }
      if (!allfeasible(lp, param$values, param$name, param$dim)) {
        stopf("Parameter '%s' as listed in search space has infeasible bounds '%' for learner '%s'.",
            param$name, paste(param$values, collapse=" "), l$id)
      }
      if ((param$type == "int" && lptypes[[param$name]] %nin% c("integer", "integervector")) &&
          (param$type %in% c("int", "real") && lptypes[[param$name]] %nin% c("numeric", "numericvector"))) {
        stopf("Parameter '%s' as listed in search space has wrong type '%' for learner '%s'",
            param$name, param$type, l$id)
      }
      if ((param$type == "int" && lptypes[[param$name]] %nin% c("integer", "integervector")) ||
          (param$type == "bool" && lptypes[[param$name]] %nin% c("logical", "logicalvector")) ||
          (param$type == "cat" &&
            lptypes[[param$name]] %nin% c("discrete", "discretevector", "character", "charactervector"))) {
        warningf("Parameter '%s' for learner '%s' is of type '%s' and has different (but feasible) type '%' listed in search space.",
            param$name, l$id, lptypes[[param$name]], param$type)
      }
      if (hasRequires(lp$pars[[param$name]]) && is.null(param$req)) {
        warningf("Parameter '%s' for learner '%s' has a 'requires' argument but the one given in the search space has not.",
            param$name, l$id)
      }
    }
    if (param$type == "def") {
      # check whether this is /actually/ the default
      truedefault = getDefaults(getParamSet(l))[[param$name]]
      defaultcandidate = getHyperPars(l)[[param$name]]
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
    if (param$type == "fix") {
      assignment = list(rep(param$values, param$dim))
      names(assignment) = param$name
      l = setHyperPars(l, par.vals=assignment)
    } else if (param$type != "def") {
      tuneSearchSpace = c(tuneSearchSpace, createParameter(param))
    }
  }
  list(tss=makeParamSet(params=tuneSearchSpace), l=l)
}

allfeasible = function(ps, totest, name, dimension) {
  testlist = list(0)
  names(testlist) = name
  for (t in totest) {
    testlist[[1]] = rep(t, dimension)
    if (!isFeasible(ps, testlist)) {
      return(FALSE)
    }
  }
  TRUE
}

createParameter = function(param) {
  if (param$type %in% c("int", "real")) {
    pmin = param$values[0]
    pmax = param$values[1]
  }
  if (param$trafo == "exp") {
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
  switch(param$type,
      real=makeNumericVectorParam(param$name, param$dim, pmin, pmax, trafo=ptrafo, requires=param$req),
      int=makeIntegerVectorParam(param$name, param$dim, pmin, pmax, trafo=ptrafo, requires=param$req),
      cat=makeDiscreteVectorParam(param$name, param$dim, param$values, requires=param$req),
      bool=makeLogicalVectorParam(param$name, param$dim, requires=param$req),
      # the possibilities 'fix' and 'def' are only reached if we are creating a dummy parameter
      fix={  # for whatever reason one would want this...
        warningf("Parameter '%s' for learner '%s is marked dummy and has type 'fix'; This usually does not make sense.",
            param$name, l$id)
        makeDiscreteVectorParam(param$name, param$dim, param$values, requires=param$req)
      },
      def={
        stopf("Parameter '%s' for learner '%s' is marked as dummy must not have type 'def'.",
            param$name, l$id)
      }, stopf("Unknown type '%s'; parameter '%s', learner '%s'", param$type, param$name, l$id))
}

#' 
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
    ratio = sqrt((min+1) / min)
    sequence = unique(c(min * ratio ^ (seq(from=0, to=floor(log(max, base=ratio)))), max))
    return(list(trafo=function(x) sequence[x], newmin=1, newmax=length(sequence)))
  } else {
    return(list(trafo=function(x) min * (max / min)^x, newmin=0, newmax=1))
  }
}

#' Like mlr's mMMPS but respecting requirements.
#'
#' @param multiplexer the model multiplexer to use to create the ParamSet
#' @param modelParsets the list of param sets that are used to create the mmps.
makeModelMultiplexerParamSetEx = function(multiplexer, modelParsets) {
  searchspace = do.call(makeModelMultiplexerParamSet, c(list(multiplexer), modelParsets))
  # now we need to deal with the bug that makeModelMultiplexer overrides requirements
  for (modeliter in seq_along(modelParsets)) {
    origpars = modelParsets[[modeliter]]$pars
    modelid = names(modelParsets)[modeliter]
    oldnames = names(origpars)  # the names as in the original model
    newnames = paste(modelid, oldnames, sep='.')  # the name that was assigned in mm$searchspace
    substitution = lapply(newnames, asQuoted)  # substitution is a list(oldname=quote(newname))
    names(substitution) = oldnames
    for (paramiter in seq_along(origpars)) {  # iterate over the parameters in each ParamSet
      cp = origpars[[paramiter]]
      cpname = names(origpars)[paramiter]
      cprequires = cp$requires
      newname = paste(modelid, cpname, sep='.') # the name fo the current Param within mm$searchspace
      newrequires = searchspace$pars[[newname]]$requires
      if (is.null(cprequires)) {
        # if there is no '$requires' we don't need to do anything
        next
      }
      # the following line is a bit of R magic. Use do.call, so that cprequires, which is a
      # 'quote' object, is expanded to its actual content. The 'substitute' call will change all
      # names of the old parameters to the new parameters.
      cprequires = do.call(substitute, list(cprequires, substitution))
      newrequires = substitute((a) && (b), list(a=newrequires, b=cprequires))
      # at this position, newrequires has the form
      # (new requires) && (old requires)
      # where the use of short-cirquiting && should solve any problems that we might get when querying isFeasible.
      searchspace$pars[[newname]]$requires = newrequires
    }
  }
  searchspace
}
