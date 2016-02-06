

#' Take a list of autolearners and create a (big) mlr Learner object
#' 
#' @param searchspace list of autolearners.
buildLearners = function(searchspace) {
  # ok, this will be a big project. what do we need?
  # [ ] load the mlr learners if an id is given. I think mlr has an aux function for that
  # [ ] filter out learners by what is needed, e.g.
  #   [ ] check if the learner can handle the input data format (numeric, factorial)
  #   [ ] check if there is a wrapper that turns the input data into appropriate format (e.g. removes factors)
  #   [ ] (advanced) filter out classif/reg by task type
  # [ ] create the appropriate search space
  #   [ ] for default, check if the given defaults are actually the builtin defaults; warn if not and add "our" default as fixed value
  #   [ ] for fixed, set the fixed value
  #   [ ] for variable search space, add the parameter to the search space
  #     [ ] warn if there is a requirement in the builtin parameter but not ours?
  #     [ ] do a simple hack for dummy variables (check it is actually a dummy!)
  #     [ ] respect the transformation if required, respecting int / real types
  #     [ ] check that given parameter is the right type as the builtin type
  #     [ ] check that given range is within the builtin range / given categories are feasible
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
}


buildLearners.old = function(searchspace) {
  learners = searchspace[extractSubList(searchspace, "stacktype") == "learner"]
  onlyModels = extractSubList(learners, "learner", simplify=FALSE)
  modelParsets = extractSubList(learners, "searchspace", simplify=FALSE)
  names(modelParsets) = extractSubList(onlyModels, "id")
  mm = makeModelMultiplexer(onlyModels)
  mm$searchspace = do.call(makeModelMultiplexerParamSet, c(list(mm), modelParsets))

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
      newrequires = mm$searchspace$pars[[newname]]$requires
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
      mm$searchspace$pars[[newname]]$requires = newrequires
    }
  }
  mm
}
