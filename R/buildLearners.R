

buildLearners = function(searchspace) {
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
