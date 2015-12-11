

buildLearners = function(searchspace) {
  learners = searchspace[extractSubList(searchspace, "stacktype") == "learner"]
  onlyModels = extractSubList(learners, "learner", simplify=FALSE)
  modelParsets = extractSubList(onlyModels, "par.set", simplify=FALSE)
  names(modelParsets) = extractSubList(onlyModels, "id")
  mm = makeModelMultiplexer(onlyModels)
  mm$par.set = do.call(makeModelMultiplexerParamSet, c(list(mm), modelParsets))
}
