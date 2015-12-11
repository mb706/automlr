

buildLearners = function(searchspace) {
  learners = searchspace[extractSubList(searchspace, "stacktype") == "learner"]
  onlyModels = extractSubList(learners, "learner", simplify=FALSE)
  modelParsets = extractSubList(learners, "searchspace", simplify=FALSE)
  names(modelParsets) = extractSubList(onlyModels, "id")
  mm = makeModelMultiplexer(onlyModels)
  mm$searchspace = do.call(makeModelMultiplexerParamSet, c(list(mm), modelParsets))
}
