
#' @title Create an mlr learner that catches errors and replaces them with the
#' result of the simplest possible learner.
#'
#' @description
#' When a learner throws an error, its performance is treated by mlr as a
#' "missing value". This behaviour is not always optimal, especially for tuning
#' purposes, since a learner that sometimes fails but otherwise gives good
#' results could still be better than a learner that never fails but gives
#' awful results.
#' 
#' @param learner [\code{Learner}]\cr
#'   The learner to be wrapped.
#' 
#' @return [\code{FailImputationWrapper}]
#' A \code{Learner} that catches errors and returns a dummy model if an error
#' occurs.
makeFailImputationWrapper = function(learner) {
  wrapLearner("FailImputationWrapper", "fiw", "FailImputationWrapper", learner)
}

#' @export
trainLearner.FailImputationWrapper = function(.learner, .task, .subset,
    .weights = NULL, ...) {
  learner = setHyperPars(.learner$learner, par.vals = list(...))
  trivialTask = dropFeatures(.task, getTaskFeatureNames(.task))
  trivialModel = train(learner, task = trivialTask, subset = .subset,
      weights = .weights)
  model = try(train(learner, task = .task, subset = .subset,
          weights = .weights), silent = TRUE)
  if (is.error(model) || isFailureModel(model)) {
    model = NULL
  }
  result = list(model = model, trivialModel = trivialModel)
}

#' @export
predictLearner.FailImputationWrapper = function(.learner, .model, .newdata,
    ...) {
  result = NULL
  if (!is.null(.model$learner.model$model)) {
    result = try(getPredictionResponse(
            stats::predict(.model$learner.model$model, newdata = .newdata)),
        silent = TRUE)
    if (is.error(result) || all(is.na(result))) {
      result = NULL
    }
  }
  if (is.null(result)) {
    getPredictionResponse(stats::predict(.model$learner.model$trivialModel,
            newdata = .newdata))
  } else {
    result
  }
}
