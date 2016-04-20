


#' @title Create an mlr learner that limits the runtime of a train/predict
#'   cycle.
#' 
#' @description
#' When max.learner.time, given to automlr, is overrun, we want to either give
#' an error or a dummy learner.
#' An error should be given either if the resampling makes only one iterration,
#' or if the first iterration overruns its time by a large amount (10%?).
#' If the first resampling was an error, the other resamplings should also give
#' errors without starting the run. Otherwise they should themselves run (with
#' the correct timeout). If any run goes over budget and does not give an error,
#' it should return a trivial learner that predicts the majority.
#' 
#' @param learner [\code{Learner}]\cr
#'   The learner to be wrapped.
#' @param time [\code{numeric(1)}]\cr
#'   The maximum runtime, for \code{train} and \code{predict} \emph{combined},
#'   after which to abort.
#' @param timeFirstIter [\code{numeric(1)}]\cr
#'   If given, this is the maximum runtime for the first iteration in a
#'   resampling, given that (1) the evaluation happens inside \code{resample}
#'   and (2) the resampling will go through more than one iteration and (3) the
#'   the resampling is not being parallelized.
#' 
#' @return [\code{TimeconstraintWrapper}]
#' A \code{Learner} that runs only for the given time and will then return
#' either an error or a dummy learner.
makeTimeconstraintWrapper = function(learner, time, timeFirstIter = NULL) {

  constructor = switch(learner$type,
      classif = makeRLearnerClassif,
      regr = makeRLearnerRegr,
      surv = makeRLearnerSurv,
      multilabel = makeRLearnerMultilabel,
      stopf("Learner type '%s' not supported.", taskdesc$type))
  wrapper = constructor(
      cl = "TimeconstraintWrapper",
      short.name = "tcw",
      name = "TimeconstraintWrapper",
      properties = getLearnerProperties(properties),
      par.set = getParamSet(learner),
      par.vals = getHyperPars(learner),
      package = "automlr")
  wrapper$fix.factors.prediction = learner$fix.factors.prediction
  
  wrapper$learner = learner
  wrapper$env = new.env(parent = emptyenv())

  wrapper$time = time
  wrapper$timeFirstIter = timeFirstIter

  wrapper$env$untouched = TRUE
}

trainLearner.TimeconstraintWrapper = function(.learner, .task, .subset,
    .weights = NULL, ...) {
  
  # if communication works and we are in the >1. iteration of a resampling
  # and the first round failed with > timeFirstIter -> error.
  
  # if this is a resampling > 1 round and it is being parallelized -> run with
  # normal timeout; return dummy learner if overtime.
  
  # if this is a resampling > 1 round and it is not being parallelized:
  #  1.st round run with timeFirstIter timeout, 
  
  # reset hyperpars
  # set hyperpars to ...
  
  
  result = train(.learner$learner, task = .task, subset = .subset,
      weights = .weights)
}

predictLearner.TimeconstraintWrapper = function(.learner, .model, .newdata,
    ...) {
  
}