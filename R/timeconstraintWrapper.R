


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
#' it should return a trivial learner that predicts a constant.
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

  # time should be at least one second; otherwise there might be trouble when
  # setting timeout too low since setTimeLimit(Inf) might fail...
  assertNumeric(time, lower = 1, any.missing = FALSE, len = 1)
  if (!is.null(timeFirstIter)) {
    assertNumeric(timeFirstIter, lower = time, any.missing = FALSE, len = 1)
  }
  
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
      properties = getLearnerProperties(learner),
      par.set = getParamSet(learner),
      par.vals = getHyperPars(learner),
      package = "automlr")
  wrapper$fix.factors.prediction = learner$fix.factors.prediction
  
  wrapper$learner = removeHyperPars(learner, names(getHyperPars(learner)))
  wrapper$env = new.env(parent = emptyenv())

  wrapper$time = time
  wrapper$timeFirstIter = timeFirstIter

  wrapper$env$untouched = TRUE
  wrapper$env$wasError = FALSE
  wrapper
}

trainLearner.TimeconstraintWrapper = function(.learner, .task, .subset,
    .weights = NULL, ...) {

  learner = setHyperPars(.learner$learner, par.vals = list(...))

  # the "normal" behaviour, when not doing resampling, or when doing resampling
  # with only one iteration
  runinfo = list(
      specialFirstIter = FALSE,
      time = .learner$time,
      ontimeout = "error",
      dummyModel = NULL,
      traintime = NULL,
      finished = NULL)

  if (isInsideResampling() && getResampleMaxIters() > 1) {
    if (isResampleParallel()) {
      # if doing resampling in parallel, all iterations are treated the same.
      # on error, an imputed value is generated.
      runinfo$ontimeout = "dummy"
    } else {
      if (isFirstResampleIter()) {
        runinfo$time = coalesce(.learner$timeFirstIter, .learner$time)
        runinfo$specialFirstIter = TRUE
      } else {
        if (.learner$env$untouched) {
          # if we are not parallelized and are in the >1st iteration. therefore,
          # if env is untouched, some error happened.
          stop("TimeconstraintWrapper communication by environment failed.")
        }
        if (.learner$env$wasError) {
          # if the first iteration timed out we abort all the other iterations
          # right away.
          stop("First resampling run was timeout.")
        }
        runinfo$ontimeout = "dummy"
      }
    }
  } else {
    .learner$env$untouched = TRUE
    .learner$env$wasError = FALSE
  }
  assert(runinfo$ontimeout %in% c("error", "dummy"))
  if (runinfo$ontimeout == "dummy" || runinfo$specialFirstIter) {
    # we also need the dummyModel for the specialFirstIter, since it might
    # default to a dummy prediction.
    trivialTask = dropFeatures(.task, getTaskFeatureNames(.task))
    runinfo$dummyModel = train(learner, task = trivialTask, subset = .subset,
        weights = .weights)
  }

  if (runinfo$specialFirstIter) {
    .learner$env$untouched = FALSE
    # set wasError to TRUE and set it to FALSE after train() again. It will not
    # be reached if an error occurs.
    .learner$env$wasError = TRUE
  }
  
  result = list()
  exec.time = system.time({
        runinfo$finished <- runWithTimeout({
              result <- train(learner, task = .task, subset = .subset,
                  weights = .weights)
            }, runinfo$time, runinfo$ontimeout == "error")
      }, gcFirst = FALSE)

  runinfo$traintime = exec.time[3]
  
  if (runinfo$finished && runinfo$traintime > runinfo$time + 0.5) {
    # we allow ourselves 0.5 seconds buffer or bad things might happen.
    # Putting the 0.5 into runWithTimeout won't do, since exec.time might be
    # more than the timeout value due to overhead.
    if (runinfo$ontimeout == "error") {
      stop(timeoutMessage)
    } else {
      runinfo$finished = FALSE
      result = list()
    }
  }

  .learner$env$wasError = FALSE
  
  result$runinfo = runinfo
  result
}

predictLearner.TimeconstraintWrapper = function(.learner, .model, .newdata,
    ...) {
  runinfo = .model$learner.model$runinfo
  if (runinfo$finished) {
    # we go here if the training run finished without timeout
    remainingTime = runinfo$time - runinfo$traintime
    
    if (runinfo$specialFirstIter) {
      .learner$env$wasError = TRUE
    }

    exec.time = system.time({
          finished <- runWithTimeout({
                result <- getPredictionResponse(predict(.model$learner.model,
                        newdata = .newdata))
              }, remainingTime, runinfo$ontimeout == "error")
        }, gcFirst = FALSE)
    predicttime = exec.time[3]
    totalTime = predicttime + runinfo$traintime

    .learner$env$wasError = FALSE

    if (runinfo$specialFirstIter && totalTime > .learner$time) {
      # on the first 'special' run, we might be below the timeFirstIter timeout
      # value, but still above the regular timeout value. In that case, we 
      # treat the run as if it did produce a timeout on a subsequent resampling
      # iteration and do the dummy prediction. 
      finished = FALSE
    }
    if (finished) {
      return(result)
    }
  }
  assert(runinfo$ontimeout == "dummy" || runinfo$specialFirstIter)
  getPredictionResponse(predict(runinfo$dummyModel, newdata = .newdata))
}

getSearchspace.TimeconstraintWrapper = function(learner) {
  getSearchspace(learner$learner)
}




