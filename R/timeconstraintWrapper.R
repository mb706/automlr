
#' @title Create an mlr learner that limits the runtime of a train/predict
#'   cycle.
#' 
#' @description
#' When max.learner.time, given to automlr, is overrun, we want to give an
#' error.
#' If the first resampling was an error, the other resamplings should also give
#' errors without starting the run. Otherwise they should themselves run (with
#' the correct timeout). 
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
  
  wrapper = wrapLearner("TimeconstraintWrapper", "tcw", "TimeconstraintWrapper",
      learner = learner)
  
  wrapper$env = new.env(parent = emptyenv())

  wrapper$time = time
  wrapper$timeFirstIter = timeFirstIter

  wrapper$env$resampleUID = -1
  wrapper$env$firstResampleError = FALSE
  wrapper
}

#' @export
trainLearner.TimeconstraintWrapper = function(.learner, .task, .subset,
    .weights = NULL, ...) {

  .learner$learner = setHyperPars(.learner$learner, par.vals = list(...))

  # the "normal" behaviour, when not doing resampling, or when doing resampling
  # with only one iteration
  runinfo = list(
      specialFirstIter = FALSE,
      time = .learner$time,
      dummyModel = NULL,
      traintime = NULL)

  if (isInsideResampling() &&
      getResampleMaxIters() > 1 &&
      !isResampleParallel()) {
    # (if doing resampling in parallel, all iterations are treated the same.)
    if (isFirstResampleIter()) {
      runinfo$time = coalesce(.learner$timeFirstIter, .learner$time)
      runinfo$specialFirstIter = TRUE
      .learner$env$resampleUID = setResampleUID()
    } else {
      if (.learner$env$resampleUID != getResampleUID()) {
        # we are not parallelized and are in the >1st iteration. therefore,
        # if env is untouched, some error happened.
        stop("TimeconstraintWrapper communication by environment failed.")
      }
      if (.learner$env$firstResampleError) {
        # if the first iteration timed out we abort all the other iterations
        # right away.
        stop("First resampling run was timeout.")
      }
    }
  }
  
  .learner$env$firstResampleError = FALSE

  exec.time = system.time(result <- runWithTimeout(NextMethod("trainLearner"),
          runinfo$time, FALSE), gcFirst = FALSE)

  runinfo$traintime = exec.time[3]
  
  if (result$timeout || runinfo$traintime > runinfo$time + 0.5) {
    # we allow ourselves 0.5 seconds buffer or bad things might happen.
    if (runinfo$specialFirstIter) {
      .learner$env$firstResampleError = TRUE
    }
    stop("TimeoutWrapper Timeout")
  }

  result = result$result

  result$runinfo = runinfo
  result
}

#' @export
predictLearner.TimeconstraintWrapper = function(.learner, .model, .newdata,
    ...) {
  runinfo = .model$learner.model$runinfo
  # we go here if the training run finished without timeout
  remainingTime = max(runinfo$time - runinfo$traintime, 1)
  

  exec.time = system.time(result <- runWithTimeout(NextMethod("predictLearner"),
          remainingTime, FALSE), gcFirst = FALSE)
  predicttime = exec.time[3]
  totalTime = predicttime + runinfo$traintime
  
  if (result$timeout && runinfo$specialFirstIter) {
    .learner$env$firstResampleError = TRUE
  }

  if (result$timeout ||
      (runinfo$specialFirstIter && totalTime > .learner$time)) {
    # on the first 'special' run, we might be below the timeFirstIter timeout
    # value, but still above the regular timeout value. In that case, we 
    # treat the run as if it did produce a timeout on a subsequent resampling
    # iteration and do the dummy prediction. 
    stop("TimeoutWrapper Timeout")
  }
  result$result
}





