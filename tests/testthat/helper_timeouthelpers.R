
# The Sys.sleep() function ignores the
interruptibleSleep = function(time) {
  for (i in seq_len(ceiling(time * 10))) {
    Sys.sleep(0.1)
  }
}

# A learner that takes different amounts of time, depending on the parameters.
slowLearner = makeRLearnerClassif("slowLearner", character(0),
  makeParamSet(
      makeUntypedLearnerParam("env", default = NULL, when = "both"),
      makeNumericVectorLearnerParam("trainlag", len = 5,
        lower = 0, upper = 10, when = "train"),
      makeNumericVectorLearnerParam("predictlag", len = 5,
        lower = 0, upper = 10, when = "predict"),
      makeNumericLearnerParam("rate", lower = 0, upper = 1,
        when = "predict")),
  properties = c("twoclass", "numerics", "missings"))
slowLearner$fix.factors.prediction = TRUE
trainLearner.slowLearner = function(.learner, .task, .subset, .weights = NULL,
    trainlag, env, ...) {
  if (missing(env) || is.null(env) || env$sleep) {
    interruptibleSleep(trainlag[coalesce(getResampleIter(), 1)])
  }
  NULL
}
predictLearner.slowLearner = function(.learner, .model, .newdata, predictlag,
    rate, env, ...) {
  if (missing(env) || is.null(env) || env$sleep) {
    interruptibleSleep(predictlag[coalesce(getResampleIter(), 1)])
  }
  factor(.model$factor.levels[[1]][1 + rbinom(nrow(.newdata), 1, rate)])
}

registerS3method("trainLearner", "slowLearner", trainLearner.slowLearner)
registerS3method("predictLearner", "slowLearner", predictLearner.slowLearner)

######################## TODO: make getResampleIter smarter (recognize last 'resample' call and first 'train' / 'predict' call after that)


slAL = function(learner) autolearner(learner, list(sp("rate", "real", c(0, 1))))


# walltime overrun without exceeding max.walltime.overrun,
# walltime overrun with exceeding max.walltime.overrun
