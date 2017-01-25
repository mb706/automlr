
# The Sys.sleep() function ignores the 
interruptibleSleep = function(time) {
  for (i in seq_len(ceiling(time * 10))) {
    Sys.sleep(0.1)
  }
}

# A learner that takes different amounts of time, depending on the parameters.
slowLearner = makeRLearnerClassif("slowLearner", character(0),
    makeParamSet(
        makeNumericVectorLearnerParam("trainlag", len = 5,
            lower = 0, upper = 10, when = "train"),
        makeNumericVectorLearnerParam("predictlag", len = 5,
            lower = 0, upper = 10, when = "predict"),
        makeNumericLearnerParam("rate", lower = 0, upper = 1,
            when = "predict")),
    properties = c("twoclass", "numerics", "missings"))
slowLearner$fix.factors.prediction = TRUE
trainLearner.slowLearner = function(.learner, .task, .subset, .weights = NULL,
    trainlag, ...) {
  interruptibleSleep(trainlag[coalesce(getResampleIter(), 1)])
  NULL
}
predictLearner.slowLearner = function(.learner, .model, .newdata, predictlag,
    rate, ...) {
  interruptibleSleep(predictlag[coalesce(getResampleIter(), 1)])
  factor(.model$factor.levels[[1]][1 + rbinom(nrow(.newdata), 1, rate)])
}



ccAL = autolearner(slowLearner, list(
        sp("rate", "real", c(0, 1))))

# test: no timeout, timeout on late (but not early) ri,
# timeout on early (but not late) ri, constant timeout

# walltime overrun without exceeding max.walltime.overrun,
# walltime overrun with exceeding max.walltime.overrun

