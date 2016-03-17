test_that("irace handles all kinds of parameters and their requirements", {

  tf = c(TRUE, FALSE)
  names(tf) = !tf  # maybe this will cause some confusion..

  # a bogus learner with exotic hyperparameters
  bogusClassif = makeRLearnerClassif("bogusClassif", character(0),
      makeParamSet(makeIntegerLearnerParam("useVectors", 0, 1, when="both"),
                   makeIntegerLearnerParam("int", -10, 10, when="predict", requires=quote(useVectors==0)),
                   makeIntegerVectorLearnerParam("intv", 3, -10, 10, when="both", requires=quote(useVectors==1)),
                   makeNumericLearnerParam("num", -10, 10, when="both", requires=quote(useVectors==0)),
                   makeNumericVectorLearnerParam("numv", 3, -10, 10, when="predict", requires=quote(useVectors==1)),
                   makeLogicalLearnerParam("log", when="predict", requires=quote(useVectors==0)),
                   makeLogicalVectorLearnerParam("logv", 3, when="both", requires=quote(useVectors==1)),
                   makeDiscreteLearnerParam("disc1", c("a", "b", "c"), when="both", requires=quote(useVectors==0)),  # easy: character discrete params
                   makeDiscreteVectorLearnerParam("disc1v", 3, c("a", "b", "c"), when="predict", requires=quote(useVectors==1)),
                   makeDiscreteLearnerParam("disc2", tf, when="predict", requires=quote(useVectors==0)),  # harder: booleans
                   makeDiscreteVectorLearnerParam("disc2v", 3, tf, when="both", requires=quote(useVectors==1)),
                   makeDiscreteLearnerParam("disc3", c(3, 10), when="both", requires=quote(useVectors==0)),  # also harder: numeric
                   makeDiscreteVectorLearnerParam("disc3v", 3, c(3, 10), when="predict", requires=quote(useVectors==1)),
                   # challenging: mixed types
                   makeDiscreteLearnerParam("disc4", list(`3`=3, `TRUE`="TRUE", `FALSE`=TRUE, li=list(TRUE, FALSE), fun=function() TRUE), when="both", requires=quote(useVectors==0)),  
                   makeDiscreteVectorLearnerParam("disc4v", 3, list(`3`=3, `TRUE`="TRUE", `FALSE`=TRUE, li=list(TRUE, FALSE)), when="predict", requires=quote(useVectors==1))),
      properties=c("twoclass", "numerics", "factors", "ordered", "missings"))
  bogusClassif$fix.factors.prediction = TRUE

  trainLearner.bogusClassif = function(.learner, .task, .subset, .weights=NULL, ...) {
    list()
  }
  predictLearner.bogusClassif = function(.learner, .model, .newdata, useVectors,
      int, intv, num, numv, log, logv, disc1, disc1v, disc2, disc2v, disc3, disc3v, disc4, disc4v, ...) {
    if (useVectors == 0) {
      expect_numeric(int, len=1, any.missing=FALSE)
      expect_equal(round(int), int)
      expect_numeric(num, len=1, any.missing=FALSE)
      expect_logical(log, len=1, any.missing=FALSE)
      expect_character(disc1, len=1, any.missing=FALSE)
      expect_logical(disc2, len=1, any.missing=FALSE)
      expect_numeric(disc3, len=1, any.missing=FALSE)
      expect_true(is.function(disc4) || identical(disc4, list(TRUE, FALSE)) || disc4 == "TRUE" || disc4 == 3)
      bar = mean(c(int > 0, num > 0, log == TRUE, disc1 == "a", disc2 == TRUE, disc3 == 3))      
    } else {
      expect_true(useVectors == 1)
      expect_numeric(intv, len=3, any.missing=FALSE)    
      expect_numeric(numv, len=3, any.missing=FALSE)
      expect_logical(logv, len=3, any.missing=FALSE)
      expect_list(disc1v, types="character", any.missing=FALSE, len=3)
      expect_list(disc2v, types="logical", any.missing=FALSE, len=3)
      expect_list(disc3v, types="numeric", any.missing=FALSE, len=3)
      expect_list(disc4v, any.missing=FALSE, len=3)
      for (d4v in disc4v) {
        expect_true(identical(d4v, list(TRUE, FALSE)) || d4v == "TRUE" || d4v == 3)
      }
      bar = mean(c(intv[1] + intv[2] + intv[3] > 0,
          mean(numv), logv[1] && logv[2] || logv[3],
          disc1v[[1]] == disc1v[[2]], 
          disc2v[[1]] && disc2v[[2]], disc3v[[1]] * disc3v[[2]] > 10))      
    }
    factor(.model$factor.levels[[1]][1 + (runif(nrow(.newdata)) > bar)])
  }
  ps = getParamSet(bogusClassif)
  ctrl = makeTuneControlIrace(maxExperiments = 20L, nbIterations=1L, minNbSurvival=1L)
  res = tuneParams(bogusClassif, pid.task, hout, par.set = ps, control = ctrl)
  res = tuneParams(bogusClassif, sonar.task, hout, par.set = ps, control = ctrl)
  res = tuneParams(bogusClassif, bc.task, hout, par.set = ps, control = ctrl)

})

  
