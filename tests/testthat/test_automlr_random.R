

context("random")

###
# Test 'easy' nofailSearchSpace

test_that("backend 'random' works with basic search space", {
  source("helper_automlrhelpers.R")
  backendToTest = "random"
  searchSpaceToTest = nofailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

###
# Test withFailSearchSpace

test_that("backend 'random' works with search space with model failures allowed", {
  source("helper_automlrhelpers.R")
  backendToTest = "random"
  searchSpaceToTest = withFailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest, learnersMayFail = TRUE)
})

###
# Test withPPSearchSpace, including a wrapper

test_that("backend 'random' works with search space with preprocessing and model failure", {
  source("helper_automlrhelpers.R")
  backendToTest = "random"
  searchSpaceToTest = withPPSearchSpace
  checkBackend(searchSpaceToTest, backendToTest, thorough = TRUE,
    learnersMayFail = TRUE)
})

###
# Test paramtestSearchSpace, with parameters of different types

test_that("backend 'random' works with search space with various parameter types", {
  source("helper_automlrhelpers.R")
  backendToTest = "random"
  searchSpaceToTest = paramtestSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

###
# Test reqstestSearchSpace, with complicated parameter requirements

test_that("backend 'random' works with search space with requirements", {
  source("helper_automlrhelpers.R")
  backendToTest = "random"
  searchSpaceToTest = reqstestSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

###
# Test the optRandom faux error machinery

test_that("backend 'random' generates credible error objects", {

  .model = list(task.desc=list(class.levels=c('a', 'b', 'c')))
  for (t in c('classif', 'regr', 'surv', 'multilabel')) {
    for (p in c('prob', 'response', 'se')) {
      if (t == 'surv' && p == 'prob') {
        next
      }
      .learner = list(type=t, predict.type=p)
      res <- mlr:::checkPredictLearnerOutput(.learner, .model, automlr:::createDummyError(.learner, .model))
      expect_true(is.error(res))
      expect_equal(as.character(res), automlr:::timeout.string)
    }
  }

})
