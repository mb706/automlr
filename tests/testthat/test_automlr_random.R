

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
  checkBackend(searchSpaceToTest, backendToTest, thorough = TRUE, learnersMayFail = TRUE)
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
