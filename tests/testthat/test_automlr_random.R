

context("random")

###
# Test 'easy' nofailSearchSpace

test_that("backend 'random' works with basic search space", {
  backendToTest = "random"
  searchSpaceToTest = nofailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

###
# Test withFailSearchSpace

test_that("backend 'random' works with search space with model failures allowed", {
  backendToTest = "random"
  searchSpaceToTest = withFailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

###
# Test withPPSearchSpace, including a wrapper

test_that("backend 'random' works with search space with preprocessing and model failure", {
  backendToTest = "random"
  searchSpaceToTest = withPPSearchSpace
  checkBackend(searchSpaceToTest, backendToTest, thorough = TRUE)
})

###
# Test paramtestSearchSpace, with parameters of different types

test_that("backend 'random' works with search space with various parameter types", {
  backendToTest = "random"
  searchSpaceToTest = paramtestSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

###
# Test reqstestSearchSpace, with complicated parameter requirements

test_that("backend 'random' works with search space with requirements", {
  backendToTest = "random"
  searchSpaceToTest = reqstestSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})
