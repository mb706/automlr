

context("irace")

###
# Test 'easy' nofailSearchSpace

test_that("backend 'irace' works with basic search space", {
  backendToTest = "irace"
  searchSpaceToTest = nofailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

###
# Test withFailSearchSpace

test_that("backend 'irace' works with search space with model failures allowed", {
  backendToTest = "irace"
  searchSpaceToTest = withFailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

###
# Test withPPSearchSpace, including a wrapper

test_that("backend 'irace' works with search space with preprocessing and model failure", {
  backendToTest = "irace"
  searchSpaceToTest = withPPSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

###
# Test paramtestSearchSpace, with parameters of different types

test_that("backend 'irace' works with search space with various parameter types", {
  backendToTest = "irace"
  searchSpaceToTest = paramtestSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

###
# Test reqstestSearchSpace, with complicated parameter requirements

test_that("backend 'irace' works with search space with requirements", {
  backendToTest = "irace"
  searchSpaceToTest = reqstestSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})
