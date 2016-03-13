

context("mbo")

###
# Test 'easy' nofailSearchSpace

test_that("backend 'mbo' works with basic search space", {
  backendToTest = "mbo"
  searchSpaceToTest = nofailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

###
# Test withFailSearchSpace

test_that("backend 'mbo' works with search space with model failures allowed", {
  backendToTest = "mbo"
  searchSpaceToTest = withFailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

###
# Test withPPSearchSpace, including a wrapper

test_that("backend 'mbo' works with search space with preprocessing and model failure", {
  backendToTest = "mbo"
  searchSpaceToTest = withPPSearchSpace
  checkBackend(searchSpaceToTest, backendToTest, thorough = TRUE)
})

###
# Test paramtestSearchSpace, with parameters of different types

test_that("backend 'mbo' works with search space with various parameter types", {
  backendToTest = "mbo"
  searchSpaceToTest = paramtestSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

###
# Test reqstestSearchSpace, with complicated parameter requirements

test_that("backend 'mbo' works with search space with requirements", {
  backendToTest = "mbo"
  searchSpaceToTest = reqstestSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})