

context("automlrstress")


test_that("backend 'random' works with basic search space", {
  backendToTest = "random"
  searchSpaceToTest = nofailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

test_that("backend 'irace' works with basic search space", {
  backendToTest = "irace"
  searchSpaceToTest = nofailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

test_that("backend 'mbo' works with basic search space", {
  backendToTest = "mbo"
  searchSpaceToTest = nofailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

test_that("backend 'random' works with search space with model failures allowed", {
  backendToTest = "random"
  searchSpaceToTest = withFailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

test_that("backend 'irace' works with search space with model failures allowed", {
  backendToTest = "irace"
  searchSpaceToTest = withFailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

test_that("backend 'mbo' works with search space with model failures allowed", {
  backendToTest = "mbo"
  searchSpaceToTest = withFailSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

test_that("backend 'random' works with search space with preprocessing and model failure", {
  backendToTest = "random"
  searchSpaceToTest = withPPSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

test_that("backend 'irace' works with search space with preprocessing and model failure", {
  backendToTest = "irace"
  searchSpaceToTest = withPPSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})

test_that("backend 'mbo' works with search space with preprocessing and model failure", {
  backendToTest = "mbo"
  searchSpaceToTest = withPPSearchSpace
  checkBackend(searchSpaceToTest, backendToTest)
})
