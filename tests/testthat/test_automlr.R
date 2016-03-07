context("automlr")

test_that("error on budget misclassification", {
  #the following is backend independent
  backendToTest = "random"

  amobject = automlr(theTask, searchspace = nofailSearchSpace, backend = backendToTest)
  expect_set_equal(amobject$spent, c(0, 0, 0, 0))
  amfile = tempfile()
  fileboundObject = automlr(theTask, savefile = amfile, budget = c(evals = 1), searchspace = nofailSearchSpace, backend = backendToTest)

  expect_class(automlr(theTask, searchspace = nofailSearchSpace, backend = backendToTest, budget = 0), c("AMObject", "AMState"))
  expect_error(automlr(theTask, searchspace = nofailSearchSpace, backend = backendToTest, budget = 1), "Assertion on 'budget' failed")
  expect_error(automlr(theTask, searchspace = nofailSearchSpace, backend = backendToTest, budget = -1), "Assertion on 'budget' failed")
  expect_error(automlr(theTask, searchspace = nofailSearchSpace, backend = backendToTest, budget = c(test = 1)), "Assertion on 'budgetNamesOk' failed")
  expect_error(automlr(theTask, searchspace = nofailSearchSpace, backend = backendToTest, budget = c(walltime = -1)), "Assertion on 'budget' failed")
  expect_class(automlr(amobject, budget = 0), c("AMObject", "AMState"))
  expect_error(automlr(amobject, budget = 1), "Assertion on 'budget' failed")
  expect_error(automlr(amobject, budget = -1), "Assertion on 'budget' failed")
  expect_error(automlr(amobject, budget = c(test = 1)), "Assertion on 'budgetNamesOk' failed")
  expect_error(automlr(amobject, budget = c(walltime = -1)), "Assertion on 'budget' failed")
  expect_class(automlr(amfile, budget = 0), c("AMObject", "AMState"))
  expect_error(automlr(amfile, budget = 1), "Assertion on 'budget' failed")
  expect_error(automlr(amfile, budget = -1), "Assertion on 'budget' failed")
  expect_error(automlr(amfile, budget = c(test = 1)), "Assertion on 'budgetNamesOk' failed")
  expect_error(automlr(amfile, budget = c(walltime = -1)), "Assertion on 'budget' failed")
  # test error when bad searchspace given
  expect_class(automlr(theTask, searchspace = nofailSearchSpace, backend = backendToTest), c("AMObject", "AMState"))
  expect_error(automlr(theTask, searchspace = do.call(base::c, nofailSearchSpace), backend = backendToTest), "Assertion on 'searchspace' failed")
  expect_error(automlr(theTask, searchspace = list(), backend = backendToTest), "Assertion on 'searchspace' failed")
  # test error when bad savefile given
  expect_class(automlr(theTask, searchspace = nofailSearchSpace, backend = backendToTest), c("AMObject", "AMState"))
  expect_error(automlr(theTask, searchspace = nofailSearchSpace, backend = backendToTest, savefile = 3), "Assertion on 'savefile' failed")
  expect_error(automlr(theTask, searchspace = nofailSearchSpace, backend = backendToTest, savefile = ""), "Assertion on 'nchar\\(filename\\)' failed")
  expect_error(automlr(amobject, savefile = 3), "Assertion on 'savefile' failed")
  expect_error(automlr(amobject, savefile = ""), "Assertion on 'nchar\\(filename\\)' failed")
  expect_error(automlr(amfile, savefile = 3), "Assertion on 'savefile' failed")
  expect_error(automlr(amfile, savefile = ""), "Assertion on 'nchar\\(filename\\)' failed")
  expect_error(automlr(theTask, searchspace = nofailSearchSpace, backend = backendToTest, savefile = "/tmp/", save.interval = -2), "Assertion on 'save.interval' failed")
  expect_error(automlr(amobject, savefile = "/tmp/", save.interval = -2), "Assertion on 'save.interval' failed")
  expect_error(automlr(amfile, save.interval = -2), "Assertion on 'save.interval' failed")
  try(file.remove(fileboundObject$savefile), silent = TRUE)
})
