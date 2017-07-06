# Test that feature types influence the presence and absence of learners, wrapper parameters and pseudoparameters
# in the right way.
context("coltypes")

# the subset of learners that can use the given task is chosen
test_that("the correct learner is automatically chosen", {

  nlr = NumericsLearner
  nlr$searchspace[[1]] = sp("int1", "int", c(0, 10), req = quote(2 %in% c(1, 2, 3)))

  # test that '2 %in% c(1, 2, 3)' is removed since it is always true.
  l = blt(list(nlr), NumericsTask)
  expect_null(getpars(l)$NumericsLearner.int1$requires)

  # only numericslearner remains for numericstask
  expect_message(l <- blt(list(NumericsLearner, FactorsLearner, OrderedsLearner), NumericsTask),
    "Learner can not handle feature types", all = TRUE)
  checkLearnerBehaviour(l, NumericsTask, list(NumericsLearner.int1 = 1), "NumericsLearner", list(int1 = 1), list())

  # both numericslearner and missingsnumericslearner work on numericstask
  l = blt(list(NumericsLearner, MissingsNumericsLearner), NumericsTask)
  checkLearnerBehaviour(l, NumericsTask, list(selected.learner = "MissingsNumericsLearner"),
                        "MissingsNumericsLearner", list(), list())

  # only missingsnumericslearner works on missingsnumericstask, int1 must be set since missings are present
  expect_message(l <- blt(list(NumericsLearner, MissingsNumericsLearner), MissingsNumericsTask),
    "Learner can not handle the task", all = TRUE)

  checkLearnerBehaviour(l, MissingsNumericsTask, list(MissingsNumericsLearner.int1 = 2),
                        "MissingsNumericsLearner", list(int1 = 2), list())
})

# test that automlr.has.missings and automlr.remove.missings work as they should, are present only when needed, and respect wrapper order.
test_that("parameters are fed to the correct learner / wrapper", {
  l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, XRemover, NARemover, FactorRemover), NumericsTask)
  # numericslearner for numericstask: wrappers don't do anything.
  checkLearnerBehaviour(l, NumericsTask,
                        list(selected.learner = "NumericsLearner", NumericsLearner.int1 = 3, automlr.wrappersetup = "FactorRemover$XRemover$NARemover",
                             XRemover.spare1 = 3),
                        "NumericsLearner", list(int1 = 3), list(),
                        FactorRemover = list(FactorRemover.spare1 = 0, FactorRemover.spare2 = 0),
                        XRemover = list(XRemover.spare1 = 3, XRemover.spare2 = 0),
                        NARemover = list(NARemover.spare1 = 0, NARemover.spare2 = 0))

  # numericslearner for missingsnumericstask: NARemover automatically removes NAs
  # also watch changed order of wrappers
  l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, XRemover, NARemover, FactorRemover), MissingsNumericsTask)
  checkLearnerBehaviour(l, MissingsNumericsTask,
                        list(selected.learner = "NumericsLearner", NumericsLearner.int1 = 3, automlr.wrappersetup = "XRemover$FactorRemover$NARemover",
                             XRemover.spare1 = 3),
                        "NumericsLearner", list(int1 = 3), list(),
                        XRemover = list(XRemover.spare1 = 3, XRemover.spare2 = 9),
                        FactorRemover = list(FactorRemover.spare1 = 0, FactorRemover.spare2 = 0),
                        NARemover = list(NARemover.spare1 = 0, NARemover.spare2 = 0, NARemover.remove.NA = TRUE))

  # putting NARemover before XRemover causes XRemover not seeing missings any more, therefore changing XRemover.spare2 from 9 to 0
  l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, XRemover, NARemover, FactorRemover), MissingsNumericsTask)
  checkLearnerBehaviour(l, NumericsTask,
                        list(selected.learner = "NumericsLearner", NumericsLearner.int1 = 3, automlr.wrappersetup = "NARemover$XRemover$FactorRemover",
                             XRemover.spare1 = 3),
                        "NumericsLearner", list(int1 = 3), list(),
                        NARemover = list(NARemover.spare1 = 0, NARemover.spare2 = 0, NARemover.remove.NA = TRUE),
                        XRemover = list(XRemover.spare1 = 3, XRemover.spare2 = 0),
                        FactorRemover = list(FactorRemover.spare1 = 0, FactorRemover.spare2 = 0))

  # using MissingsNumericsLearner has the same effect if automlr.remove.missings == TRUE
  l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, XRemover, NARemover, FactorRemover), MissingsNumericsTask)
  checkLearnerBehaviour(l, NumericsTask,
                        list(selected.learner = "MissingsNumericsLearner", automlr.wrappersetup = "NARemover$XRemover$FactorRemover",
                             XRemover.spare1 = 3, automlr.remove.missings = TRUE),
                        "MissingsNumericsLearner", list(), list(),
                        NARemover = list(NARemover.spare1 = 0, NARemover.spare2 = 0, NARemover.remove.NA = TRUE),
                        XRemover = list(XRemover.spare1 = 3, XRemover.spare2 = 0),
                        FactorRemover = list(FactorRemover.spare1 = 0, FactorRemover.spare2 = 0))

  # with automlr.remove.missings == FALSE, XRemover sees the missings again and XRemover.spare2 is 9; also MissingsNumericsLearner.int1 has true requirement.
  l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, XRemover, NARemover, FactorRemover), MissingsNumericsTask)
  checkLearnerBehaviour(l, NumericsTask,
                        list(selected.learner = "MissingsNumericsLearner", automlr.wrappersetup = "NARemover$XRemover$FactorRemover",
                             XRemover.spare1 = 3, automlr.remove.missings = FALSE, MissingsNumericsLearner.int1 = 9),
                        "MissingsNumericsLearner", list(int1 = 9), list(),
                        NARemover = list(NARemover.spare1 = 0, NARemover.spare2 = 0),
                        XRemover = list(XRemover.spare1 = 3, XRemover.spare2 = 9),
                        FactorRemover = list(FactorRemover.spare1 = 0, FactorRemover.spare2 = 0))
})

# automlr.remove.x and automlr.wremoving.x are present in the right cases, and cause the right behaviour.
test_that("requirements using pseudoparameters behave as expected", {
  # all learners and removers
  expect_warning(l <- buildLearners(list(
      NumericsLearner, FactorsLearner, OrderedsLearner,
      MissingsNumericsLearner, MissingsFactorsLearner, MissingsFactorsNumericsLearner,
      FactorsNumericsLearner, AllLearner,
      XRemover, NARemover, FactorRemover, NAFactorRemover), MissingsNumericsFactorsTask, verbose = TRUE),
      "different \\(but feasible\\) type 'cat' listed", all = TRUE)

  # the wrappers that can remove factors, or missings, are the expected ones
  expect_set_equal(unlist(getpars(l)$automlr.wremoving.factors$values), c("FactorRemover", "NAFactorRemover"))
  expect_set_equal(unlist(getpars(l)$automlr.wremoving.missings$values), c("NARemover", "NAFactorRemover"))

  # the numericslearner requires removal of factors and missings; the wremoving parameters correctly determine which wrapper removes them.
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
                        list(selected.learner = "NumericsLearner", automlr.wrappersetup = "NARemover$XRemover$FactorRemover$NAFactorRemover",
                             automlr.wremoving.factors = "FactorRemover", automlr.wremoving.missings = "NARemover",
                             XRemover.spare1 = 3, FactorRemover.convertFactors = FALSE,
                             NumericsLearner.int1 = 8),
                        "NumericsLearner", list(int1 = 8), list(),
                        NARemover = list(NARemover.spare1 = 0, NARemover.spare2 = 0, NARemover.remove.NA = TRUE),
                        XRemover = list(XRemover.spare1 = 3, XRemover.spare2 = 0),
                        FactorRemover = list(FactorRemover.spare1 = 0, FactorRemover.spare2 = 0, FactorRemover.remove.factors = TRUE),
                        NAFactorRemover = list(NAFactorRemover.spare1 = 0, NAFactorRemover.spare2 = 0))

  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
                        list(selected.learner = "NumericsLearner", automlr.wrappersetup = "NARemover$XRemover$FactorRemover$NAFactorRemover",
                             automlr.wremoving.factors = "FactorRemover", automlr.wremoving.missings = "NAFactorRemover",
                             XRemover.spare1 = 3, FactorRemover.convertFactors = TRUE,
                             NumericsLearner.int1 = 7),
                        "NumericsLearner", list(int1 = 7), list(),
                        NARemover = list(NARemover.spare1 = 0, NARemover.spare2 = 0),
                        XRemover = list(XRemover.spare1 = 3, XRemover.spare2 = 9),
                        FactorRemover = list(FactorRemover.spare1 = 0, FactorRemover.spare2 = 0, FactorRemover.convert.fac2num = TRUE),
                        NAFactorRemover = list(NAFactorRemover.spare1 = 0, NAFactorRemover.spare2 = 0, NAFactorRemover.remove.NA = TRUE))

  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
                        list(selected.learner = "NumericsLearner", automlr.wrappersetup = "NARemover$XRemover$FactorRemover$NAFactorRemover",
                             automlr.wremoving.factors = "NAFactorRemover", automlr.wremoving.missings = "NAFactorRemover",
                             XRemover.spare1 = 3,
                             NumericsLearner.int1 = 6),
                        "NumericsLearner", list(int1 = 6), list(),
                        NARemover = list(NARemover.spare1 = 0, NARemover.spare2 = 0),
                        XRemover = list(XRemover.spare1 = 3, XRemover.spare2 = 9),
                        FactorRemover = list(FactorRemover.spare1 = 0, FactorRemover.spare2 = 0),
                        NAFactorRemover = list(NAFactorRemover.spare1 = 0, NAFactorRemover.spare2 = 0, NAFactorRemover.remove.NA = TRUE, NAFactorRemover.remove.factors = TRUE))

  # The learner that can deal with missings and factors adds parameter automlr.remove.x. The wremoving parameter is only present if the corresponding remove.x is TRUE.
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
                        list(selected.learner = "MissingsFactorsNumericsLearner", automlr.wrappersetup = "NARemover$XRemover$FactorRemover$NAFactorRemover",
                             automlr.remove.factors = TRUE, automlr.remove.missings = TRUE,
                             automlr.wremoving.factors = "NAFactorRemover", automlr.wremoving.missings = "NAFactorRemover",
                             XRemover.spare1 = 3),
                        "MissingsFactorsNumericsLearner", list(), list(),
                        NARemover = list(NARemover.spare1 = 0, NARemover.spare2 = 0),
                        XRemover = list(XRemover.spare1 = 3, XRemover.spare2 = 9),
                        FactorRemover = list(FactorRemover.spare1 = 0, FactorRemover.spare2 = 0),
                        NAFactorRemover = list(NAFactorRemover.spare1 = 0, NAFactorRemover.spare2 = 0, NAFactorRemover.remove.NA = TRUE, NAFactorRemover.remove.factors = TRUE))

  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
                        list(selected.learner = "MissingsFactorsNumericsLearner", automlr.wrappersetup = "NARemover$XRemover$FactorRemover$NAFactorRemover",
                             automlr.remove.factors = FALSE, automlr.remove.missings = TRUE,
                             automlr.wremoving.missings = "NARemover",
                             XRemover.spare1 = 3,
                             MissingsFactorsNumericsLearner.bool1 = TRUE),
                        "MissingsFactorsNumericsLearner", list(bool1 = TRUE, real1 = 10), list(),
                        NARemover = list(NARemover.spare1 = 0, NARemover.spare2 = 0, NARemover.remove.NA = TRUE),
                        XRemover = list(XRemover.spare1 = 3, XRemover.spare2 = 0),
                        FactorRemover = list(FactorRemover.spare1 = 0, FactorRemover.spare2 = 0),
                        NAFactorRemover = list(NAFactorRemover.spare1 = 0, NAFactorRemover.spare2 = 0))

  # AllLearner behaves essentially the same as MissingsFactorsNumericsLearner for the given task.
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
                        list(selected.learner = "AllLearner", automlr.wrappersetup = "NARemover$XRemover$FactorRemover$NAFactorRemover",
                             automlr.remove.factors = FALSE, automlr.remove.missings = TRUE,
                             automlr.wremoving.missings = "NAFactorRemover",
                             XRemover.spare1 = 3,
                             AllLearner.int1 = 3, AllLearner.real1 = 0.5),
                        "AllLearner", list(bool1 = TRUE, real1 = 0.5, int1 = 3), list(),
                        NARemover = list(NARemover.spare1 = 0, NARemover.spare2 = 0),
                        XRemover = list(XRemover.spare1 = 3, XRemover.spare2 = 9),
                        FactorRemover = list(FactorRemover.spare1 = 0, FactorRemover.spare2 = 0),
                        NAFactorRemover = list(NAFactorRemover.spare1 = 0, NAFactorRemover.spare2 = 0, NAFactorRemover.remove.NA = TRUE))

  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
                        list(selected.learner = "AllLearner", automlr.wrappersetup = "NARemover$XRemover$FactorRemover$NAFactorRemover",
                             automlr.remove.factors = FALSE, automlr.remove.missings = FALSE,
                             XRemover.spare1 = 3,
                             AllLearner.real1 = 0.5),
                        "AllLearner", list(bool1 = TRUE, real1 = 0.5, int1 = 2), list(),
                        NARemover = list(NARemover.spare1 = 0, NARemover.spare2 = 0),
                        XRemover = list(XRemover.spare1 = 3, XRemover.spare2 = 9),
                        FactorRemover = list(FactorRemover.spare1 = 0, FactorRemover.spare2 = 0),
                        NAFactorRemover = list(NAFactorRemover.spare1 = 0, NAFactorRemover.spare2 = 0))

  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
                        list(selected.learner = "AllLearner", automlr.wrappersetup = "NARemover$XRemover$FactorRemover$NAFactorRemover",
                             automlr.remove.factors = TRUE, automlr.remove.missings = FALSE,
                             automlr.wremoving.factors = "FactorRemover",
                             FactorRemover.convertFactors = FALSE,
                             XRemover.spare1 = 3,
                             AllLearner.int1.AMLRFIX2 = 19, AllLearner.real1 = 0.5),
                        "AllLearner", list(bool1 = TRUE, real1 = 0.5, int1 = 19), list(),
                        NARemover = list(NARemover.spare1 = 0, NARemover.spare2 = 0),
                        XRemover = list(XRemover.spare1 = 3, XRemover.spare2 = 9),
                        FactorRemover = list(FactorRemover.spare1 = 0, FactorRemover.spare2 = 0, FactorRemover.remove.factors = TRUE),
                        NAFactorRemover = list(NAFactorRemover.spare1 = 0, NAFactorRemover.spare2 = 0))
})



