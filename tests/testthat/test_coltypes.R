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

# test that automlr.has.missings works as it should, is present only when needed
test_that("parameters are fed to the correct learner / wrapper", {
  l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, pWW(nimp1), pWW(nimp2)), NumericsTask, verbosity = 5)

  # numericslearner for numericstask: wrappers don't do anything.
  checkLearnerBehaviour(l, NumericsTask,
                        list(selected.learner = "NumericsLearner", NumericsLearner.int1 = 3),
                        "NumericsLearner", list(int1 = 3), list())

  # numericslearner for missingsnumericstask: NARemover automatically removes NAs
  # also watch changed order of wrappers
  l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, pWW(nimp1), pWW(nimp2)), MissingsNumericsTask, verbosity = 6)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    list(selected.learner = "NumericsLearner", NumericsLearner.int1 = 3, automlr.wimputing.numerics = "numimputer1"),
    "NumericsLearner", list(int1 = 3),
    numimputer1 = list())

  checkLearnerBehaviour(l, MissingsNumericsTask,
                        list(selected.learner = "NumericsLearner", NumericsLearner.int1 = 3, automlr.wimputing.numerics = "numimputer2", multiplier = 2),
                        "NumericsLearner", list(int1 = 3), numimputer2 = list(multiplier = 2))


    # using MissingsNumericsLearner has the same effect if automlr.remove.missings == TRUE
  l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, pWW(nimp1), pWW(nimp2), pWW(fimp1), pWW(fimp2), pWW(oimp1), pWW(oimp2)),
    MissingsNumericsTask, verbosity = 6)
  checkLearnerBehaviour(l, NumericsTask,
                        list(selected.learner = "MissingsNumericsLearner", automlr.impute = TRUE, automlr.wimputing.numerics = "numimputer2", multiplier = 2),
                        "MissingsNumericsLearner", list(), list(),
                        numimputer2 = list(multiplier = 2))

  # with automlr.remove.missings == FALSE, MissingsNumericsLearner.int1 has true requirement.
  l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, pWW(nimp1), pWW(nimp2), pWW(fimp1), pWW(fimp2), pWW(oimp1), pWW(oimp2)),
    MissingsNumericsTask, verbosity = 6)
  checkLearnerBehaviour(l, NumericsTask,
                        list(selected.learner = "MissingsNumericsLearner", automlr.impute = FALSE, MissingsNumericsLearner.int1 = 9),
                        "MissingsNumericsLearner", list(int1 = 9), list())
})

# automlr.impute and automlr.wimputing are present in the right cases, and cause the right behaviour.
test_that("requirements using pseudoparameters behave as expected", {

  # all learners and most removers
  expect_warning(l <- buildLearners(list(
      NumericsLearner, FactorsLearner, OrderedsLearner,
      MissingsNumericsLearner, MissingsFactorsLearner, MissingsFactorsNumericsLearner,
      FactorsNumericsLearner, AllLearner,
      pWW(nimp1), pWW(nimp2), pWW(fimp1), pWW(fimp2), pWW(oimp1), pWW(oimp2),
      pWW(fnconv1), pWW(onconv1), pWW(onconv2),
      pWW(nfconv1), pWW(nfconv2), pWW(ofconv1), pWW(ofconv2),
      pWW(noconv1), pWW(noconv2), pWW(foconv1), pWW(foconv2)), MissingsNumericsFactorsTask, verbosity = 6),
      "different \\(but feasible\\) type 'cat' listed|has parameters .* not mentioned in search space", all = TRUE)

  # the wrappers that can remove factors, or missings, are the expected ones
  expect_set_equal(unlist(getpars(l)$automlr.wconverting.factors.to.ordered$values), c("foconv1", "foconv2"))
  expect_set_equal(unlist(getpars(l)$automlr.wimputing.ordered$values), c("ordimputer1", "ordimputer2"))

  # the numericslearner requires removal of factors and missings; most parameters correctly determine which conversions are necessary.
  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = TRUE, automlr.convert.before.impute = FALSE,
    automlr.wimputing.factors = "factimputer1",
    automlr.wimputing.numerics = "numimputer1",
    reference.cat = FALSE, fimp.const = "NAx", NumericsLearner.int1 = 8)
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
    pvs, "NumericsLearner", list(int1 = 8), list(),
    factimputer1 = list(fimp.const = "NAx"),
    numimputer1 = list(),
    fnconv1 = list(reference.cat = FALSE))

#  pvx = namedList(getParamIds(getParamSet(l)), NA)
#  isFeasible(getParamSet(l), insert(pvx, pvs))


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



