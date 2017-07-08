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

  # the numericslearner requires removal of factors and missings; parameters correctly determine which conversions are necessary.
  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = TRUE, automlr.convert.before.impute = FALSE,
    automlr.wimputing.factors = "factimputer1",
    automlr.wimputing.numerics = "numimputer1",
    reference.cat = FALSE, fimp.const = "NAx", NumericsLearner.int1 = 8)
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
    pvs, "NumericsLearner", list(int1 = 8), list(),
    factimputer1 = list(fimp.const = "NAx"),
    numimputer1 = list(),
    fnconv1 = list(reference.cat = FALSE))

  pvx = namedList(getParamIds(getParamSet(l)), NA)
  isFeasible(getParamSet(l), insert(pvx, pvs))


  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = TRUE, automlr.convert.before.impute = TRUE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    reference.cat = FALSE, NumericsLearner.int1 = 9)
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
   pvs, "NumericsLearner", list(int1 = 9), list(),
   fnconv1 = list(reference.cat = FALSE),
   numimputer2 = list(multiplier = 1))

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = FALSE,
    automlr.convert.before.impute = TRUE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    reference.cat = FALSE, NumericsLearner.int1 = 9)
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    fnconv1 = list(reference.cat = FALSE),
    numimputer2 = list(multiplier = 1))

  # The learner that can deal with missings and factors adds parameter automlr.impute. The wimputing / wconverting parameter is only present if the corresponding conversion happens.
  pvs = list(selected.learner = "MissingsFactorsNumericsLearner",
    automlr.convert.factors = TRUE, automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.numerics = FALSE,
    automlr.missing.indicators = TRUE, automlr.convert.before.impute = FALSE,
    automlr.wimputing.factors = "factimputer1",
    automlr.wimputing.numerics = "numimputer1",
    reference.cat = FALSE, fimp.const = "NAx")
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
    pvs, "MissingsFactorsNumericsLearner", list(), list(),
    factimputer1= list(fimp.const = "NAx"),
    numimputer1 = list(),
    fnconv1 = list(reference.cat = FALSE))

  pvs = list(selected.learner = "MissingsFactorsNumericsLearner",
    automlr.convert.factors = TRUE, automlr.impute = FALSE, automlr.convert = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.missing.indicators = TRUE,
    automlr.wconverting.numerics.to.factors = "nfconv2",
    reference.cat = FALSE, conv2.numsplits = 2,
    MissingsFactorsNumericsLearner.int1 = 9)
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
    pvs, "MissingsFactorsNumericsLearner", list(int1 = 9, real1 = 10), list(),
    fnconv1 = list(reference.cat = FALSE),
    nfconv2 = list(conv2.numsplits = 2))

  pvs = list(selected.learner = "MissingsFactorsNumericsLearner",
    automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.factors = FALSE,
    automlr.missing.indicators = TRUE,
    automlr.wconverting.numerics.to.factors = "nfconv2",
    conv2.numsplits = 2,
    automlr.wimputing.factors = "factimputer1", fimp.const = "NAx",
    MissingsFactorsNumericsLearner.bool1 = TRUE)
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
    pvs, "MissingsFactorsNumericsLearner", list(bool1 = TRUE, real1 = 10), list(),
    nfconv2 = list(conv2.numsplits = 2),
    factimputer1 = list(fimp.const = "NAx"))

  # AllLearner behaves essentially the same as MissingsFactorsNumericsLearner for the given task.

  pvs = list(selected.learner = "AllLearner",
    automlr.convert.factors = TRUE, automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.factors.to = "numerics",
    automlr.convert.numerics = FALSE,
    automlr.missing.indicators = TRUE, automlr.convert.before.impute = FALSE,
    automlr.wimputing.factors = "factimputer1",
    automlr.wimputing.numerics = "numimputer1",
    reference.cat = FALSE, fimp.const = "NAx",
    AllLearner.int1 = 9, AllLearner.real1 = 8)

  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
    pvs, "AllLearner", list(bool1 = TRUE, int1 = 9, real1 = 8), list(),
    factimputer1= list(fimp.const = "NAx"),
    numimputer1 = list(),
    fnconv1 = list(reference.cat = FALSE))

  pvs = list(selected.learner = "AllLearner",
    automlr.convert.factors = TRUE, automlr.impute = FALSE, automlr.convert = TRUE,
    automlr.convert.factors.to = "numerics",
    automlr.convert.numerics = TRUE,
    automlr.convert.numerics.to = "factors",
    automlr.missing.indicators = TRUE,
    automlr.wconverting.numerics.to.factors = "nfconv2",
    reference.cat = FALSE, conv2.numsplits = 2,
    AllLearner.real1 = 10)
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
    pvs, "AllLearner", list(int1 = 2, real1 = 10), list(),
    fnconv1 = list(reference.cat = FALSE),
    nfconv2 = list(conv2.numsplits = 2))

  pvs = list(selected.learner = "AllLearner",
    automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.numerics.to = "factors",
    automlr.convert.factors = FALSE,
    automlr.missing.indicators = TRUE,
    automlr.wconverting.numerics.to.factors = "nfconv2",
    conv2.numsplits = 2,
    automlr.wimputing.factors = "factimputer1", fimp.const = "NAx",
    AllLearner.real1 = 10, AllLearner.int1 = 6)
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
    pvs, "AllLearner", list(int1 = 6, real1 = 10), list(),
    nfconv2 = list(conv2.numsplits = 2),
    factimputer1 = list(fimp.const = "NAx"))

  pvs = list(selected.learner = "AllLearner",
    automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.numerics.to = "ordered",
    automlr.convert.factors = TRUE,
    automlr.convert.factors.to = "ordered",
    automlr.missing.indicators = TRUE,
    automlr.wconverting.numerics.to.ordered = "noconv2",
    automlr.wconverting.factors.to.ordered = "foconv1",
    automlr.wimputing.ordered = "ordimputer1", oimp.const = "NAx",
    AllLearner.real1 = 10, AllLearner.int1 = 6)
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
    pvs, "AllLearner", list(int1 = 6, real1 = 10), list(),
    foconv1 = list(), noconv2 = list(),
    ordimputer1 = list(oimp.const = "NAx"))

  pvs = list(selected.learner = "AllLearner",
    automlr.impute = FALSE, automlr.convert = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.numerics.to = "ordered",
    automlr.convert.factors = TRUE,
    automlr.convert.factors.to = "ordered",
    automlr.missing.indicators = TRUE,
    automlr.wconverting.numerics.to.ordered = "noconv2",
    automlr.wconverting.factors.to.ordered = "foconv1",
    AllLearner.real1 = 10, AllLearner.int1.AMLRFIX2 = 12)
  checkLearnerBehaviour(l, MissingsNumericsFactorsTask,
    pvs, "AllLearner", list(int1 = 12, real1 = 10), list(),
    foconv1 = list(), noconv2 = list())


})



# automlr.impute and automlr.wimputing are present in the right cases, and cause the right behaviour.
test_that("factors are present whenever missing indicators are introduced", {


  # all learners and most removers
  expect_warning(l <- buildLearners(list(
      NumericsLearner, FactorsLearner, OrderedsLearner,
      MissingsNumericsLearner, MissingsFactorsLearner, MissingsFactorsNumericsLearner,
      FactorsNumericsLearner, AllLearner,
      pWW(nimp1), pWW(nimp2), pWW(fimp1), pWW(fimp2), pWW(oimp1), pWW(oimp2),
      pWW(fnconv1), pWW(onconv1), pWW(onconv2),
      pWW(nfconv1), pWW(nfconv2), pWW(ofconv1), pWW(ofconv2),
      pWW(noconv1), pWW(noconv2), pWW(foconv1), pWW(foconv2)), MissingsNumericsTask, verbosity = 6),
      "different \\(but feasible\\) type 'cat' listed|has parameters .* not mentioned in search space", all = TRUE)


  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = TRUE, automlr.convert.before.impute = TRUE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    reference.cat = FALSE, NumericsLearner.int1 = 9)
  checkLearnerBehaviour(l, MissingsNumericsTask,
   pvs, "NumericsLearner", list(int1 = 9), list(),
   fnconv1 = list(reference.cat = FALSE),
   numimputer2 = list(multiplier = 1))

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    reference.cat = FALSE, NumericsLearner.int1 = 9)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    fnconv1 = list(reference.cat = FALSE),
    numimputer2 = list(multiplier = 1))

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = FALSE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    NumericsLearner.int1 = 9)
  checkLearnerBehaviour(l, MissingsNumericsTask,
   pvs, "NumericsLearner", list(int1 = 9), list(),
   numimputer2 = list(multiplier = 1))

  # The learner that can deal with missings and factors adds parameter automlr.impute. The wimputing / wconverting parameter is only present if the corresponding conversion happens.
  pvs = list(selected.learner = "MissingsFactorsNumericsLearner",
    automlr.convert.factors = TRUE, automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.numerics = FALSE,
    automlr.missing.indicators = TRUE, automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer1",
    reference.cat = FALSE)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "MissingsFactorsNumericsLearner", list(), list(),
    numimputer1 = list(),
    fnconv1 = list(reference.cat = FALSE))

  pvs = list(selected.learner = "MissingsFactorsNumericsLearner",
    automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.numerics = FALSE,
    automlr.missing.indicators = FALSE,
    automlr.wimputing.numerics = "numimputer1")
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "MissingsFactorsNumericsLearner", list(), list(),
    numimputer1 = list())


  pvs = list(selected.learner = "MissingsFactorsNumericsLearner",
    automlr.convert.factors = TRUE, automlr.impute = FALSE, automlr.convert = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.missing.indicators = TRUE,
    automlr.wconverting.numerics.to.factors = "nfconv2",
    reference.cat = FALSE, conv2.numsplits = 2,
    MissingsFactorsNumericsLearner.int1 = 9)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "MissingsFactorsNumericsLearner", list(int1 = 9, real1 = 10), list(),
    fnconv1 = list(reference.cat = FALSE),
    nfconv2 = list(conv2.numsplits = 2))

  pvs = list(selected.learner = "MissingsFactorsNumericsLearner",
    automlr.impute = FALSE, automlr.convert = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.missing.indicators = FALSE,
    automlr.wconverting.numerics.to.factors = "nfconv2",
    conv2.numsplits = 2,
    MissingsFactorsNumericsLearner.int1 = 9)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "MissingsFactorsNumericsLearner", list(int1 = 9, real1 = 10), list(),
    nfconv2 = list(conv2.numsplits = 2))


  pvs = list(selected.learner = "MissingsFactorsNumericsLearner",
    automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.factors = FALSE,
    automlr.missing.indicators = TRUE,
    automlr.wconverting.numerics.to.factors = "nfconv2",
    conv2.numsplits = 2,
    automlr.wimputing.factors = "factimputer1", fimp.const = "NAx",
    MissingsFactorsNumericsLearner.bool1 = TRUE)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "MissingsFactorsNumericsLearner", list(bool1 = TRUE, real1 = 10), list(),
    nfconv2 = list(conv2.numsplits = 2),
    factimputer1 = list(fimp.const = "NAx"))

  pvs = list(selected.learner = "MissingsFactorsNumericsLearner",
    automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.missing.indicators = FALSE,
    automlr.wconverting.numerics.to.factors = "nfconv2",
    conv2.numsplits = 2,
    automlr.wimputing.factors = "factimputer1", fimp.const = "NAx",
    MissingsFactorsNumericsLearner.bool1 = TRUE)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "MissingsFactorsNumericsLearner", list(bool1 = TRUE, real1 = 10), list(),
    nfconv2 = list(conv2.numsplits = 2),
    factimputer1 = list(fimp.const = "NAx"))

  # AllLearner behaves essentially the same as MissingsFactorsNumericsLearner for the given task.

  pvs = list(selected.learner = "AllLearner",
    automlr.convert.factors = TRUE, automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.factors.to = "numerics",
    automlr.convert.numerics = FALSE,
    automlr.missing.indicators = TRUE, automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer1",
    reference.cat = FALSE,
    AllLearner.int1 = 9, AllLearner.real1 = 8)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "AllLearner", list(bool1 = TRUE, int1 = 9, real1 = 8), list(),
    numimputer1 = list(),
    fnconv1 = list(reference.cat = FALSE))

  pvs = list(selected.learner = "AllLearner",
    automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.numerics = FALSE,
    automlr.missing.indicators = FALSE,
    automlr.wimputing.numerics = "numimputer1",
    AllLearner.int1 = 9, AllLearner.real1 = 8)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "AllLearner", list(bool1 = TRUE, int1 = 9, real1 = 8), list(),
    numimputer1 = list())

  pvs = list(selected.learner = "AllLearner",
    automlr.convert.factors = TRUE, automlr.impute = FALSE, automlr.convert = TRUE,
    automlr.convert.factors.to = "numerics",
    automlr.convert.numerics = TRUE,
    automlr.convert.numerics.to = "factors",
    automlr.missing.indicators = TRUE,
    automlr.wconverting.numerics.to.factors = "nfconv2",
    reference.cat = FALSE, conv2.numsplits = 2,
    AllLearner.real1 = 10)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "AllLearner", list(int1 = 2, real1 = 10), list(),
    fnconv1 = list(reference.cat = FALSE),
    nfconv2 = list(conv2.numsplits = 2))
pvs = list(selected.learner = "AllLearner",
    automlr.impute = FALSE, automlr.convert = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.numerics.to = "factors",
    automlr.missing.indicators = FALSE,
    automlr.wconverting.numerics.to.factors = "nfconv2",
    conv2.numsplits = 2,
    AllLearner.real1 = 10)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "AllLearner", list(int1 = 2, real1 = 10), list(),
    nfconv2 = list(conv2.numsplits = 2))

  pvs = list(selected.learner = "AllLearner",
    automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.numerics.to = "factors",
    automlr.convert.factors = FALSE,
    automlr.missing.indicators = TRUE,
    automlr.wconverting.numerics.to.factors = "nfconv2",
    conv2.numsplits = 2,
    automlr.wimputing.factors = "factimputer1", fimp.const = "NAx",
    AllLearner.real1 = 10, AllLearner.int1 = 6)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "AllLearner", list(int1 = 6, real1 = 10), list(),
    nfconv2 = list(conv2.numsplits = 2),
    factimputer1 = list(fimp.const = "NAx"))

pvs = list(selected.learner = "AllLearner",
    automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.numerics.to = "factors",
    automlr.missing.indicators = FALSE,
    automlr.wconverting.numerics.to.factors = "nfconv2",
    conv2.numsplits = 2,
    automlr.wimputing.factors = "factimputer1", fimp.const = "NAx",
    AllLearner.real1 = 10, AllLearner.int1 = 6)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "AllLearner", list(int1 = 6, real1 = 10), list(),
    nfconv2 = list(conv2.numsplits = 2),
    factimputer1 = list(fimp.const = "NAx"))


  pvs = list(selected.learner = "AllLearner",
    automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.numerics.to = "ordered",
    automlr.convert.factors = TRUE,
    automlr.convert.factors.to = "ordered",
    automlr.missing.indicators = TRUE,
    automlr.wconverting.numerics.to.ordered = "noconv2",
    automlr.wconverting.factors.to.ordered = "foconv1",
    automlr.wimputing.ordered = "ordimputer1", oimp.const = "NAx",
    AllLearner.real1 = 10, AllLearner.int1 = 6)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "AllLearner", list(int1 = 6, real1 = 10), list(),
    foconv1 = list(), noconv2 = list(),
    ordimputer1 = list(oimp.const = "NAx"))

pvs = list(selected.learner = "AllLearner",
    automlr.impute = TRUE, automlr.convert = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.numerics.to = "ordered",
    automlr.missing.indicators = FALSE,
    automlr.wconverting.numerics.to.ordered = "noconv2",
    automlr.wimputing.ordered = "ordimputer1", oimp.const = "NAx",
    AllLearner.real1 = 10, AllLearner.int1 = 6)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "AllLearner", list(int1 = 6, real1 = 10), list(),
    noconv2 = list(),
    ordimputer1 = list(oimp.const = "NAx"))


  pvs = list(selected.learner = "AllLearner",
    automlr.impute = FALSE, automlr.convert = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.numerics.to = "ordered",
    automlr.convert.factors = TRUE,
    automlr.convert.factors.to = "ordered",
    automlr.missing.indicators = TRUE,
    automlr.wconverting.numerics.to.ordered = "noconv2",
    automlr.wconverting.factors.to.ordered = "foconv1",
    AllLearner.real1 = 10, AllLearner.int1.AMLRFIX2 = 12)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "AllLearner", list(int1 = 12, real1 = 10), list(),
    foconv1 = list(), noconv2 = list())

pvs = list(selected.learner = "AllLearner",
    automlr.impute = FALSE, automlr.convert = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.numerics.to = "ordered",
    automlr.missing.indicators = FALSE,
    automlr.wconverting.numerics.to.ordered = "noconv2",
    AllLearner.real1 = 10, AllLearner.int1.AMLRFIX2 = 12)
  checkLearnerBehaviour(l, MissingsNumericsTask,
    pvs, "AllLearner", list(int1 = 12, real1 = 10), list(),
    noconv2 = list())


})
