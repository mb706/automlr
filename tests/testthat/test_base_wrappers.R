# Testing that wrappers generate a correct search space, get executed in the right
# order, with the right parameters
context("wrappers")

test_that("wrappers are used in the expected way", {
  # define autolearners with a parameter
  test1 = autolearner(
      testLearner("test1", makeParamSet(predefParams$int1), c("numerics", "twoclass")),
      list(sp("int1", "int", c(0, 10))))
  test2 = autolearner(
      testLearner("test2", makeParamSet(predefParams$int1), c("numerics", "twoclass")),
      list(sp("int1", "int", c(0, 10))))

  # test the resulting parameter sets contain the expected parameters.
  # converters, imputers, wrappers that are not relevant are ignored.
  expect_set_equal(getParamIds(getParamSet(bl(test1, test2))), c("selected.learner", "test1.int1", "test2.int1"))
  expect_set_equal(getParamIds(getParamSet(bl(test1, test2, ofconv1, ofconv2, nimp1, nimp2))), c("selected.learner", "test1.int1", "test2.int1"))
  expect_set_equal(getParamIds(getParamSet(bl(np1, test1, test2))),
    c("selected.learner", "test1.int1", "test2.int1", "automlr.preproc.numerics", "pca.scale", "pca.center"))
  expect_set_equal(getParamIds(getParamSet(bl(np1, test1, test2, ofconv1, ofconv2, nimp1, nimp2))),
    c("selected.learner", "test1.int1", "test2.int1", "automlr.preproc.numerics", "pca.scale", "pca.center"))
  expect_set_equal(getParamIds(getParamSet(bl(np1, np2, test1, test2, ofconv1, ofconv2, nimp1, nimp2))),
    c("selected.learner", "test1.int1", "test2.int1", "automlr.preproc.numerics", "pca.scale", "pca.center", "scale.center", "scale.scale"))
  expect_set_equal(getParamIds(getParamSet(bl(np1, np2, op1, op2, test1, test2, ofconv1, ofconv2, nimp1, nimp2))),
    c("selected.learner", "test1.int1", "test2.int1", "automlr.preproc.numerics", "pca.scale", "pca.center", "scale.center", "scale.scale"))
  expect_warning(expect_set_equal(getParamIds(getParamSet(blt(list(np1, np2, op1, op2, fp1, fp2, test1, test2, nimp1, nimp2, AllLearner), NumericsFactorsTask))),
    c("automlr.preproc.numerics", "pca.scale", "pca.center", "scale.center", "scale.scale",
      "automlr.preproc.factors", "AllLearner.real1", "AllLearner.int1")), "'bool1' for learner 'AllLearner' is of type 'logical'", all = TRUE)


  # for a search space including a preproc wrapper, test that:
  # - the parameter space is the expected one, with the expected automlr.preproc*
  # - the dependency of parameters on the automlr.preproc.* is correct
  # - the expected wrappers show up on learner execution
  lw1w1r = bl(pWW(np1), test1, test2, ofconv1, ofconv2, nimp1, nimp2)
  lw1w1rPS = getParamSet(lw1w1r)
  expect_set_equal(getParamIds(lw1w1rPS),  c("selected.learner", "test1.int1", "test2.int1", "automlr.preproc.numerics", "pca.scale", "pca.center"))
  expect_set_equal(as.character(lw1w1rPS$pars$automlr.preproc.numerics$values), c("$", "np1"))
  expect_true(isFeasibleNoneMissing(lw1w1rPS, list(test1.int1 = 1, selected.learner = "test1", automlr.preproc.numerics = "$")))
  expect_true(isFeasibleNoneMissing(lw1w1rPS, list(test1.int1 = 1, selected.learner = "test1", automlr.preproc.numerics = "np1", pca.scale = TRUE, pca.center = FALSE)))
  lx = setHyperPars(lw1w1r, test1.int1 = 1, selected.learner = "test1", automlr.preproc.numerics = "$")
  expect_learner_output(lx, pid.task, "test1", list(int1 = 1), list())
  lx = setHyperPars(lw1w1r, test2.int1 = 2, selected.learner = "test2", automlr.preproc.numerics = "np1", pca.scale = TRUE, pca.center = FALSE)
  expect_learner_output(lx, pid.task, "test2", list(int1 = 2), list(), np1 = list(pca.scale = TRUE, pca.center = FALSE))


  # for a larger set of wrappers, check that:
  # - the resulting parameter set is correct
  # - the expected parameters are feasible
  # - the wrappers show up on learner execution in correct order and with correct parameters
  lw1 = bl(pWW(np1), pWW(np2), test1, test2)
  lw1PS = getParamSet(lw1)
  expect_set_equal(getParamIds(lw1PS),  c("selected.learner", "test1.int1", "test2.int1",
    "automlr.preproc.numerics", "pca.scale", "pca.center", "scale.scale", "scale.center"))
  expect_set_equal(as.character(lw1PS$pars$automlr.preproc.numerics$values), c("$", "np1", "np2", "np1$np2", "np2$np1"))
  expect_true(isFeasibleNoneMissing(lw1PS, list(test1.int1 = 1, selected.learner = "test1", automlr.preproc.numerics = "$")))
  expect_true(isFeasibleNoneMissing(lw1PS, list(test1.int1 = 1, selected.learner = "test1", automlr.preproc.numerics = "np1", pca.scale = TRUE, pca.center = FALSE)))
  lx = setHyperPars(lw1, test1.int1 = 1, selected.learner = "test1", automlr.preproc.numerics = "$")
  expect_learner_output(lx, pid.task, "test1", list(int1 = 1), list())
  lx = setHyperPars(lw1, test2.int1 = 2, selected.learner = "test2", automlr.preproc.numerics = "np1", pca.center = TRUE, pca.scale = TRUE)
  expect_learner_output(lx, pid.task, "test2", list(int1 = 2), list(), np1 = list(pca.center = TRUE, pca.scale = TRUE))
  lx = setHyperPars(lw1, test2.int1 = 2, selected.learner = "test2", automlr.preproc.numerics = "np2", scale.center = TRUE, scale.scale = TRUE)
  expect_learner_output(lx, pid.task, "test2", list(int1 = 2), list(), np2 = list(scale.center = TRUE, scale.scale = TRUE))
  lx = setHyperPars(lw1, test2.int1 = 2, selected.learner = "test2", automlr.preproc.numerics = "np1$np2",
    pca.center = TRUE, pca.scale = TRUE, scale.center = FALSE, scale.scale = FALSE)
  expect_learner_output(lx, pid.task, "test2", list(int1 = 2), list(),
    np1 = list(pca.center = TRUE, pca.scale = TRUE), np2 = list(scale.center = FALSE, scale.scale = FALSE))
  lx = setHyperPars(lw1, test2.int1 = 2, selected.learner = "test2", automlr.preproc.numerics = "np2$np1",
    pca.center = TRUE, pca.scale = TRUE, scale.center = FALSE, scale.scale = FALSE)
  expect_learner_output(lx, pid.task, "test2", list(int1 = 2), list(),
    np2 = list(scale.center = FALSE, scale.scale = FALSE), np1 = list(pca.center = TRUE, pca.scale = TRUE))
})

test_that("preproc wrappers interact nicely with conversion / imputation wrappers", {

  # all learners, many removers, all wrappers
  expect_warning(l <- buildLearners(list(
      NumericsLearner, FactorsLearner, OrderedsLearner,
      MissingsNumericsLearner, MissingsFactorsLearner, MissingsFactorsNumericsLearner,
      FactorsNumericsLearner, AllLearner,
      pWW(nimp1), pWW(nimp2), pWW(fimp1), pWW(fimp2), pWW(oimp1), pWW(oimp2),
      pWW(fnconv1), pWW(onconv1), pWW(onconv2),
      pWW(nfconv1), pWW(nfconv2), pWW(ofconv1), pWW(ofconv2),
      pWW(noconv1), pWW(noconv2), pWW(foconv1), pWW(foconv2),
      pWW(np1), pWW(np2), pWW(op1), pWW(op2), pWW(fp1), pWW(fp2)), MissingsNumericsOrderedTask, verbosity = 6),
      "different \\(but feasible\\) type 'cat' listed|has parameters .* not mentioned in search space", all = TRUE)


  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wconverting.ordered.to.numerics = "onconv2",
    reference.cat = FALSE, NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "$")
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    fnconv1 = list(reference.cat = FALSE),
    onconv2 = list(),
    numimputer2 = list(multiplier = 1))

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wconverting.ordered.to.numerics = "onconv2",
    reference.cat = FALSE, NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "np1",
    pca.scale = TRUE, pca.center = FALSE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    fnconv1 = list(reference.cat = FALSE),
    onconv2 = list(),
    numimputer2 = list(multiplier = 1),
    np1 = list(pca.scale = TRUE, pca.center = FALSE))

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wconverting.ordered.to.numerics = "onconv2",
    reference.cat = FALSE, NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "np2$np1",
    pca.scale = TRUE, pca.center = FALSE,
    scale.scale = FALSE, scale.center = TRUE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    fnconv1 = list(reference.cat = FALSE),
    onconv2 = list(),
    numimputer2 = list(multiplier = 1),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE))

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = FALSE,
    automlr.convert.before.impute = TRUE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wconverting.ordered.to.numerics = "onconv2",
    NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "$")
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    onconv2 = list(),
    numimputer2 = list(multiplier = 1))

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = FALSE,
    automlr.convert.before.impute = TRUE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wconverting.ordered.to.numerics = "onconv2",
    NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "np1",
    pca.scale = TRUE, pca.center = FALSE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    onconv2 = list(),
    numimputer2 = list(multiplier = 1),
    np1 = list(pca.scale = TRUE, pca.center = FALSE))

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = FALSE,
    automlr.convert.before.impute = TRUE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wconverting.ordered.to.numerics = "onconv2",
    NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "np2$np1",
    pca.scale = TRUE, pca.center = FALSE,
    scale.scale = FALSE, scale.center = TRUE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    onconv2 = list(),
    numimputer2 = list(multiplier = 1),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE))

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = FALSE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wimputing.ordered = "ordimputer2",
    automlr.wconverting.ordered.to.numerics = "onconv2",
    NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "$",
    automlr.preproc.ordered = "$",
    automlr.wrapafterconvert.ordered = FALSE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    ordimputer2 = list(),
    numimputer2 = list(multiplier = 1),
    onconv2 = list())

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = FALSE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wimputing.ordered = "ordimputer2",
    automlr.wconverting.ordered.to.numerics = "onconv2",
    NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "$",
    automlr.preproc.ordered = "$",
    automlr.wrapafterconvert.ordered = TRUE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    ordimputer2 = list(),
    numimputer2 = list(multiplier = 1),
    onconv2 = list())

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = FALSE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wimputing.ordered = "ordimputer2",
    automlr.wconverting.ordered.to.numerics = "onconv2",
    NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "np1",
    automlr.preproc.ordered = "op1",
    pca.scale = TRUE, pca.center = FALSE,
    automlr.wrapafterconvert.ordered = FALSE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    ordimputer2 = list(),
    numimputer2 = list(multiplier = 1),
    op1 = list(),
    np1 = list(pca.scale = TRUE, pca.center = FALSE),
    onconv2 = list())

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = FALSE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wimputing.ordered = "ordimputer2",
    automlr.wconverting.ordered.to.numerics = "onconv2",
    NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "np1",
    automlr.preproc.ordered = "op1",
    pca.scale = TRUE, pca.center = FALSE,
    automlr.wrapafterconvert.ordered = TRUE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    ordimputer2 = list(),
    numimputer2 = list(multiplier = 1),
    op1 = list(),
    np1 = list(pca.scale = TRUE, pca.center = FALSE),
    onconv2 = list(),
    np1 = list(pca.scale = TRUE, pca.center = FALSE))

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = FALSE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wimputing.ordered = "ordimputer2",
    automlr.wconverting.ordered.to.numerics = "onconv2",
    NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "np2$np1",
    automlr.preproc.ordered = "op2$op1",
    pca.scale = TRUE, pca.center = FALSE,
    scale.scale = FALSE, scale.center = TRUE,
    automlr.wrapafterconvert.ordered = FALSE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    ordimputer2 = list(),
    numimputer2 = list(multiplier = 1),
    op2 = list(),
    op1 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE),
    onconv2 = list())

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = FALSE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wimputing.ordered = "ordimputer2",
    automlr.wconverting.ordered.to.numerics = "onconv2",
    NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "np2$np1",
    automlr.preproc.ordered = "op2$op1",
    pca.scale = TRUE, pca.center = FALSE,
    scale.scale = FALSE, scale.center = TRUE,
    automlr.wrapafterconvert.ordered = TRUE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    ordimputer2 = list(),
    numimputer2 = list(multiplier = 1),
    op2 = list(),
    op1 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE),
    onconv2 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE))



  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = TRUE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wimputing.ordered = "ordimputer2",
    automlr.wconverting.ordered.to.numerics = "onconv2",
    NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "np2$np1",
    automlr.preproc.ordered = "op2$op1",
    automlr.preproc.factors = "$",
    reference.cat = FALSE,
    pca.scale = TRUE, pca.center = FALSE,
    scale.scale = FALSE, scale.center = TRUE,
    automlr.wrapafterconvert.ordered = TRUE,
    automlr.wrapafterconvert.factors = FALSE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    ordimputer2 = list(),
    numimputer2 = list(multiplier = 1),
    op2 = list(),
    op1 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE),
    fnconv1 = list(reference.cat = FALSE),
    onconv2 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE))

  pvs = list(selected.learner = "NumericsLearner", automlr.missing.indicators = TRUE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wimputing.ordered = "ordimputer2",
    automlr.wconverting.ordered.to.numerics = "onconv2",
    NumericsLearner.int1 = 9,
    automlr.preproc.numerics = "np2$np1",
    automlr.preproc.ordered = "op2$op1",
    automlr.preproc.factors = "fp1",
    reference.cat = FALSE,
    pca.scale = TRUE, pca.center = FALSE,
    scale.scale = FALSE, scale.center = TRUE,
    automlr.wrapafterconvert.ordered = TRUE,
    automlr.wrapafterconvert.factors = FALSE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "NumericsLearner", list(int1 = 9), list(),
    ordimputer2 = list(),
    numimputer2 = list(multiplier = 1),
    fp1 = list(),
    op2 = list(),
    op1 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE),
    fnconv1 = list(reference.cat = FALSE),
    onconv2 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE))


  pvs = list(selected.learner = "MissingsFactorsNumericsLearner", automlr.missing.indicators = TRUE,
    automlr.impute = TRUE,
    automlr.convert.factors = TRUE,
    automlr.convert.numerics = FALSE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wimputing.ordered = "ordimputer2",
    automlr.convert.ordered.to = "numerics",
    automlr.wconverting.ordered.to.numerics = "onconv2",
    automlr.preproc.numerics = "np2$np1",
    automlr.preproc.ordered = "op2$op1",
    automlr.preproc.factors = "fp1",
    reference.cat = FALSE,
    pca.scale = TRUE, pca.center = FALSE,
    scale.scale = FALSE, scale.center = TRUE,
    automlr.wrapafterconvert.ordered = TRUE,
    automlr.wrapafterconvert.factors = FALSE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "MissingsFactorsNumericsLearner", list(), list(),
    ordimputer2 = list(),
    numimputer2 = list(multiplier = 1),
    fp1 = list(),
    op2 = list(),
    op1 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE),
    fnconv1 = list(reference.cat = FALSE),
    onconv2 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE))

  setMlrOption("debug.seed", 123)

  pvs = list(selected.learner = "MissingsFactorsNumericsLearner", automlr.missing.indicators = TRUE,
    automlr.impute = TRUE,
    automlr.convert.factors = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wimputing.ordered = "ordimputer2",
    automlr.convert.ordered.to = "numerics",
    automlr.wconverting.ordered.to.numerics = "onconv2",
    automlr.wconverting.numerics.to.factors = "nfconv1",
    numsplits = 2,
    automlr.preproc.numerics = "np2$np1",
    automlr.preproc.ordered = "op2$op1",
    automlr.preproc.factors = "fp1",
    reference.cat = FALSE,
    pca.scale = TRUE, pca.center = FALSE,
    scale.scale = FALSE, scale.center = TRUE,
    automlr.wrapafterconvert.ordered = TRUE,
    automlr.wrapafterconvert.numerics = FALSE,
    MissingsFactorsNumericsLearner.bool1 = TRUE,
    automlr.wrapafterconvert.factors = FALSE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "MissingsFactorsNumericsLearner", list(real1 = 10, bool1 = TRUE), list(),
    ordimputer2 = list(),
    numimputer2 = list(multiplier = 1),
    fp1 = list(),
    op2 = list(),
    op1 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE),
    fnconv1 = list(reference.cat = FALSE),
    onconv2 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE),
    nfconv1 = list(numsplits = 2))

  set.seed(123)

  nfc = MissingsNumericsOrderedTask %>>%
    cpoSelect("numeric") %>>%
    cpoImputeMin(make.dummy.cols = FALSE, multiplier = 1) %>>%
    cpoScale(center = TRUE, scale = FALSE) %>>%
    cpoPca(center = FALSE, scale = TRUE) %>>%
    splitnumcpo(2)
  ofc = MissingsNumericsOrderedTask %>>%
    cpoSelect("ordered") %>>%
    cpoImputeHist(make.dummy.cols = FALSE) %>>%
    reversefacorder() %>>%
    cpoToBinaryFc() %>>%
    asnumcpo() %>>%
    cpoScale(center = TRUE, scale = FALSE) %>>%
    cpoPca(center = FALSE, scale = TRUE)
  ffc = MissingsNumericsOrderedTask %>>%
    cpoMissingIndicators() %>>%
    cpoDummyEncode(FALSE)
  expected = cbind(getTaskData(nfc, target.extra = TRUE)$data, getTaskData(ofc, target.extra = TRUE)$data,
    getTaskData(ffc, target.extra = TRUE)$data, fac.1 = getTaskData(ffc, target.extra = TRUE)$target)

  checkLearnerData(l, pvs, MissingsNumericsOrderedTask, expected)

  pvs = list(selected.learner = "MissingsFactorsNumericsLearner", automlr.missing.indicators = TRUE,
    automlr.impute = TRUE,
    automlr.convert.factors = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2", multiplier = 1,
    automlr.wimputing.ordered = "ordimputer2",
    automlr.convert.ordered.to = "numerics",
    automlr.wconverting.ordered.to.numerics = "onconv2",
    automlr.wconverting.numerics.to.factors = "nfconv1",
    numsplits = 2,
    automlr.preproc.numerics = "np2$np1",
    automlr.preproc.ordered = "op2$op1",
    automlr.preproc.factors = "fp1",
    reference.cat = FALSE,
    pca.scale = TRUE, pca.center = FALSE,
    scale.scale = FALSE, scale.center = TRUE,
    automlr.wrapafterconvert.ordered = TRUE,
    automlr.wrapafterconvert.numerics = TRUE,
    MissingsFactorsNumericsLearner.bool1 = TRUE,
    automlr.wrapafterconvert.factors = FALSE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "MissingsFactorsNumericsLearner", list(real1 = 10, bool1 = TRUE), list(),
    ordimputer2 = list(),
    numimputer2 = list(multiplier = 1),
    fp1 = list(),
    op2 = list(),
    op1 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE),
    fnconv1 = list(reference.cat = FALSE),
    onconv2 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE),
    nfconv1 = list(numsplits = 2),
    fp1 = list())

  set.seed(123)
  nfc = MissingsNumericsOrderedTask %>>%
    cpoSelect("numeric") %>>%
    cpoImputeMin(make.dummy.cols = FALSE, multiplier = 1) %>>%
    cpoScale(center = TRUE, scale = FALSE) %>>%
    cpoPca(center = FALSE, scale = TRUE) %>>%
    splitnumcpo(2) %>>%
    cpoToBinary()
  ofc = MissingsNumericsOrderedTask %>>%
    cpoSelect("ordered") %>>%
    cpoImputeHist(make.dummy.cols = FALSE) %>>%
    reversefacorder() %>>%
    cpoToBinaryFc() %>>%
    asnumcpo() %>>%
    cpoScale(center = TRUE, scale = FALSE) %>>%
    cpoPca(center = FALSE, scale = TRUE)
  ffc = MissingsNumericsOrderedTask %>>%
    cpoMissingIndicators() %>>%
    cpoDummyEncode(FALSE)
  expected = cbind(getTaskData(nfc, target.extra = TRUE)$data, getTaskData(ofc, target.extra = TRUE)$data,
    getTaskData(ffc, target.extra = TRUE)$data, fac.1 = getTaskData(ffc, target.extra = TRUE)$target)

  checkLearnerData(l, pvs, MissingsNumericsOrderedTask, expected)

  pvs = list(selected.learner = "MissingsFactorsNumericsLearner", automlr.missing.indicators = TRUE,
    automlr.impute = FALSE,
    automlr.convert.factors = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.ordered.to = "numerics",
    automlr.wconverting.ordered.to.numerics = "onconv2",
    automlr.wconverting.numerics.to.factors = "nfconv1",
    numsplits = 2,
    automlr.preproc.numerics = "np2$np1",
    automlr.preproc.ordered = "op2$op1",
    automlr.preproc.factors = "fp1",
    reference.cat = FALSE,
    pca.scale = TRUE, pca.center = FALSE,
    scale.scale = FALSE, scale.center = TRUE,
    automlr.wrapafterconvert.ordered = TRUE,
    automlr.wrapafterconvert.numerics = TRUE,
    automlr.wrapafterconvert.factors = FALSE,
    MissingsFactorsNumericsLearner.int1 = 4)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "MissingsFactorsNumericsLearner", list(int1 = 4, real1 = 10), list(),
    fp1 = list(),
    op2 = list(),
    op1 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE),
    fnconv1 = list(reference.cat = FALSE),
    onconv2 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE),
    nfconv1 = list(numsplits = 2),
    fp1 = list())

  set.seed(123)
  nfc = MissingsNumericsOrderedTask %>>%
    cpoSelect("numeric") %>>%
    cpoScale(center = TRUE, scale = FALSE) %>>%
    cpoImputeMedian(id = "pcaimp", make.dummy.cols = FALSE) %>>%
    cpoPca(center = FALSE, scale = TRUE) %>>%
    splitnumcpo(2) %>>%
    cpoToBinary()
  ofc = MissingsNumericsOrderedTask %>>%
    cpoSelect("ordered") %>>%
    reversefacorder() %>>%
    cpoToBinaryFc() %>>%
    asnumcpo() %>>%
    cpoScale(center = TRUE, scale = FALSE) %>>%
    cpoImputeMedian(id = "pcaimp", make.dummy.cols = FALSE) %>>%
    cpoPca(center = FALSE, scale = TRUE)
  ffc = MissingsNumericsOrderedTask %>>%
    cpoMissingIndicators() %>>%
    cpoDummyEncode(FALSE)
  expected = cbind(getTaskData(nfc, target.extra = TRUE)$data, getTaskData(ofc, target.extra = TRUE)$data,
    getTaskData(ffc, target.extra = TRUE)$data, fac.1 = getTaskData(ffc, target.extra = TRUE)$target)

  checkLearnerData(l, pvs, MissingsNumericsOrderedTask, expected)

  pvs = list(selected.learner = "MissingsFactorsNumericsLearner", automlr.missing.indicators = TRUE,
    automlr.impute = TRUE,
    automlr.convert.factors = TRUE,
    automlr.convert.numerics = TRUE,
    automlr.convert.before.impute = TRUE,
    automlr.wimputing.factors = "factimputer1",
    fimp.const = "NAx",
    automlr.convert.ordered.to = "factors",
    automlr.wconverting.ordered.to.factors = "ofconv2",
    ofconv.numsplits = 2,
    automlr.wconverting.numerics.to.factors = "nfconv1",
    numsplits = 2,
    automlr.preproc.numerics = "np2$np1",
    automlr.preproc.factors = "fp1",
    reference.cat = FALSE,
    pca.scale = TRUE, pca.center = FALSE,
    scale.scale = FALSE, scale.center = TRUE,
    MissingsFactorsNumericsLearner.bool1 = TRUE)
  checkLearnerBehaviour(l, MissingsNumericsOrderedTask,
    pvs, "MissingsFactorsNumericsLearner", list(real1 = 10, bool1 = TRUE), list(),
    fnconv1 = list(reference.cat = FALSE),
    ofconv2 = list(ofconv.numsplits = 2),
    nfconv1 = list(numsplits = 2),
    factimputer1 = list(fimp.const = "NAx"),
    fp1 = list(),
    np2 = list(scale.scale = FALSE, scale.center = TRUE),
    np1 = list(pca.scale = TRUE, pca.center = FALSE))

  set.seed(123)
  ofc = MissingsNumericsOrderedTask %>>%
    cpoSelect(c("numeric", "ordered")) %>>%
    asnumcpo() %>>%
    splitnumcpo(2) %>>%
    cpoImputeConstant(const = "NAx", make.dummy.cols = FALSE) %>>%
    cpoToBinary()
  ffc = MissingsNumericsOrderedTask %>>%
    cpoMissingIndicators() %>>%
    cpoDummyEncode(FALSE) %>>%
    cpoScale(center = TRUE, scale = FALSE) %>>%
    cpoPca(center = FALSE, scale = TRUE)
  expected = cbind(getTaskData(ofc, target.extra = TRUE)$data,
    getTaskData(ffc, target.extra = TRUE)$data, fac.1 = getTaskData(ffc, target.extra = TRUE)$target)
  checkLearnerData(l, pvs, MissingsNumericsOrderedTask, expected)

})


