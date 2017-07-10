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




