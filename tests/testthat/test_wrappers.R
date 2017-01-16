# Testing that wrappers generate a correct search space, get executed in the right
# order, with the right parameters
context("wrappers")

test_that("wrappers are used in the expected way", {
  # define wrappers, some of them 'required', with some parameters
  w1 = autolearner(
      autoWrapper("w1", function(learner, ...) changeColsWrapper(learner, "w1", ...), identity),
      list(sp("w1.spare1", "int", c(0, 10))),
      "wrapper")
  w1r = autolearner(
      autoWrapper("w1r", function(learner, ...) changeColsWrapper(learner, "w1r", ...), identity),
      list(sp("w1r.spare1", "int", c(0, 10), special = 'dummy')),
      "requiredwrapper")
  w2 = autolearner(
      autoWrapper("w2", function(learner, ...) changeColsWrapper(learner, "w2", ...), identity),
      list(sp("w2.spare1", "int", c(0, 10)),
           sp("w2.spare2", "int", c(0, 10), req = quote(w2.spare1==1))),
      "wrapper")
  w2r = autolearner(
      autoWrapper("w2r", function(learner, ...) changeColsWrapper(learner, "w2r", ...), identity),
      list(sp("w2r.spare1", "fix", 2, special = 'dummy'),
           sp("w2r.spare2", "int", c(0, 10), req = quote(w2r.spare1==1)),
           sp("w2r.spare2.AMLRFIX1", "fix", 9, req = quote(w2r.spare1==2))),
      "requiredwrapper")
  # define autolearners with a parameter
  test1 = autolearner(
      testLearner("test1", makeParamSet(predefParams$int1), c("numerics", "twoclass")),
      list(sp("int1", "int", c(0, 10))))
  test2 = autolearner(
      testLearner("test2", makeParamSet(predefParams$int1), c("numerics", "twoclass")),
      list(sp("int1", "int", c(0, 10))))

  # test the resulting parameter sets contain the expected parameters.
  # in particular, a single required wrapper entails no automlr.wrappersetup
  expect_set_equal(getParamIds(getParamSet(bl(test1, test2))), c("selected.learner", "test1.int1", "test2.int1"))
  expect_set_equal(getParamIds(getParamSet(bl(w1r, test1, test2))), c("selected.learner", "test1.int1", "test2.int1", "w1r.spare1"))
  expect_set_equal(getParamIds(getParamSet(bl(w1r, test1, test2))), c("selected.learner", "test1.int1", "test2.int1", "w1r.spare1"))
  expect_set_equal(getParamIds(getParamSet(bl(w1, test1, test2))), c("selected.learner", "test1.int1", "test2.int1", "w1.spare1", "automlr.wrappersetup"))
  expect_set_equal(getParamIds(getParamSet(bl(w1, w1r, test1, test2))),
                   c("selected.learner", "test1.int1", "test2.int1", "w1r.spare1", "w1.spare1", "automlr.wrappersetup"))

  # for a search space including a nonrequired wrapper, test that:
  # - the parameter space is the expected one, with the expected automlr.wrappersetup
  # - the dependency of parameters on the automlr.wrappersetup is correct
  # - the expected wrappers show up on learner execution
  lw1w1r = bl(w1, w1r, test1, test2)
  lw1w1rPS = getParamSet(lw1w1r)
  expect_set_equal(getParamIds(lw1w1rPS),  c("selected.learner", "test1.int1", "test2.int1", "w1.spare1", "w1r.spare1", "automlr.wrappersetup"))
  expect_set_equal(as.character(lw1w1rPS$pars$automlr.wrappersetup$values), c("w1r", "w1$w1r", "w1r$w1"))
  expect_true(isFeasibleNoneMissing(lw1w1rPS, list(test1.int1 = 1, w1r.spare1 = 4, selected.learner = "test1", automlr.wrappersetup = "w1r")))
  expect_true(isFeasibleNoneMissing(lw1w1rPS, list(test1.int1 = 1, w1.spare1 = 3, w1r.spare1 = 4, selected.learner = "test1", automlr.wrappersetup = "w1$w1r")))
  lx = setHyperPars(lw1w1r, test1.int1 = 1, selected.learner = "test1", w1r.spare1 = 4, automlr.wrappersetup = "w1r")
  expect_learner_output(lx, pid.task, "test1", list(int1 = 1), list(), w1r = list(w1r.spare1 = 0, w1r.spare2 = 0))
  lx = setHyperPars(lw1w1r, test2.int1 = 2, selected.learner = "test2", w1r.spare1 = 4, w1.spare1 = 9, automlr.wrappersetup = "w1r$w1")
  expect_learner_output(lx, pid.task, "test2", list(int1 = 2), list(), w1r = list(w1r.spare1 = 0, w1r.spare2 = 0), w1 = list(w1.spare1 = 9, w1.spare2 = 0))

  lw1 = bl(w1, test1, test2)
  lw1PS = getParamSet(lw1)
  expect_set_equal(getParamIds(lw1PS),  c("selected.learner", "test1.int1", "test2.int1", "w1.spare1", "automlr.wrappersetup"))
  expect_set_equal(as.character(lw1PS$pars$automlr.wrappersetup$values), c("$", "w1"))
  expect_true(isFeasibleNoneMissing(lw1PS, list(test1.int1 = 1, selected.learner = "test1", automlr.wrappersetup = "$")))
  expect_true(isFeasibleNoneMissing(lw1PS, list(test1.int1 = 1, w1.spare1 = 3, selected.learner = "test1", automlr.wrappersetup = "w1")))
  lx = setHyperPars(lw1, test1.int1 = 1, selected.learner = "test1", automlr.wrappersetup = "$")
  expect_learner_output(lx, pid.task, "test1", list(int1 = 1), list())
  lx = setHyperPars(lw1, test2.int1 = 2, selected.learner = "test2", w1.spare1 = 8, automlr.wrappersetup = "w1")
  expect_learner_output(lx, pid.task, "test2", list(int1 = 2), list(), w1 = list(w1.spare1 = 8, w1.spare2 = 0))

  # for a larger set of wrappers, check that:
  # - the resulting parameter set is correct
  # - the expected parameters are feasible
  # - the wrappers show up on learner execution in correct order and with correct parameters
  allwrappers = bl(w1, w2, w1r, w2r, test1, test2)
  awPS = getParamSet(allwrappers)
  expect_set_equal(getParamIds(awPS),c("selected.learner", "test1.int1", "test2.int1", "w1.spare1",
                                       "w1r.spare1", "w2.spare1", "w2.spare2", "automlr.wrappersetup"))
  parSet = list(test2.int1 = 3, selected.learner = "test2", automlr.wrappersetup = "w2r$w1r", w1r.spare1 = 1)
  expect_true(isFeasibleNoneMissing(awPS, parSet))
  expect_learner_output(setHyperPars(allwrappers, par.vals = parSet), pid.task, "test2", list(int1 = 3), list(), w2r = list(w2r.spare1 = 0, w2r.spare2 = 9),
                        w1r = list(w1r.spare1 = 0, w1r.spare2 = 0))

  parSet = list(test2.int1 = 4, selected.learner = "test2", automlr.wrappersetup = "w1r$w2$w2r", w1r.spare1 = 2, w2.spare1 = 1, w2.spare2 = 6)
  expect_true(isFeasibleNoneMissing(awPS, parSet))
  expect_learner_output(setHyperPars(allwrappers, par.vals = parSet), pid.task, "test2", list(int1 = 4), list(), w1r = list(w1r.spare1 = 0, w1r.spare2 = 0),
                        w2 = list(w2.spare1 = 1, w2.spare2 = 6), w2r = list(w2r.spare1 = 0, w2r.spare2 = 9))

  parSet = list(test1.int1 = 5, selected.learner = "test1", automlr.wrappersetup = "w1$w1r$w2r$w2", w1r.spare1 = 3, w2.spare1 = 2, w1.spare1 = 8)
  expect_true(isFeasibleNoneMissing(awPS, parSet))
  expect_learner_output(setHyperPars(allwrappers, par.vals = parSet), pid.task, "test1", list(int1 = 5), list(),
                        w1 = list(w1.spare1 = 8, w1.spare2 = 0),
                        w1r = list(w1r.spare1 = 0, w1r.spare2 = 0),
                        w2r = list(w2r.spare1 = 0, w2r.spare2 = 9),
                        w2 = list(w2.spare1 = 2, w2.spare2 = 0))

})




