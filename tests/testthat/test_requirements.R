# Check influence of requirements on presence/absence and range of parameters
# Also check interplay of automlr.has.X and automlr.remove.X requirements
context("requirements")

test_that("requirements including pseudoparameters are simplified as they should", {

  theTask = createTestClassifTask("MissingsNumericsFactorsTask", 200, nNumeric = 3, nFactor = 3, missings = TRUE)

  # simple learner with one parameter
  l0 = autolearner(
      testLearner("l0", makeParamSet(predefParams$int1), c("numerics", "twoclass")),
      list(sp("int1", "int", c(0, 10))))
  # learner with parameter requirements, among others depending on task properties
  l1 = autolearner(
      testLearner("l1", makeParamSet(predefParams$int1, predefParams$real1, predefParams$bool1, predefParams$cat1),
                  c("numerics", "twoclass", "factors", "ordered", "missings")),
      list(sp("int1", "int", c(0, 3), req = quote(automlr.has.missings==FALSE)),
           sp("int1.AMLRFIX1", "int", c(2, 2), req = quote(automlr.has.missings==TRUE && automlr.has.factors == FALSE)),
           sp("int1.AMLRFIX2", "int", c(3, 5), req = quote(automlr.has.missings==TRUE && automlr.has.factors == TRUE)),
           sp("intermediate", "real", c(0, 0), req = quote(int1==2), special = "dummy"),
           sp("intermediate2", "bool", req = quote(int1 == 0), special = "dummy"),
           sp("intermediate3", "bool", req = quote(int1==2 && intermediate == 0), special = "dummy"),
           sp("bool1", "fix", TRUE),
           sp("real1", "real", c(0, 10), req = quote(bool1 == TRUE)),
           sp("real1.AMLRFIX1", "real", c(10, 20), req = quote(bool1 == FALSE)),
           sp("cat1", "cat", c("a", "b"), req = quote(int1==2 && intermediate == 0 && intermediate3)),
           sp("cat1.AMLRFIX1", "cat", "c", req = quote(int1!=2)),
           sp("nevertrue", "bool", special = "dummy", req = quote(bool1 == FALSE))))

  # work with the learner containing all the above
  l = buildLearners(list(l0, l1, pWW(nimp1), pWW(nimp2), pWW(fimp1), pWW(fnconv1)), theTask, verbosity = 6)

  # all expected parameters are present
  expect_set_equal(getParamIds(getParamSet(l)),
    c("selected.learner", "automlr.impute", "automlr.convert", "automlr.convert.factors",
      "automlr.wimputing.numerics",  "automlr.missing.indicators", "automlr.convert.before.impute",
      "reference.cat", "fimp.const", "multiplier",
      "l0.int1", "l1.int1", "l1.int1.AMLRFIX2", "l1.intermediate2", "l1.intermediate3",
      "l1.real1", "l1.cat1"))

  # always true requirement is removed
  expect_equal(deparse(getParamSet(l)$pars[["l1.real1"]]$requires), 'selected.learner == "l1"')

  # check that parameter requirements work correctly:
  # for learners (l1):
  #  - l1.int1 needs to be given if there are no missings, or (AMLRFIX2) if missings and factors are present
  #  - l1.intermediate never shows up, since it is a fixed dummy
  #  - l1.intermediate2 is present if int1 is 0
  #  - l1.intermediate3 is present if int1 == 2 or if missings are present but factors are not
  #  - l1.real1 is always present, real1.AMLRFIX1 and l1.nevertrue is never present
  #  - l1.cat1 is present if intermediate3 is present and TRUE and defaults to c if int1 is not 2
  # for wrappers:
  #  - NAFactorRemover1 never shows up
  #  - NAFR2.remove.factors is TRUE as expected by virtue of NAFR2.intermediate assuming the correct value
  #  - NAFR2.intermediate2 present / absent depending on automlr.remove.x
  #  - NAFR2.spare1 present if removing factors, otherwise defaults to 4

  pvs = list(selected.learner = "l1",
    automlr.impute = TRUE, automlr.convert = TRUE, automlr.convert.factors = TRUE,
    automlr.missing.indicators = TRUE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer1",
    reference.cat = TRUE,
    fimp.const = "NAx",
    l1.int1 = 0, # also try others
    l1.intermediate2 = TRUE,
    l1.real1 = 5)

  checkLearnerBehaviour(l, theTask, pvs,
                        "l1", list(int1 = 0, bool1 = TRUE, real1 = 5, cat1 = "c"),
                        factimputer1 = list(fimp.const = "NAx"),
                        numimputer1 = list(),
                        fnconv1 = list(reference.cat = TRUE))

  pvs = list(selected.learner = "l1",
    automlr.impute = TRUE, automlr.convert = TRUE, automlr.convert.factors = TRUE,
    automlr.missing.indicators = TRUE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer1",
    reference.cat = TRUE,
    fimp.const = "NAx",
    l1.int1 = 0, # also try others
    l1.intermediate2 = TRUE,
    l1.real1 = 5)
  checkLearnerBehaviour(l, theTask, pvs,
                        "l1", list(int1 = 0, bool1 = TRUE, real1 = 5, cat1 = "c"),
                        factimputer1 = list(fimp.const = "NAx"),
                        numimputer1 = list(),
                        fnconv1 = list(reference.cat = TRUE))

  pvs = list(selected.learner = "l1",
    automlr.impute = TRUE, automlr.convert = TRUE, automlr.convert.factors = TRUE,
    automlr.missing.indicators = TRUE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2",
    multiplier = 2,
    reference.cat = TRUE,
    fimp.const = "NAx",
    l1.int1 = 2,
    l1.intermediate3 = TRUE,  # also try 'false'
    l1.real1 = 5,
    l1.cat1 = "b")
  checkLearnerBehaviour(l, theTask, pvs,
                        "l1", list(int1 = 2, bool1 = TRUE, real1 = 5, cat1 = "b"),
                        factimputer1 = list(fimp.const = "NAx"),
                        numimputer2 = list(multiplier = 2),
                        fnconv1 = list(reference.cat = TRUE))

  pvs = list(selected.learner = "l1",
    automlr.impute = TRUE, automlr.convert = TRUE, automlr.convert.factors = TRUE,
    automlr.missing.indicators = TRUE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2",
    multiplier = 2,
    reference.cat = TRUE,
    fimp.const = "NAx",
    l1.int1 = 2,
    l1.intermediate3 = FALSE,
    l1.real1 = 5)
  checkLearnerBehaviour(l, theTask, pvs,
                        "l1", list(int1 = 2, bool1 = TRUE, real1 = 5),
                        factimputer1 = list(fimp.const = "NAx"),
                        numimputer2 = list(multiplier = 2),
                        fnconv1 = list(reference.cat = TRUE))

  pvs = list(selected.learner = "l1",
    automlr.impute = TRUE, automlr.convert = TRUE, automlr.convert.factors = TRUE,
    automlr.missing.indicators = TRUE,
    automlr.convert.before.impute = FALSE,
    automlr.wimputing.numerics = "numimputer2",
    multiplier = 2,
    reference.cat = TRUE,
    fimp.const = "NAx",
    l1.int1 = 3,
    l1.real1 = 5)
  checkLearnerBehaviour(l, theTask, pvs,
                        "l1", list(int1 = 3, bool1 = TRUE, real1 = 5, cat1 = "c"),
                        factimputer1 = list(fimp.const = "NAx"),
                        numimputer2 = list(multiplier = 2),
                        fnconv1 = list(reference.cat = TRUE))


  pvs = list(selected.learner = "l1",
    automlr.impute = FALSE, automlr.convert = TRUE, automlr.convert.factors = TRUE,
    automlr.missing.indicators = TRUE,
    reference.cat = TRUE,
    l1.intermediate3 = FALSE,
    l1.real1 = 5)
  checkLearnerBehaviour(l, theTask, pvs,
                        "l1", list(int1 = 2, bool1 = TRUE, real1 = 5),
                        fnconv1 = list(reference.cat = TRUE))

  pvs = list(selected.learner = "l1",
    automlr.impute = FALSE, automlr.convert = FALSE,
    automlr.missing.indicators = TRUE,
    l1.int1.AMLRFIX2 = 4,
    l1.real1 = 5)
  checkLearnerBehaviour(l, theTask, pvs,
                        "l1", list(int1 = 4, bool1 = TRUE, real1 = 5, cat1 = "c"))


  pvs = list(selected.learner = "l0",
    automlr.missing.indicators = TRUE, automlr.wimputing.numerics = "numimputer2",
    multiplier = 2, reference.cat = TRUE, automlr.convert.before.impute = TRUE,
    l0.int1 = 3)
  checkLearnerBehaviour(l, theTask, pvs,
    "l0", list(int1 = 3),
    fnconv1 = list(reference.cat = TRUE),
    numimputer2 = list(multiplier = 2))

})

