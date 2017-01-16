# Test that feature types influence the presence and absence of learners, wrapper parameters and pseudoparameters
# in the right way.
context("coltypes")

# define tasks and learnerswfor factors, numerics, ordereds, numerics+factors; each with and without missings
# also make numerics+ordered task with and without missings, and make an additional learner for all things
NumericsTask = createTestClassifTask("NumericsTask", 200, nNumeric = 3)
FactorsTask = createTestClassifTask("FactorsTask", 200, nFactor = 3)
OrderedTask = createTestClassifTask("OrderedTask", 200, nOrdered = 3)
NumericsFactorsTask = createTestClassifTask("NumericsFactorsTask", 200, nNumeric = 3, nFactor = 3)
NumericsOrderedTask = createTestClassifTask("NumericsOrderedTask", 200, nNumeric = 3, nOrdered = 3)
MissingsNumericsTask = createTestClassifTask("MissingsNumericsTask", 200, nNumeric = 3, missings = TRUE)
MissingsFactorsTask = createTestClassifTask("MissingsFactorsTask", 200, nFactor = 3, missings = TRUE)
MissingsOrderedTask = createTestClassifTask("MissingsOrderedTask", 200, nOrdered = 3, missings = TRUE)
MissingsNumericsFactorsTask = createTestClassifTask("MissingsNumericsFactorsTask", 200, nNumeric = 3, nFactor = 3, missings = TRUE)
MissingsNumericsOrderedTask = createTestClassifTask("MissingsNumericsOrderedTask", 200, nNumeric = 3, nOrdered = 3, missings = TRUE)

NumericsLearner = autolearner(
    testLearner("NumericsLearner", makeParamSet(predefParams$int1), c("numerics", "twoclass")),
    list(sp("int1", "int", c(0, 10))))

FactorsLearner = autolearner(
    testLearner("FactorsLearner", makeParamSet(predefParams$int1), c("factors", "twoclass")),
    list())

OrderedsLearner = autolearner(
    testLearner("OrderedsLearner", makeParamSet(predefParams$int1), c("ordered", "twoclass")),
    list())

MissingsNumericsLearner = autolearner(
    testLearner("MissingsNumericsLearner", makeParamSet(predefParams$int1), c("numerics", "twoclass", "missings")),
    list(sp("int1", "int", c(0, 10), req = quote(automlr.has.missings==TRUE))))

MissingsFactorsLearner = autolearner(
    testLearner("MissingsFactorsLearner", makeParamSet(predefParams$int1), c("factors", "twoclass", "missings")),
    list())

MissingsFactorsNumericsLearner = autolearner(
    testLearner("MissingsFactorsNumericsLearner", makeParamSet(predefParams$int1, predefParams$real1, predefParams$bool1),
                c("numerics", "twoclass", "missings", "factors")),
    list(sp("int1", "int", c(0, 10), req = quote(automlr.has.missings==TRUE)),
         sp("real1", "real", c(10, 10), req = quote(automlr.has.factors==TRUE)),
         sp("bool1", "bool", req = quote(automlr.has.missings != automlr.has.factors))))

FactorsNumericsLearner = autolearner(
    testLearner("FactorsNumericsLearner", makeParamSet(predefParams$real1, predefParams$bool1), c("numerics", "twoclass", "factors")),
    list(sp("real1", "real", c(0, 10), req = quote(automlr.has.factors==FALSE)),
         sp("bool1", "cat", FALSE, req = quote(automlr.has.missings == automlr.has.factors)),
         sp("bool1.AMLRFIX1", "cat", TRUE, req = quote(automlr.has.missings != automlr.has.factors))))

AllLearner = autolearner(
    testLearner("AllLearner", makeParamSet(predefParams$int1, predefParams$real1, predefParams$bool1), c("numerics", "twoclass", "factors", "ordered", "missings")),
    list(sp("int1", "int", c(0, 10), req = quote(automlr.has.missings==FALSE)),
         sp("real1", "real", c(0, 10), req = quote(automlr.has.factors %in% c(TRUE, FALSE))),
         sp("bool1", "fix", TRUE, req = quote(automlr.has.ordered == automlr.has.factors)),
         sp("int1.AMLRFIX1", "int", c(2, 2), req = quote(automlr.has.missings==TRUE && automlr.has.factors == TRUE)),
         sp("int1.AMLRFIX2", "int", c(11, 20), req = quote(automlr.has.missings==TRUE && automlr.has.factors == FALSE))))

# make removing wrappers for missings, factors, missings+factors, and 'nothing'
XRemover = autolearner(
    autoWrapper("XRemover", function(learner, ...) changeColsWrapper(learner, "XRemover", ...), identity),
    list(sp("XRemover.spare1", "int", c(0, 10), req = quote(2 %in% c(1, 2, 3))),
         sp("XRemover.spare2", "fix", 9, req = quote(automlr.has.missings==TRUE))),
    "requiredwrapper")

NARemover = autolearner(
    autoWrapper("NARemover", function(learner, ...) changeColsWrapper(learner, "NARemover", ...), function(x) switch(x, missings = c("missings", ""))),
    list(sp("NARemover.remove.NA", "fix", TRUE, req = quote(automlr.remove.missings == TRUE))),
    "requiredwrapper")

FactorRemover = autolearner(
    autoWrapper("FactorRemover", function(learner, ...) changeColsWrapper(learner, "FactorRemover", ...), function(x) switch(x, factors = c("factors", ""))),
    list(sp("FactorRemover.remove.factors", "fix", FALSE, req = quote(automlr.remove.factors == FALSE)),
         sp("FactorRemover.convertFactors", "bool", special = "dummy", req = quote(automlr.remove.factors == TRUE && automlr.has.numerics == TRUE)),
         sp("FactorRemover.convert.fac2num", "fix", TRUE, req = quote(automlr.remove.factors && automlr.has.numerics && FactorRemover.convertFactors)),
         sp("FactorRemover.remove.factors.AMLRFIX2", "fix", TRUE, req = quote(automlr.remove.factors && (!automlr.has.numerics || !FactorRemover.convertFactors)))),
    "requiredwrapper")

NAFactorRemover = autolearner(
    autoWrapper("NAFactorRemover", function(learner, ...) changeColsWrapper(learner, "NAFactorRemover", ...),
                function(x) switch(x, missings = c("missings", ""), factors = c("factors", ""))),
    list(sp("NAFactorRemover.remove.NA", "fix", TRUE, req = quote(automlr.remove.missings == TRUE)),
         sp("NAFactorRemover.remove.factors", "fix", TRUE, req = quote(automlr.remove.factors == TRUE))),
    "requiredwrapper")


# the subset of learners that can use the given task is chosen
test_that("the correct learner is automatically chosen", {
  # test that '2 %in% c(1, 2, 3)' is removed since it is always true.
  l = buildLearners(list(NumericsLearner, XRemover), NumericsTask)
  expect_null(getParamSet(l)$pars$XRemover.spare1$requires)

  # only numericslearner remains for numericstask
  l = buildLearners(list(NumericsLearner, FactorsLearner, OrderedsLearner), NumericsTask)
  checkLearnerBehaviour(l, NumericsTask, list(NumericsLearner.int1 = 1), "NumericsLearner", list(int1 = 1), list())

  # both numericslearner and missingsnumericslearner work on numericstask
  l = buildLearners(list(NumericsLearner, MissingsNumericsLearner), NumericsTask)
  checkLearnerBehaviour(l, NumericsTask, list(selected.learner = "MissingsNumericsLearner"),
                        "MissingsNumericsLearner", list(), list())

  # only missingsnumericslearner works on missingsnumericstask, int1 must be set since missings are present
  l = buildLearners(list(NumericsLearner, MissingsNumericsLearner), MissingsNumericsTask)
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



