# Tests that the correct learners are chosen for a given task and wrappers.

context("taskbuilding")

# give empty result or error if searchspace has no appropriate or bad searchspace
test_that("trivial learners are returned when appropriate", {
  taskNormal = createTestClassifTask("t1", 20, nNumeric = 1)
  taskWWeights = createTestClassifTask("t1", 20, nNumeric = 1, weights = rpois(20, 1))
  trivialLearner1 = list(autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass"))))
  trivialLearner2 = list(autolearner(testLearner("test", makeParamSet(), c("factors", "twoclass"))))

  # using learner that fits the task works 
  expect_class(buildLearners(trivialLearner1, taskNormal), "RLearnerClassif")

  # empty searchspace gives NULL result
  expect_warning(lrns <- buildLearners(list(), pid.task), 'No model fits the given task')
  expect_null(lrns)

  # searchspace with no fitting learner gives NULL result
  expect_warning(res <- buildLearners(trivialLearner2, taskNormal), 'No model fits the given task')
  expect_null(res)

  # duplicate learner ID gives error
  expect_error(buildLearners(c(trivialLearner1, trivialLearner1), taskNormal), "Base learners must all have unique ids")

  # weights are not supported
  expect_error(buildLearners(trivialLearner1, taskWWeights), "Tasks with weights are currently not supported")
  expect_error(buildLearners(trivialLearner2, taskWWeights), "Tasks with weights are currently not supported")
})

# the learners that remain in the searchspace fit to the task and the possible transformations
test_that("the right kind of learners are filtered out", {
  # first we need different tasks with different properties
  # binary classification
  t.n2 = createTestClassifTask("t.n2", 200, nNumeric = 1)
  t.f2 = createTestClassifTask("t.f2", 200, nFactor = 1)
  t.o2 = createTestClassifTask("t.o2", 200, nOrdered = 1)
  # multiclass classification
  t.nx = createTestClassifTask("t.nx", 200, nNumeric = 1, nClasses = 25)
  t.fx = createTestClassifTask("t.fx", 200, nFactor = 1, nClasses = 25)
  t.ox = createTestClassifTask("t.ox", 200, nOrdered = 1, nClasses = 25)
  # binary with missings
  t.nm2 = createTestClassifTask("t.nm2", 200, nNumeric = 1, missings = TRUE)
  t.fm2 = createTestClassifTask("t.fm2", 200, nFactor = 1, missings = TRUE)
  t.om2 = createTestClassifTask("t.om2", 200, nOrdered = 1, missings = TRUE)
  # multi with missings
  t.nmx = createTestClassifTask("t.nmx", 200, nNumeric = 1, nClasses = 3, missings = TRUE)
  t.fmx = createTestClassifTask("t.fmx", 200, nFactor = 1, nClasses = 3, missings = TRUE)

  # test that from the autolearners, the present learners in the resulting searchspace fit to the task.
  # checkLearnersPresent is called for each of the above tasks which checks the appropriate learner is present for it.
  # the transformation function returns a list of vectors that represent possible requirements to the present learners
  checkWrapperEffect = function(autolearnersPL, transformation = list, ...) {
    checkLearnersPresent(autolearnersPL, t.n2, transformation(c("numerics", "twoclass")), ...)
    checkLearnersPresent(autolearnersPL, t.f2, transformation(c("factors", "twoclass")), ...)
    checkLearnersPresent(autolearnersPL, t.o2, transformation(c("ordered", "twoclass")), ...)
    checkLearnersPresent(autolearnersPL, t.nx, transformation(c("numerics", "multiclass")), ...)
    checkLearnersPresent(autolearnersPL, t.fx, transformation(c("factors", "multiclass")), ...)
    checkLearnersPresent(autolearnersPL, t.ox, transformation(c("ordered", "multiclass")), ...)
    checkLearnersPresent(autolearnersPL, t.nm2, transformation(c("numerics", "twoclass", "missings")), ...)
    checkLearnersPresent(autolearnersPL, t.fm2, transformation(c("factors", "twoclass", "missings")), ...)
    checkLearnersPresent(autolearnersPL, t.om2, transformation(c("ordered", "twoclass", "missings")), ...)
    checkLearnersPresent(autolearnersPL, t.nmx, transformation(c("numerics", "multiclass", "missings")), ...)
    checkLearnersPresent(autolearnersPL, t.fmx, transformation(c("factors", "multiclass", "missings")), ...)
  }

  # wrappers that anounce that they perform transformations:
  # removing missings, factors, ordereds, missings-and-factors
  # converting ordereds to numerics, factors to numerics
  # combined removing of factors and missings and conversion of factors to numerics
  MRemover = autoWrapper("M.R", identity, function(x) switch(x, missings = c("missings", ""), x))
  FRemover = autoWrapper("F.R", identity, function(x) switch(x, factors = c("factors", ""), x))
  ORemover = autoWrapper("O.R", identity, function(x) switch(x, ordered = c("ordered", ""), x))
  MFRemover = autoWrapper("MF.R", identity, function(x) switch(x, missings = c("missings", ""), factors = c("factors", ""), x))
  OConverter = autoWrapper("O.C", identity, function(x) switch(x, ordered = c("ordered", "numerics"), x))
  FConverter = autoWrapper("F.C", identity, function(x) switch(x, factors = c("factors", "numerics"), x))
  MFRemoverFConverter = autoWrapper("MF.R.F.C", identity, function(x) switch(x, factors = c("factors", "numerics", ""), missings = c("missings", ""), x))

  # check that without wrappers, exactly those learners remain that fit the tasks
  autolearnersPL = autolearnersBASIC
  checkWrapperEffect(autolearnersPL)
  checkWrapperEffectEx(autolearnersPL)

  # non-required wrappers have no effect. TODO: this should not be.
  for (uneffectiveWrapper in list(MRemover, FRemover, ORemover, MFRemover, OConverter, FConverter, MFRemoverFConverter)) {
    autolearnersPL = c(autolearnersBASIC, list(autolearner(uneffectiveWrapper, stacktype = "wrapper")))
    checkWrapperEffect(autolearnersPL)
    checkWrapperEffectEx(autolearnersPL)
  }

  # learners that cannot handle a certain property are present if the appropriate removing wrapper is present.
  # test for: missings, factors, ordereds, factors-and-missings
  # test for combination of multiple wrappers: missings + factors, factors-and-missings + ordereds,
  # test interplay of converters and removers
  autolearnersPL = c(autolearnersBASIC, list(autolearner(MRemover, stacktype = "requiredwrapper")))
  checkWrapperEffect(autolearnersPL, function(x) list(setdiff(x, "missings")))
  checkWrapperEffectEx(autolearnersPL, function(x) list(setdiff(x, "missings")))

  autolearnersPL = c(autolearnersBASIC, list(autolearner(FRemover, stacktype = "requiredwrapper")))
  checkWrapperEffect(autolearnersPL, function(x) list(x, setdiff(x, "factors")))
  checkWrapperEffectEx(autolearnersPL, function(x) list(x, setdiff(x, "factors")))

  autolearnersPL = c(autolearnersBASIC, list(autolearner(ORemover, stacktype = "requiredwrapper")))
  checkWrapperEffect(autolearnersPL, function(x) list(x, setdiff(x, "ordered")))
  checkWrapperEffectEx(autolearnersPL, function(x) list(x, setdiff(x, "ordered")))

  autolearnersPL = c(autolearnersBASIC, list(autolearner(MFRemover, stacktype = "requiredwrapper")))
  checkWrapperEffect(autolearnersPL, function(x) lapply(list(c("factors", "missings"), "missings"), setdiff, x = x))
  checkWrapperEffectEx(autolearnersPL, function(x) lapply(list(c("factors", "missings"), "missings"), setdiff, x = x))

  autolearnersPL = c(autolearnersBASIC, list(autolearner(MRemover, stacktype = "requiredwrapper"), autolearner(FRemover, stacktype = "requiredwrapper")))
  checkWrapperEffect(autolearnersPL, function(x) lapply(list(c("factors", "missings"), "missings"), setdiff, x = x))
  checkWrapperEffectEx(autolearnersPL, function(x) lapply(list(c("factors", "missings"), "missings"), setdiff, x = x))

  # if a wrapper is not required, it is ignored.
  autolearnersPL = c(autolearnersBASIC, list(autolearner(MRemover, stacktype = "requiredwrapper"), autolearner(FRemover, stacktype = "wrapper")))
  checkWrapperEffect(autolearnersPL, function(x) list(setdiff(x, "missings")))
  checkWrapperEffectEx(autolearnersPL, function(x) list(setdiff(x, "missings")))

  # when removing more than one factor type, we still remove these types from the requirements list. however, each learner must support at least
  # one of the factor types present, therefore the complicated transformation function.
  autolearnersPL = c(autolearnersBASIC, list(autolearner(MFRemover, stacktype = "requiredwrapper"), autolearner(ORemover, stacktype = "requiredwrapper")))
  checkWrapperEffect(autolearnersPL, function(x) lapply(list("ordered", "factors", c("ordered", "factors")), function(y) setdiff(x, c("missings", y))))
  checkWrapperEffectEx(autolearnersPL, function(x) lapply(list("ordered", "factors", c("ordered", "factors")), function(y) setdiff(x, c("missings", y))))

  # the MFRemoverFConverter can either remove factors or convert them to numerics
  autolearnersPL = c(autolearnersBASIC, list(autolearner(MFRemoverFConverter, stacktype = "requiredwrapper")))
  checkWrapperEffect(autolearnersPL, function(x) lapply(list(x, setdiff(x, "factors"), sub("factors", "numerics", x)), setdiff, y = "missings"))
  checkWrapperEffectEx(autolearnersPL, function(x) lapply(list(x, setdiff(x, "factors"), sub("factors", "numerics", x)), setdiff, y = "missings"))

  # fconverter + mfremover has the same effect
  autolearnersPL = c(autolearnersBASIC, list(autolearner(FConverter, stacktype = "requiredwrapper"), autolearner(MFRemover, stacktype = "requiredwrapper")))
  checkWrapperEffect(autolearnersPL, function(x) lapply(list(x, setdiff(x, "factors"), sub("factors", "numerics", x)), setdiff, y = "missings"))
  checkWrapperEffectEx(autolearnersPL, function(x) lapply(list(x, setdiff(x, "factors"), sub("factors", "numerics", x)), setdiff, y = "missings"))

  # TODO: it is natural that the following fails, but it would be nice if it worked.
  #       TODO: why does it fail? It shouldn't?
  #autolearnersPL = c(autolearnersBASIC, list(autolearner(OConverter, stacktype = "requiredwrapper")))
  #checkWrapperEffect(autolearnersPL, function(x) list(x, sub("ordered", "numerics", x)))
  #checkWrapperEffectEx(autolearnersPL, function(x) list(x, sub("ordered", "numerics", x)))
  #
  #autolearnersPL = c(autolearnersBASIC, list(autolearner(FConverter, stacktype = "requiredwrapper")))
  #checkWrapperEffect(autolearnersPL, function(x) list(x, sub("factors", "numerics", x)))
  #checkWrapperEffectEx(autolearnersPL, function(x) list(x, sub("factors", "numerics", x)))
  #
  #autolearnersPL = c(autolearnersBASIC, list(
  #    autolearner(FConverter, stacktype = "requiredwrapper"),
  #    autolearner(MFRemover, stacktype = "requiredwrapper"),
  #    autolearner(ORemover, stacktype = "requiredwrapper")))
  #chgfun = function(x) {
  #  urlist = list(x, setdiff(x, "factors"), sub("factors", "numerics", x))
  #  c(lapply(urlist, setdiff, y = "missings"), lapply(urlist, setdiff, y = c("missings", "ordered")))
  #}
  # TODO: the following should work
  #checkWrapperEffect(autolearnersPL, chgfun)
  #checkWrapperEffectEx(autolearnersPL, chgfun)
  ###
})
