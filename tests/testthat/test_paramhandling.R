# Testing that defaults and fixes are checked but not part of the searchspace, that the
# AMLRFIX things work, and that AMLRFIX conflicts give errors.
context("paramhandling")

# define an autolearner with a large number of parameters
outerSS = testLearner("test", makeParamSet(params = predefParams[c(
    "int1", "int2", "int3", "int4", "real1", "real2", "real3", "real4",
    "cat1", "cat2", "cat3", "cat4", "bool1", "bool2", "bool3")]), c("numerics", "twoclass"))
ssAL = autolearner(outerSS, list(
    sp("int0", "int", c(0, 10), special = "inject"),
    sp("int7", "int", c(0, 10), special = "dummy"),
    sp("int3", "int", c(0, 1), req = quote(int7==0)),
    sp("int3.AMLRFIX1", "int", c(2, 3), req = quote(int7==1)),
    sp("int3.AMLRFIX2", "int", c(4, 4), req = quote(int7==2)),
    sp("int1", "fix", 1),
    sp("real1", "fix", 0),
    sp("cat1", "fix", "a"),
    sp("bool1", "fix", FALSE),
    sp("real3", "def", 0),
    sp("cat3", "def", "a"),
    sp("bool3", "def", FALSE),
    sp("int2", "int", c(0, 10)),
    sp("real2", "real", c(0, 10)),
    sp("cat2", "cat", c("a", "b")),
    sp("bool2", "bool"),
    sp("int8", "fix", 0L, special = "inject"),
    sp("real0", "real", c(0, 10), special = "inject"),
    sp("cat0", "cat", c(0, 10), special = "inject"),
    sp("bool0", "bool", special = "inject"),
    sp("real7", "real", c(0, 10), special = "dummy"),
    sp("cat7", "cat", c(0, 10), special = "dummy"),
    sp("bool7", "bool", special = "dummy"),
    sp("cat8", "fix", "a", special = "inject"),
    sp("real8", "fix", 0.5, special = "inject"),
    sp("int4", "int", c(0, 0)),
    sp("real4", "real", c(0.5, 0.5)),
    sp("cat4", "cat", "a")))

# test that fixed parameters are removed, and that dependencies work.
test_that("buildLearners handles fixed, dummy, inject as required", {
  # expect warnings about fixed injects, missing requirement
  # test all expected parameters are present
  expect_warning(lss <- bl(ssAL), "(is marked 'dummy/inject' and has type 'fix'|'test' has a 'requires' argument but the one given)", all = TRUE)
  expect_set_equal(getParamIds(getParamSet(lss)),
      paste0("test.", c("int0", "int7", "int3", "int3.AMLRFIX1", "int2", "real2", "cat2", "bool2", "real0", "cat0", "bool0", "real7", "cat7", "bool7")))

  # test the influence of int7 on int3; the other parameters stay the same.
  # test int7 being 0, 1, 2.
  plss = getParamSet(lss)
  expect_true(isFeasibleNoneMissing(plss, x = list(
              test.int0 = 0, test.int2 = 0, test.real2 = 0, test.cat2 = "a", test.bool2 = TRUE, test.real0 = 0,
              test.cat0 = 0, test.bool0 = FALSE, test.real7 = 0, test.cat7 = 0, test.bool7 = FALSE,
              test.int7 = 0, test.int3 = 0)))
  expect_true(isFeasibleNoneMissing(plss, x = list(test.int7 = 0, test.int3 = 0,
              test.int0 = 0, test.int2 = 0, test.real2 = 0, test.cat2 = "a", test.bool2 = TRUE, test.real0 = 0,
              test.cat0 = 0, test.bool0 = FALSE, test.real7 = 0, test.cat7 = 10, test.bool7 = FALSE)))
  expect_false(isFeasible(plss, x = list(test.int7 = 0, test.int3.AMLRFIX1 = 2,
              test.int0 = 0, test.int2 = 0, test.real2 = 0, test.cat2 = "a", test.bool2 = TRUE, test.real0 = 0,
              test.cat0 = 0, test.bool0 = FALSE, test.real7 = 0, test.cat7 = 10, test.bool7 = FALSE)))
  expect_true(isFeasibleNoneMissing(plss, x = list(test.int7 = 1, test.int3.AMLRFIX1 = 2,
              test.int0 = 0, test.int2 = 0, test.real2 = 0, test.cat2 = "a", test.bool2 = TRUE, test.real0 = 0,
              test.cat0 = 0, test.bool0 = FALSE, test.real7 = 0, test.cat7 = 10, test.bool7 = FALSE)))
  expect_false(isFeasible(plss, x = list(test.int7 = 2, test.int3.AMLRFIX1 = 2)))
  expect_true(isFeasibleNoneMissing(plss, x = list(test.int7 = 3,
              test.int0 = 0, test.int2 = 0, test.real2 = 0, test.cat2 = "a", test.bool2 = TRUE, test.real0 = 0,
              test.cat0 = 0, test.bool0 = FALSE, test.real7 = 0, test.cat7 = 10, test.bool7 = FALSE)))
})

# parameters set for the autolearner have the correct value inside the learner
test_that("parameters of learners are treated as expected", {
  # create a simple learner with two real parameters, one dummy and one injected
  dval = c(TRUE, FALSE)
  names(dval) = dval
  innerSS1 = testLearner("test", makeParamSet(makeDiscreteLearnerParam("cat0", dval), makeLogicalLearnerParam("bool0")), c("numerics", "twoclass"))
  issAL1 = autolearner(innerSS1, list(sp("cat0", "bool"),
      sp("bool0", "cat", dval),
      sp("dumm0", "bool", special = 'dummy'),
      sp("inj0", "cat", c('a', 'b'), special = "inject")))
  # bool -> cat works but gives a warning
  expect_warning(lissAL1 <- bl(issAL1), "and has different \\(but feasible\\) type '", all = TRUE)
  expect_learner_output(lissAL1, pid.task, "test")
  expect_learner_output(setHyperPars(lissAL1, test.cat0 = TRUE), pid.task, "test", list(cat0 = TRUE))
  expect_learner_output(setHyperPars(lissAL1, test.bool0 = TRUE), pid.task, "test", list(bool0 = TRUE))
  expect_learner_output(setHyperPars(lissAL1, test.cat0 = FALSE, test.bool0 = TRUE), pid.task, "test", list(cat0 = FALSE, bool0 = TRUE))

  expect_learner_output(setHyperPars(lissAL1, test.dumm0 = TRUE), pid.task, "test")
  expect_learner_output(setHyperPars(lissAL1, test.dumm0 = FALSE, test.bool0 = TRUE), pid.task, "test", list(bool0 = TRUE))

  expect_learner_output(setHyperPars(lissAL1, test.inj0 = 'a'), pid.task, "test", list(inj0 = 'a'))
  expect_learner_output(setHyperPars(lissAL1, test.inj0 = 'b', test.dumm0 = FALSE, test.bool0 = TRUE), pid.task, "test", list(inj0 = 'b', bool0 = TRUE))
})

# learner with many complicated parameters
truename = TRUE
names(truename) = truename
innerSS2 = testLearner("test", makeParamSet(params = predefParams), c("numerics", "twoclass"))
issAL2 = autolearner(innerSS2, list(
    sp("int1", "int", c(0, 3)),
    sp("int2", "def", NULL),  # 'true' default is NULL
    sp("int3", "def", 1),     # wrong default
    sp("int4", "int", c(0, 0), req = quote(int1==0)),
    sp("int5", "int", c(0, 2), req = quote(int1==0), dim = 3),
    sp("int5.AMLRFIX1", "int", c(2, 4), req = quote(int1==1), dim = 3),
    sp("int5.AMLRFIX2", "int", c(5, 5), req = quote(int1 >= 2), dim = 3),
    sp("int5.AMLRFIX3", "int", c(6, 6), req = quote(int1==3), dim = 3),
    sp("int6", "def", 0),
    sp("real1", "real", c(0, 3)),
    sp("real2", "def", NULL),  # 'true' default is NULL
    sp("real3", "def", 1),     # wrong default
    sp("real4", "real", c(0, 0), req = quote(real1==0)),
    sp("real5", "real", c(0, 2), req = quote(real1==0), dim = 3),
    sp("real5.AMLRFIX1", "real", c(2, 4), req = quote(real1==1), dim = 3),
    sp("real5.AMLRFIX2", "real", c(5, 5), req = quote(real1 >= 2), dim = 3),
    sp("real5.AMLRFIX3", "real", c(6, 6), req = quote(real1==3), dim = 3),
    sp("real6", "def", 0),
    sp("cat2", "cat", c("a", "b", "c")),
    sp("cat1", "def", NULL),  # 'true' default is NULL
    sp("cat3", "def", "b"),     # wrong default
    sp("cat4", "cat", "a", req = quote(cat2=="a")),
    sp("cat5", "cat", c("a", "b"), req = quote(cat2=="a"), dim = 3),
    sp("cat5.AMLRFIX1", "cat", "c", req = quote(cat2!="a"), dim = 3),
    sp("cat5.AMLRFIX2", "cat", c("b", "c"), req = quote(cat2=="c"), dim = 3),
    sp("cat6", "def", "a"),
    sp("bool1", "bool"),
    sp("bool2", "def", NULL),  # 'true' default is NULL
    sp("bool3", "def", TRUE),     # wrong default
    sp("bool4", "cat", truename, req = quote(bool1==TRUE)),
    sp("bool5", "bool", req = quote(bool1==TRUE), dim = 3),
    sp("bool5.AMLRFIX1", "cat", truename, req = quote(bool1==FALSE), dim = 3),
    sp("bool6", "bool", req = quote(TRUE==TRUE)),
    sp("bool6.AMLRFIX3", "cat", truename, req = quote(bool1==FALSE))))

# fixed parameters do not show up in the searchspace;
# errors are thrown when AMLRFIX parameters conflict because of requirement overlap
# TODO: what happens when AMLRFIX conflicts with default?
test_that("complicated parameter requirement dependencies are handled well", {
  expect_warning(lissAL2 <- bl(issAL2), "(differs from the true default|and has different \\(but feasible\\) type 'cat)", all = TRUE)

  # many default and fixed parameters are used, only these remain:
  expect_set_equal(getParamIds(getParamSet(lissAL2)),
                   c("test.int1", "test.int5", "test.int5.AMLRFIX1",
                     "test.real1", "test.real5", "test.real5.AMLRFIX1",
                     "test.cat2", "test.cat5", "test.cat5.AMLRFIX2",
                     "test.bool1", "test.bool5", "test.bool6"))


  # requirement of int5 on int1 is respected
  params = list(
      test.int1 = 0, test.int5 = c(1, 1, 1),
      test.real1 = 0, test.real5 = c(1, 1, 1),
      test.cat2 = "a", test.cat5 = list("a", "a", "a"),
      test.bool1 = TRUE, test.bool5 = c(TRUE, TRUE, TRUE), test.bool6 = TRUE)
  expect_true(isFeasibleNoneMissing(getParamSet(lissAL2), params))
  expect_learner_output(setHyperPars(lissAL2, par.vals = params), pid.task, "test",
                        list(int1 = 0, int4 = 0, int5 = c(1, 1, 1), int6 = 0,
                             real1 = 0, real4 = 0, real5 = c(1, 1, 1), real6 = 0,
                             cat2 = "a", cat4 = "a", cat5 = list("a", "a", "a"), cat6 = list("a"),
                             bool1 = TRUE, bool4 = TRUE, bool5 = c(TRUE, TRUE, TRUE), bool6 = TRUE),
                        list(int3 = 1, int4 = 0, real3 = 1, real4 = 0, cat3 = "b", cat4 = "a", bool3 = TRUE, bool4 = TRUE))

  expect_true(isFeasible(getParamSet(lissAL2), list(test.int1 = 0, test.int5 = c(1, 1, 1))))  # check that isFeasible ignores missing parameters
  expect_false(isFeasible(getParamSet(lissAL2), list(test.int1 = 1, test.int5 = c(1, 1, 1))))
  expect_false(isFeasible(getParamSet(lissAL2), list(test.real1 = 1, test.real5 = c(1, 1, 1))))
  expect_false(isFeasible(getParamSet(lissAL2), list(test.cat2 = "b", test.cat5 = list("b", "b", "b"))))

  # requirements of int5 and real5 on int/real1 switch it to AMLRFIX1
  # cat5 disappears because of cat2 (defaults to "c")
  params = list(
      test.int1 = 1, test.int5.AMLRFIX1 = c(4, 4, 4),
      test.real1 = 1, test.real5.AMLRFIX1 = c(4, 4, 4),
      test.cat2 = "b",
      test.bool1 = TRUE, test.bool5 = c(TRUE, TRUE, TRUE), test.bool6 = TRUE)
  expect_true(isFeasibleNoneMissing(getParamSet(lissAL2), params))

  expect_learner_output(setHyperPars(lissAL2, par.vals = params), pid.task, "test",
                        list(int1 = 1, int5 = c(4, 4, 4), int6 = 0,
                             real1 = 1, real5 = c(4, 4, 4), real6 = 0,
                             cat2 = "b", cat5 = list("c", "c", "c"), cat6 = list("a"),
                             bool1 = TRUE, bool4 = TRUE, bool5 = c(TRUE, TRUE, TRUE), bool6 = TRUE),
                        list(int3 = 1, real3 = 1, cat3 = "b", bool3 = TRUE, bool4 = TRUE))

  # int/real1 set to 2 leads to real/int5.AMLRFIX2 being used, which gives a constant
  params = list(
      test.int1 = 2,
      test.real1 = 2,
      test.cat2 = "b",
      test.bool1 = TRUE, test.bool5 = c(TRUE, TRUE, TRUE), test.bool6 = TRUE)
  expect_true(isFeasibleNoneMissing(getParamSet(lissAL2), params))

  expect_learner_output(setHyperPars(lissAL2, par.vals = params), pid.task, "test",
                        list(int1 = 2, int5 = c(5, 5, 5), int6 = 0,
                             real1 = 2, real5 = c(5, 5, 5), real6 = 0,
                             cat2 = "b", cat5 = list("c", "c", "c"), cat6 = list("a"),
                             bool1 = TRUE, bool4 = TRUE, bool5 = c(TRUE, TRUE, TRUE), bool6 = TRUE),
                        list(int3 = 1, real3 = 1, cat3 = "b", bool3 = TRUE, bool4 = TRUE))

  # setting int1 or real1 to 3, cat2 to 'c' or bool1 to FALSE gives a conflict between AMLRFIX2 and AMLRFIX3 and results in a failuremodel
  params = list(
      test.int1 = 3,
      test.real1 = 2,
      test.cat2 = "b",
      test.bool1 = TRUE, test.bool5 = c(TRUE, TRUE, TRUE), test.bool6 = TRUE)


  expect_true(isFeasibleNoneMissing(getParamSet(lissAL2), params))
  expect_warning(t <- train(setHyperPars(lissAL2, par.vals = params), pid.task),
                 "is a static \\(internal\\) parameter but was also given externally|both given although they should be exclusive", all = TRUE)
  expect_class(t, "FailureModel")

  params = list(
      test.int1 = 2,
      test.real1 = 3,
      test.cat2 = "b",
      test.bool1 = TRUE, test.bool5 = c(TRUE, TRUE, TRUE), test.bool6 = TRUE)
  expect_true(isFeasibleNoneMissing(getParamSet(lissAL2), params))
  expect_warning(t <- train(setHyperPars(lissAL2, par.vals = params), pid.task),
                 "is a static \\(internal\\) parameter but was also given externally|both given although they should be exclusive", all = TRUE)
  expect_class(t, "FailureModel")

  params = list(
      test.int1 = 2,
      test.real1 = 2,
      test.cat2 = "c", test.cat5.AMLRFIX2 = list("b", "b", "b"),
      test.bool1 = TRUE, test.bool5 = c(TRUE, TRUE, TRUE), test.bool6 = TRUE)
  expect_true(isFeasibleNoneMissing(getParamSet(lissAL2), params))
  expect_warning(t <- train(setHyperPars(lissAL2, par.vals = params), pid.task),
                 "is a static \\(internal\\) parameter but was also given externally|both given although they should be exclusive", all = TRUE)
  expect_class(t, "FailureModel")

  params = list(
      test.int1 = 2,
      test.real1 = 2,
      test.cat2 = "c", test.cat5.AMLRFIX2 = list("c", "c", "c"),
      test.bool1 = TRUE, test.bool5 = c(TRUE, TRUE, TRUE), test.bool6 = TRUE)
  expect_true(isFeasibleNoneMissing(getParamSet(lissAL2), params))
  expect_warning(t <- train(setHyperPars(lissAL2, par.vals = params), pid.task),
                 "is a static \\(internal\\) parameter but was also given externally|both given although they should be exclusive", all = TRUE)
  expect_class(t, "FailureModel")

  params = list(
      test.int1 = 2,
      test.real1 = 2,
      test.cat2 = "b",
      test.bool1 = FALSE, test.bool6 = TRUE)
  expect_true(isFeasibleNoneMissing(getParamSet(lissAL2), params))
  expect_warning(t <- train(setHyperPars(lissAL2, par.vals = params), pid.task),
                 "is a static \\(internal\\) parameter but was also given externally|both given although they should be exclusive", all = TRUE)
  expect_class(t, "FailureModel")

  params = list(
      test.int1 = 2,
      test.real1 = 2,
      test.cat2 = "b",
      test.bool1 = FALSE)
  #    test.bool6 = TRUE)
  # The situation is: test.bool1 is FALSE, so both bool6 and bool6.AMLRFIX3 have fulfilled requirements.
  # AMLRFIX3 sets bool6 to TRUE, so running this works, even though isFeasibleNoneMissing gives an error here since test.bool6 is missing.
  # Giving test.bool6 as TRUE throws an error since bool6 is then provided twice.
  expect_true(isFeasible(getParamSet(lissAL2), params))  
  expect_learner_output(setHyperPars(lissAL2, par.vals = params), pid.task, "test",
                        list(int1 = 2, int5 = c(5, 5, 5), int6 = 0,
                             real1 = 2, real5 = c(5, 5, 5), real6 = 0,
                             cat2 = "b", cat5 = list("c", "c", "c"), cat6 = list("a"),
                             bool1 = FALSE, bool5 = c(TRUE, TRUE, TRUE), bool6 = TRUE),
                        list(int3 = 1, real3 = 1, cat3 = "b", bool3 = TRUE))
})

# default and fixed parameters are checked for being the actual default and are handed on to the learner
test_that("default parameter values are handled as required", {
  innerSS2HP = setHyperPars(innerSS2, int1 = 0, int2 = 0, int3 = 0, real1 = 0, real2 = 0, real3 = 0, cat1 = "a", cat2 = "a", cat3 = "a", bool1 = FALSE, bool2 = FALSE, bool3 = FALSE)
  # searchspace fully made up from default and fixed values
  issAL3 = autolearner(innerSS2HP, list(
      sp("int2", "def", NULL),  # 'true' default is NULL
      sp("int3", "def", 1),     # wrong default
      sp("real2", "def", NULL),  # 'true' default is NULL
      sp("real3", "def", 1),     # wrong default
      sp("cat1", "def", NULL),  # 'true' default is NULL
      sp("cat3", "def", "b"),     # wrong default
      sp("bool2", "def", NULL),  # 'true' default is NULL
      sp("bool3", "def", TRUE),   # wrong default
      sp("int0", "fix", 1, special = "inject", dim = 2),
      sp("real0", "fix", 1, special = "inject", dim = 2),
      sp("cat0", "fix", "a", special = "inject", dim = 2)))

  expect_warning(lissAL3 <- bl(issAL3), paste0("(that are not mentioned in search space|",
                                               "but the learner has it already set to a different value|",
                                               "differs from the true default|",
                                               "was already set to a value; this value has been removed|",
                                               "is marked 'dummy/inject' and has type 'fix')"), all = TRUE)

  expect_set_equal(getParamIds(getParamSet(lissAL3)), character(0))

  # the wrong defaults are still used and passed on
  expect_learner_output(lissAL3, pid.task, "test",
                        list(int1 = 0, real1 = 0, cat2 = "a", bool1 = FALSE, int0 = c(1, 1), real0 = c(1, 1), cat0 = list("a", "a")),
                        list(int3 = 1, real3 = 1, cat3 = "b", bool3 = TRUE))
})
