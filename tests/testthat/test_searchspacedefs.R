# Tests that pertain to the hyperparameter space of single learners
context("searchspacedefs")

# warn or stop on parameter space definition with missing information or contradictions
test_that("wrong or unusual searchspace defs give warnings", {
  # warn about learner parameter not mentioned in search space
  emptySS = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")))
  expect_warning(bl(emptySS), "parameters int1 that are not mentioned in search space.", all = TRUE)

  # dummy parameter can not be default
  dummyDef = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("test", "def", 0, special = "dummy")))
  expect_error(bl(dummyDef), "Dummy parameter\\(s\\) 'test' given for learner 'test' must not be of type 'fix', 'def', or 'fixdef'")

  # dummy parameter can not be fixed
  dummyFix = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("test", "fix", 0, special = "dummy")))
  expect_error(bl(dummyFix), "Dummy parameter\\(s\\) 'test' given for learner 'test' must not be of type 'fix', 'def', or 'fixdef'")

  # injected parameter can not be default
  injectDef = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("test", "def", 0, special = "inject")))
  expect_error(bl(injectDef), "Parameter 'test' for learner 'test' marked as 'inject' must not have type 'def'")

  # fixed injected parameter does not make much sense
  injectFix = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("test", "fix", 0, special = "inject")))
  expect_warning(bl(injectFix), "Parameter 'test' for learner 'test' is marked 'inject' and has type 'fix'", all = TRUE)

  # warn about singular param id
  singleId = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), id = "int")))
  expect_warning(bl(singleId), "'int1' of learner 'test' is the only one with parameter id 'int'", all = TRUE)

  # warn about missing requirement
  tl = testLearner("test", makeParamSet(predefParams$int1, predefParams$int4), c("numerics", "twoclass"))
  reqDef = autolearner(tl, list(sp("int4", "int", c(0, 1)), sp("int1", "int", c(0, 1))))
  expect_warning(bl(reqDef), "'int4' for learner 'test' has a 'requires' argument but the one given in the search space has not", all = TRUE)
})

# enforce compatible parameters with same ID
test_that("parameter IDs behave as they should", {
  # having the same ID with correct parameters works
  twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), id = "int")))
  twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), id = "int")))
  expect_class(bl(twoID1, twoID2), "RLearnerClassif")

  # parameter ID same range, dimension enforced
  twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), id = "int")))
  twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 9), id = "int")))
  expect_error(bl(twoID1, twoID2), "their 'upper' property do not match")

  twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$int6), c("numerics", "twoclass")), list(sp("int6", "int", c(0, 10), id = "int", dim = 2)))
  twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$int6), c("numerics", "twoclass")), list(sp("int6", "int", c(0, 10), id = "int", dim = 3)))
  expect_error(bl(twoID1, twoID2), "their 'len' property do not match")

  # parameter ID compatible type enforced
  twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), id = "int")))
  twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$real1), c("numerics", "twoclass")), list(sp("real1", "int", c(0, 10), id = "int")))
  expect_warning(bl(twoID1, twoID2), "'real1' for learner 'test2' is of type 'numeric' and has different \\(but feasible\\) type 'int'", all = TRUE)

  twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), id = "int")))
  twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$real1), c("numerics", "twoclass")), list(sp("real1", "real", c(0, 10), id = "int")))
  expect_error(bl(twoID1, twoID2), "their 'type' property do not match")

  # having the same ID with correct parameters works for categories
  twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$cat1), c("numerics", "twoclass")), list(sp("cat1", "cat", c("a", "b", "c"), id = "cat")))
  twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$cat2), c("numerics", "twoclass")), list(sp("cat2", "cat", c("a", "b", "c"), id = "cat")))
  expect_class(bl(twoID1, twoID2), "RLearnerClassif")

  # parameter ID same range enforced for categories
  twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$cat2), c("numerics", "twoclass")), list(sp("cat2", "cat", c("a", "b", "c"), id = "cat")))
  twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$cat2), c("numerics", "twoclass")), list(sp("cat2", "cat", c("a", "b", "c", "d"), id = "cat")))
  expect_error(bl(twoID1, twoID2), "but their 'values' property do not match")
})

# parameter presence, bounds and types are checked and give proper warnings or errors
test_that("mismatching searchspace defs give errors", {
  # injecting a parameter that is already present fails
  injectOverwrite = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), special = "inject")))
  expect_error(bl(injectOverwrite), "present in learner 'test' but also marked as 'inject'")

  # adding a dummy parameter that is present fails
  dummyOverwrite = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), special = "dummy")))
  expect_error(bl(dummyOverwrite), "present in learner 'test' but also marked as 'dummy'")

  # optimizing over a non-existing parameter fails
  parNotFound = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10))))
  expect_error(bl(parNotFound), "Parameter 'int1' as listed in search space is not available for learner 'test'")

  # using default of a non-existing parameter fails
  parNotFound = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("def1", "def", 10)))
  expect_error(bl(parNotFound), "Parameter 'def1' as listed in search space is not available for learner 'test'")

  # fixing a non-existing parameter fails
  parNotFound = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("fix1", "fix", 10)))
  expect_error(bl(parNotFound), "Parameter 'fix1' as listed in search space is not available for learner 'test'")

  # optimizing beyond parameter bounds fails for ints, reals, categories, booleans-as-categories
  parOOB = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(-1, 10))))
  expect_error(bl(parOOB), "'int1' as listed in search space has infeasible bounds")

  parOOB = autolearner(testLearner("test", makeParamSet(predefParams$real1), c("numerics", "twoclass")), list(sp("real1", "real", c(-1, 10))))
  expect_error(bl(parOOB), "'real1' as listed in search space has infeasible bounds")

  parOOB = autolearner(testLearner("test", makeParamSet(predefParams$cat1), c("numerics", "twoclass")), list(sp("cat1", "cat", c("a", "b", "c", "d"))))
  expect_error(bl(parOOB), "'cat1' as listed in search space has infeasible bounds")

  parOOB = autolearner(testLearner("test", makeParamSet(predefParams$bool1), c("numerics", "twoclass")), list(sp("bool1", "cat", c(0, 1))))
  expect_error(bl(parOOB), "'bool1' as listed in search space has infeasible bounds")

  # using vectorial parameters works, but fails if dimensions mismatch, for integers, reals, categories, and booleans
  parlen = autolearner(testLearner("test", makeParamSet(predefParams$int5), c("numerics", "twoclass")), list(sp("int5", "int", c(0, 10), dim = 3)))
  expect_class(bl(parlen), "RLearnerClassif")
  parlen = autolearner(testLearner("test", makeParamSet(predefParams$int6), c("numerics", "twoclass")), list(sp("int6", "int", c(0, 10), dim = 2)))
  expect_class(bl(parlen), "RLearnerClassif")
  parlen = autolearner(testLearner("test", makeParamSet(predefParams$int5), c("numerics", "twoclass")), list(sp("int5", "int", c(0, 10), dim = 2)))
  expect_error(bl(parlen), "'int5' as listed in search space has infeasible bounds")

  parlen = autolearner(testLearner("test", makeParamSet(predefParams$real5), c("numerics", "twoclass")), list(sp("real5", "real", c(0, 10), dim = 3)))
  expect_class(bl(parlen), "RLearnerClassif")
  parlen = autolearner(testLearner("test", makeParamSet(predefParams$real6), c("numerics", "twoclass")), list(sp("real6", "real", c(0, 10), dim = 2)))
  expect_class(bl(parlen), "RLearnerClassif")
  parlen = autolearner(testLearner("test", makeParamSet(predefParams$real5), c("numerics", "twoclass")), list(sp("real5", "real", c(0, 10), dim = 2)))
  expect_error(bl(parlen), "'real5' as listed in search space has infeasible bounds")

  #TODO: why does this not work?
  #parlen = autolearner(testLearner("test", makeParamSet(predefParams$cat5), c("numerics", "twoclass")), list(sp("cat5", "cat", c("a", "b", "c"), dim = 3)))
  #expect_class(bl(parlen), "RLearnerClassif")
  #parlen = autolearner(testLearner("test", makeParamSet(predefParams$cat6), c("numerics", "twoclass")), list(sp("cat6", "cat", c("a", "b", "c"), dim = 2)))
  #expect_class(bl(parlen), "RLearnerClassif")
  parlen = autolearner(testLearner("test", makeParamSet(predefParams$cat5), c("numerics", "twoclass")), list(sp("cat5", "cat", c("a", "b", "c"), dim = 2)))
  expect_error(bl(parlen), "'cat5' as listed in search space has infeasible bounds")

  parlen = autolearner(testLearner("test", makeParamSet(predefParams$bool5), c("numerics", "twoclass")), list(sp("bool5", "bool", dim = 3)))
  expect_class(bl(parlen), "RLearnerClassif")
  parlen = autolearner(testLearner("test", makeParamSet(predefParams$bool6), c("numerics", "twoclass")), list(sp("bool6", "bool", dim = 2)))
  expect_class(bl(parlen), "RLearnerClassif")
  parlen = autolearner(testLearner("test", makeParamSet(predefParams$bool5), c("numerics", "twoclass")), list(sp("bool5", "bool", dim = 2)))
  expect_error(bl(parlen), "'bool5' as listed in search space has infeasible bounds")

  # using types that enlarge the parameter bounds fails
  parType = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "real", c(0, 10))))
  expect_error(bl(parType), "'int1' as listed in search space has wrong type 'real'")
  parType = autolearner(testLearner("test", makeParamSet(predefParams$int5), c("numerics", "twoclass")), list(sp("int5", "real", c(0, 10), dim = 3)))
  expect_error(bl(parType), "'int5' as listed in search space has wrong type 'real'")
  parType = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "bool")))
  expect_error(bl(parType), "'int1' as listed in search space has infeasible bounds")
  parType = autolearner(testLearner("test", makeParamSet(predefParams$real1), c("numerics", "twoclass")), list(sp("real1", "bool")))
  expect_error(bl(parType), "'real1' as listed in search space has infeasible bounds")
  parType = autolearner(testLearner("test", makeParamSet(makeDiscreteLearnerParam("cat1", c(0, 1, 2))), c("numerics", "twoclass")), list(sp("cat1", "int", c(0, 1))))
  expect_error(bl(parType), "'cat1' as listed in search space has wrong type 'int'")
  # TODO
  # parType = autolearner(testLearner("test", makeParamSet(makeDiscreteVectorLearnerParam("cat5", c(0, 1, 2), len = 3)), c("numerics", "twoclass")), list(sp("cat5", "int", c(0, 1), dim = 3)))
  # bl(parType)
  parType = autolearner(testLearner("test", makeParamSet(makeDiscreteLearnerParam("cat1", c(0, 1, 2))), c("numerics", "twoclass")), list(sp("cat1", "real", c(0, 1))))
  expect_error(bl(parType), "'cat1' as listed in search space has wrong type 'real'")
  # TODO
  #parType = autolearner(testLearner("test", makeParamSet(makeDiscreteVectorLearnerParam("cat5", c(0, 1, 2), len = 3)), c("numerics", "twoclass")), list(sp("cat5", "real", c(0, 1), dim = 3)))
  #bl(parType)

  # using types that are a subset of true parameter bounds gives warnings
  # for bool -> cat, int -> real, cat -> real, cat -> int, cat 0> bool; for scalars and vectors
  tf = c(TRUE, FALSE)
  names(tf) = tf
  parType = autolearner(testLearner("test", makeParamSet(makeDiscreteLearnerParam("cat1", tf)), c("numerics", "twoclass")), list(sp("cat1", "bool")))
  expect_warning(bl(parType), "'discrete' and has different \\(but feasible\\) type 'bool' listed", all = TRUE)
  # TODO
  #parType = autolearner(testLearner("test", makeParamSet(makeDiscreteLearnerParam("cat5", tf)), c("numerics", "twoclass")), list(sp("cat5", "bool", dim = 3)))
  #expect_warning(bl(parType), "'discrete' and has different \\(but feasible\\) type 'bool' listed")

  parType = autolearner(testLearner("test", makeParamSet(predefParams$real1), c("numerics", "twoclass")), list(sp("real1", "int", c(0, 10))))
  expect_warning(bl(parType), "'numeric' and has different \\(but feasible\\) type 'int' listed", all = TRUE)
  parType = autolearner(testLearner("test", makeParamSet(predefParams$real5), c("numerics", "twoclass")), list(sp("real5", "int", c(0, 10), dim = 3)))
  expect_warning(bl(parType), "'numericvector' and has different \\(but feasible\\) type 'int' listed", all = TRUE)

  parType = autolearner(testLearner("test", makeParamSet(predefParams$real1), c("numerics", "twoclass")), list(sp("real1", "cat", c(0, 10))))
  expect_warning(bl(parType), "'numeric' and has different \\(but feasible\\) type 'cat' listed", all = TRUE)
  parType = autolearner(testLearner("test", makeParamSet(predefParams$real5), c("numerics", "twoclass")), list(sp("real5", "cat", c(0, 10), dim = 3)))
  expect_warning(bl(parType), "'numericvector' and has different \\(but feasible\\) type 'cat' listed", all = TRUE)

  parType = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "cat", c(0, 10))))
  expect_warning(bl(parType), "'integer' and has different \\(but feasible\\) type 'cat' listed", all = TRUE)
  parType = autolearner(testLearner("test", makeParamSet(predefParams$int5), c("numerics", "twoclass")), list(sp("int5", "cat", c(0, 10), dim = 3)))
  expect_warning(bl(parType), "'integervector' and has different \\(but feasible\\) type 'cat' listed", all = TRUE)

  parType = autolearner(testLearner("test", makeParamSet(predefParams$bool1), c("numerics", "twoclass")), list(sp("bool1", "cat", c(TRUE, FALSE))))
  expect_warning(bl(parType), "'logical' and has different \\(but feasible\\) type 'cat' listed", all = TRUE)
  parType = autolearner(testLearner("test", makeParamSet(predefParams$bool5), c("numerics", "twoclass")), list(sp("bool5", "cat", c(TRUE, FALSE), dim = 3)))
  expect_warning(bl(parType), "'logicalvector' and has different \\(but feasible\\) type 'cat' listed", all = TRUE)

  # alleging the wrong default gives a warning
  parDef = autolearner(testLearner("test", makeParamSet(predefParams$int3), c("numerics", "twoclass")), list(sp("int3", "def", 1)))
  expect_warning(bl(parDef), "'int3' for learner 'test' is of type 'default' but its alleged default '1' differs from the true default '0'", all = TRUE)
  parDef = autolearner(testLearner("test", makeParamSet(predefParams$real3), c("numerics", "twoclass")), list(sp("real3", "def", 1)))
  expect_warning(bl(parDef), "'real3' for learner 'test' is of type 'default' but its alleged default '1' differs from the true default '0'", all = TRUE)
  parDef = autolearner(testLearner("test", makeParamSet(predefParams$cat3), c("numerics", "twoclass")), list(sp("cat3", "def", "b")))
  expect_warning(bl(parDef), "'cat3' for learner 'test' is of type 'default' but its alleged default 'b' differs from the true default 'a'", all = TRUE)
  parDef = autolearner(testLearner("test", makeParamSet(predefParams$bool3), c("numerics", "twoclass")), list(sp("bool3", "def", TRUE)))
  expect_warning(bl(parDef), "'bool3' for learner 'test' is of type 'default' but its alleged default 'TRUE' differs from the true default 'FALSE'", all = TRUE)
})

# AMLRFIX is a true parameter with requirements, and only occurs next to such parameters.
test_that("AMLRFIX correctness is checked", {
  # AMLRFIX makes it possible to split one learner-parameter into two different optimization-parameters with different conditions.
  # This way it is possible e.g. to have a parameter with different ranges depending on another parameter's value.
  # Therefore, the AMLRFIX (and the parameter it overrides) must have a requirement which is mutually exclusive, and may not be fixed or def
  # (since default values get taken automatically and fixing the parameter usually doesn't make sense here? TODO I notice the reasoning heare
  # is a bit lacking.)
  tl = testLearner("test", makeParamSet(predefParams$int1, predefParams$int3), c("numerics", "twoclass"))

  # AMLRFIX needs requirement
  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "int", c(0, 10), req = quote(int1==0)), sp("int3.AMLRFIX1", "int", c(0, 5))))
  expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' has an \\.AMLRFIX suffix but no requirements")

  # parameters where an AMLRFIX exists need to have a requirement
  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3.AMLRFIX1", "int", c(0, 10), req = quote(int1==0)), sp("int3", "int", c(0, 5))))
  expect_error(bl(aftest), "'int3' for learner 'test' already defined with \\.AMLRFIX suffix cannot be given")

  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "int", c(0, 5)), sp("int3.AMLRFIX1", "int", c(0, 10), req = quote(int1==0))))
  expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' cannot have an \\.AMLRFIX suffix")

  # AMLRFIX additionally to fixed or default parameter doesn't work, in either order
  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "fix", 5), sp("int3.AMLRFIX1", "int", c(0, 10), req = quote(int1==0))))
  expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' cannot have an \\.AMLRFIX suffix")

  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "fix", 5), sp("int3.AMLRFIX1", "int", c(0, 10), req = quote(int1==0))))
  expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' cannot have an \\.AMLRFIX suffix")

  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "def", 0), sp("int3.AMLRFIX1", "int", c(0, 10), req = quote(int1==0))))
  expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' cannot have an \\.AMLRFIX suffix")

  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3.AMLRFIX1", "int", c(0, 10), req = quote(int1==0)), sp("int3", "fix", 5)))
  expect_error(bl(aftest), "'int3' for learner 'test' already defined with \\.AMLRFIX suffix cannot be given")

  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3.AMLRFIX1", "int", c(0, 10), req = quote(int1==0)), sp("int3", "def", 0)))
  expect_error(bl(aftest), "'int3' for learner 'test' already defined with \\.AMLRFIX suffix cannot be given")

  # AMLRFIX cannot be def or fix itself.
  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "int", c(0, 10), req = quote(int1!=0)), sp("int3.AMLRFIX1", "fix", 5)))
  expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' is of type 'fix' but has an \\.AMLRFIX suffix")

  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "int", c(0, 10), req = quote(int1!=0)), sp("int3.AMLRFIX1", "def", 0)))
  expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' is of type 'def' but has an \\.AMLRFIX suffix")

  # AMLRFIX cannot be dummy
  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "int", c(0, 10), req = quote(int1!=0)),
      sp("int3.AMLRFIX1", "int", c(0, 5), req = quote(int1==0), special = 'dummy')))
  expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' has an \\.AMLRFIX suffix but is also a DUMMY")

  # AMLRFIX cannot be defined after dummy and gives an AMLRFIX specific error
  # (this is optional and could be taken out since dummy AMLRFIX doesn't work in any case.)
  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "def", 0),
      sp("int2", "int", c(0, 10), req = quote(int1!=0), special = "dummy"), sp("int2.AMLRFIX1", "int", c(0, 5), req = quote(int1==0))))
  expect_error(bl(aftest), "'int2\\.AMLRFIX1' for learner 'test' cannot have an \\.AMLRFIX suffix")

  # injecting AMLRFIX works
  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "def", 0), sp("int2.AMLRFIX1", "int", c(0, 5), req = quote(int1==0), special = "inject"), sp("int2", "int", c(0, 5), req = quote(int1!=0), special = "inject")))
  expect_class(bl(aftest), "RLearnerClassif")

  # injecting AMLRFIX works in reverse order
  aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "def", 0), sp("int2", "int", c(0, 5), req = quote(int1==0), special = "inject"), sp("int2.AMLRFIX1", "int", c(0, 5), req = quote(int1!=0), special = "inject")))
  expect_class(bl(aftest), "RLearnerClassif")
})

# AMLRFIX with overlapping requirements is checked in test_paramhandling.R

# check if learner has hyperparameter set
test_that("buildLearners checks searchspace def against parameters.", {
  # no complaints if true default is used
  tl = testLearner("test", makeParamSet(predefParams$int3), c("numerics", "twoclass"))
  parDef = autolearner(tl, list(sp("int3", "def", 0)))
  expect_class(bl(parDef), "RLearnerClassif")

  # warning if parameter is already set to a value
  parDef = autolearner(setHyperPars(tl, int3 = 1), list(sp("int3", "def", 0)))
  expect_warning(bl(parDef), "learner has it already set to a different value", all = TRUE) # TODO: give a message which value is being used.

  # warning if parameter is set to a value, even if it matches with the alleged default
  parDef = autolearner(setHyperPars(tl, int3 = 1), list(sp("int3", "def", 1)))
  expect_warning(bl(parDef), "(learner has it already set to a different value|but its alleged default '1' differs from the true default '0')", all = TRUE)
})
