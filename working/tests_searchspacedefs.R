

### Testing search space parameter behaviour
emptySS = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")))
expect_warning(bl(emptySS), "parameters int1 that are not mentioned in search space.")
#
dummyDef = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("test", "def", 0, special="dummy")))
expect_error(bl(dummyDef), "Dummy parameter 'test' given for learner 'test' must not be of type 'def'")
#
dummyFix = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("test", "fix", 0, special="dummy")))
expect_error(bl(dummyFix), "Dummy parameter 'test' given for learner 'test' must not be of type 'fix'")
#
injectDef = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("test", "def", 0, special="inject")))
expect_error(bl(injectDef), "Parameter 'test' for learner 'test' marked as 'inject' must not have type 'def'")
#
injectFix = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("test", "fix", 0, special="inject")))
expect_warning(bl(injectFix), "Parameter 'test' for learner 'test' is marked 'inject' and has type 'fix'")

singleId = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), id="int")))
expect_warning(bl(singleId), "'int1' of learner 'test' is the only one with parameter id 'int'")
#
twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), id="int")))
twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), id="int")))
expect_class(bl(twoID1, twoID2), "RLearnerClassif")
#
twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), id="int")))
twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 9), id="int")))
expect_error(bl(twoID1, twoID2), "their 'upper' property do not match")
#
twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$int6), c("numerics", "twoclass")), list(sp("int6", "int", c(0, 10), id="int", dim=2)))
twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$int6), c("numerics", "twoclass")), list(sp("int6", "int", c(0, 10), id="int", dim=3)))
expect_error(bl(twoID1, twoID2), "their 'len' property do not match")
#
twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), id="int")))
twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$real1), c("numerics", "twoclass")), list(sp("real1", "int", c(0, 10), id="int")))
expect_warning(bl(twoID1, twoID2), "'real1' for learner 'test2' is of type 'numeric' and has different \\(but feasible\\) type 'int'")
#
twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), id="int")))
twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$real1), c("numerics", "twoclass")), list(sp("real1", "real", c(0, 10), id="int")))
expect_error(bl(twoID1, twoID2), "their 'type' property do not match")
#
twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$cat1), c("numerics", "twoclass")), list(sp("cat1", "cat", c("a", "b", "c"), id="cat")))
twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$cat2), c("numerics", "twoclass")), list(sp("cat2", "cat", c("a", "b", "c"), id="cat")))
expect_class(bl(twoID1, twoID2), "RLearnerClassif")
#
twoID1 = autolearner(testLearner("test1", makeParamSet(predefParams$cat2), c("numerics", "twoclass")), list(sp("cat2", "cat", c("a", "b", "c"), id="cat")))
twoID2 = autolearner(testLearner("test2", makeParamSet(predefParams$cat2), c("numerics", "twoclass")), list(sp("cat2", "cat", c("a", "b", "c", "d"), id="cat")))
expect_error(bl(twoID1, twoID2), "but their 'values' property do not match")

injectOverwrite = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), special="inject")))
expect_error(bl(injectOverwrite), "present in learner 'test' but is marked as 'inject'")
#
dummyOverwrite = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10), special="dummy")))
expect_error(bl(dummyOverwrite), "present in learner 'test' but is marked as 'dummy'")

parNotFound = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("int1", "int", c(0, 10))))
expect_error(bl(parNotFound), "Parameter 'int1' as listed in search space is not available for learner 'test'")
#
parNotFound = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("def1", "def", 10)))
expect_error(bl(parNotFound), "Parameter 'def1' as listed in search space is not available for learner 'test'")
#
parNotFound = autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass")), list(sp("fix1", "fix", 10)))
expect_error(bl(parNotFound), "Parameter 'fix1' as listed in search space is not available for learner 'test'")

parOOB = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "int", c(-1, 10))))
expect_error(bl(parOOB), "'int1' as listed in search space has infeasible bounds")
#
parOOB = autolearner(testLearner("test", makeParamSet(predefParams$real1), c("numerics", "twoclass")), list(sp("real1", "real", c(-1, 10))))
expect_error(bl(parOOB), "'real1' as listed in search space has infeasible bounds")
#
parOOB = autolearner(testLearner("test", makeParamSet(predefParams$cat1), c("numerics", "twoclass")), list(sp("cat1", "cat", c("a", "b", "c", "d"))))
expect_error(bl(parOOB), "'cat1' as listed in search space has infeasible bounds")
#
parOOB = autolearner(testLearner("test", makeParamSet(predefParams$bool1), c("numerics", "twoclass")), list(sp("bool1", "cat", c(0, 1))))
expect_error(bl(parOOB), "'bool1' as listed in search space has infeasible bounds")

parlen = autolearner(testLearner("test", makeParamSet(predefParams$int5), c("numerics", "twoclass")), list(sp("int5", "int", c(0, 10), dim=3)))
expect_class(bl(parlen), "RLearnerClassif")
parlen = autolearner(testLearner("test", makeParamSet(predefParams$int6), c("numerics", "twoclass")), list(sp("int6", "int", c(0, 10), dim=2)))
expect_class(bl(parlen), "RLearnerClassif")
parlen = autolearner(testLearner("test", makeParamSet(predefParams$int5), c("numerics", "twoclass")), list(sp("int5", "int", c(0, 10), dim=2)))
expect_error(bl(parlen), "'int5' as listed in search space has infeasible bounds")
#
parlen = autolearner(testLearner("test", makeParamSet(predefParams$real5), c("numerics", "twoclass")), list(sp("real5", "real", c(0, 10), dim=3)))
expect_class(bl(parlen), "RLearnerClassif")
parlen = autolearner(testLearner("test", makeParamSet(predefParams$real6), c("numerics", "twoclass")), list(sp("real6", "real", c(0, 10), dim=2)))
expect_class(bl(parlen), "RLearnerClassif")
parlen = autolearner(testLearner("test", makeParamSet(predefParams$real5), c("numerics", "twoclass")), list(sp("real5", "real", c(0, 10), dim=2)))
expect_error(bl(parlen), "'real5' as listed in search space has infeasible bounds")
#
#TODO: why does this not work?
#parlen = autolearner(testLearner("test", makeParamSet(predefParams$cat5), c("numerics", "twoclass")), list(sp("cat5", "cat", c("a", "b", "c"), dim=3)))
#expect_class(bl(parlen), "RLearnerClassif")
#parlen = autolearner(testLearner("test", makeParamSet(predefParams$cat6), c("numerics", "twoclass")), list(sp("cat6", "cat", c("a", "b", "c"), dim=2)))
#expect_class(bl(parlen), "RLearnerClassif")
parlen = autolearner(testLearner("test", makeParamSet(predefParams$cat5), c("numerics", "twoclass")), list(sp("cat5", "cat", c("a", "b", "c"), dim=2)))
expect_error(bl(parlen), "'cat5' as listed in search space has infeasible bounds")
#
parlen = autolearner(testLearner("test", makeParamSet(predefParams$bool5), c("numerics", "twoclass")), list(sp("bool5", "bool", dim=3)))
expect_class(bl(parlen), "RLearnerClassif")
parlen = autolearner(testLearner("test", makeParamSet(predefParams$bool6), c("numerics", "twoclass")), list(sp("bool6", "bool", dim=2)))
expect_class(bl(parlen), "RLearnerClassif")
parlen = autolearner(testLearner("test", makeParamSet(predefParams$bool5), c("numerics", "twoclass")), list(sp("bool5", "bool", dim=2)))
expect_error(bl(parlen), "'bool5' as listed in search space has infeasible bounds")

parType = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "real", c(0, 10))))
expect_error(bl(parType), "'int1' as listed in search space has wrong type 'real'")
parType = autolearner(testLearner("test", makeParamSet(predefParams$int5), c("numerics", "twoclass")), list(sp("int5", "real", c(0, 10), dim=3)))
expect_error(bl(parType), "'int5' as listed in search space has wrong type 'real'")
parType = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "bool")))
expect_error(bl(parType), "'int1' as listed in search space has infeasible bounds")
parType = autolearner(testLearner("test", makeParamSet(predefParams$real1), c("numerics", "twoclass")), list(sp("real1", "bool")))
expect_error(bl(parType), "'real1' as listed in search space has infeasible bounds")
parType = autolearner(testLearner("test", makeParamSet(makeDiscreteLearnerParam("cat1", c(0, 1, 2))), c("numerics", "twoclass")), list(sp("cat1", "int", c(0, 1))))
expect_error(bl(parType), "'cat1' as listed in search space has wrong type 'int'")
# TODO
# parType = autolearner(testLearner("test", makeParamSet(makeDiscreteVectorLearnerParam("cat5", c(0, 1, 2), len=3)), c("numerics", "twoclass")), list(sp("cat5", "int", c(0, 1), dim=3)))
# bl(parType)
parType = autolearner(testLearner("test", makeParamSet(makeDiscreteLearnerParam("cat1", c(0, 1, 2))), c("numerics", "twoclass")), list(sp("cat1", "real", c(0, 1))))
expect_error(bl(parType), "'cat1' as listed in search space has wrong type 'real'")
# TODO
#parType = autolearner(testLearner("test", makeParamSet(makeDiscreteVectorLearnerParam("cat5", c(0, 1, 2), len=3)), c("numerics", "twoclass")), list(sp("cat5", "real", c(0, 1), dim=3)))
#bl(parType)



tf = c(TRUE, FALSE)
names(tf) = tf
parType = autolearner(testLearner("test", makeParamSet(makeDiscreteLearnerParam("cat1", tf)), c("numerics", "twoclass")), list(sp("cat1", "bool")))
expect_warning(bl(parType), "'discrete' and has different \\(but feasible\\) type 'bool' listed")
# TODO
#parType = autolearner(testLearner("test", makeParamSet(makeDiscreteLearnerParam("cat5", tf)), c("numerics", "twoclass")), list(sp("cat5", "bool", dim=3)))
#expect_warning(bl(parType), "'discrete' and has different \\(but feasible\\) type 'bool' listed")
#
parType = autolearner(testLearner("test", makeParamSet(predefParams$real1), c("numerics", "twoclass")), list(sp("real1", "int", c(0, 10))))
expect_warning(bl(parType), "'numeric' and has different \\(but feasible\\) type 'int' listed")
parType = autolearner(testLearner("test", makeParamSet(predefParams$real5), c("numerics", "twoclass")), list(sp("real5", "int", c(0, 10), dim=3)))
expect_warning(bl(parType), "'numericvector' and has different \\(but feasible\\) type 'int' listed")
#
parType = autolearner(testLearner("test", makeParamSet(predefParams$real1), c("numerics", "twoclass")), list(sp("real1", "cat", c(0, 10))))
expect_warning(bl(parType), "'numeric' and has different \\(but feasible\\) type 'cat' listed")
parType = autolearner(testLearner("test", makeParamSet(predefParams$real5), c("numerics", "twoclass")), list(sp("real5", "cat", c(0, 10), dim=3)))
expect_warning(bl(parType), "'numericvector' and has different \\(but feasible\\) type 'cat' listed")
#
parType = autolearner(testLearner("test", makeParamSet(predefParams$int1), c("numerics", "twoclass")), list(sp("int1", "cat", c(0, 10))))
expect_warning(bl(parType), "'integer' and has different \\(but feasible\\) type 'cat' listed")
parType = autolearner(testLearner("test", makeParamSet(predefParams$int5), c("numerics", "twoclass")), list(sp("int5", "cat", c(0, 10), dim=3)))
expect_warning(bl(parType), "'integervector' and has different \\(but feasible\\) type 'cat' listed")
#
parType = autolearner(testLearner("test", makeParamSet(predefParams$bool1), c("numerics", "twoclass")), list(sp("bool1", "cat", c(TRUE, FALSE))))
expect_warning(bl(parType), "'logical' and has different \\(but feasible\\) type 'cat' listed")
parType = autolearner(testLearner("test", makeParamSet(predefParams$bool5), c("numerics", "twoclass")), list(sp("bool5", "cat", c(TRUE, FALSE), dim=3)))
expect_warning(bl(parType), "'logicalvector' and has different \\(but feasible\\) type 'cat' listed")

parDef = autolearner(testLearner("test", makeParamSet(predefParams$int3), c("numerics", "twoclass")), list(sp("int3", "def", 1)))
expect_warning(bl(parDef), "'int3' for learner 'test' is of type 'default' but its alleged default '1' differs from the true default '0'")
parDef = autolearner(testLearner("test", makeParamSet(predefParams$real3), c("numerics", "twoclass")), list(sp("real3", "def", 1)))
expect_warning(bl(parDef), "'real3' for learner 'test' is of type 'default' but its alleged default '1' differs from the true default '0'")
parDef = autolearner(testLearner("test", makeParamSet(predefParams$cat3), c("numerics", "twoclass")), list(sp("cat3", "def", "b")))
expect_warning(bl(parDef), "'cat3' for learner 'test' is of type 'default' but its alleged default 'b' differs from the true default 'a'")
parDef = autolearner(testLearner("test", makeParamSet(predefParams$bool3), c("numerics", "twoclass")), list(sp("bool3", "def", TRUE)))
expect_warning(bl(parDef), "'bool3' for learner 'test' is of type 'default' but its alleged default 'TRUE' differs from the true default 'FALSE'")

tl = testLearner("test", makeParamSet(predefParams$int1, predefParams$int3), c("numerics", "twoclass"))
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "int", c(0, 10), req=quote(int1==0)), sp("int3.AMLRFIX1", "int", c(0, 5))))
expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' has an \\.AMLRFIX suffix but no requirements")
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3.AMLRFIX1", "int", c(0, 10), req=quote(int1==0)), sp("int3", "int", c(0, 5))))
expect_error(bl(aftest), "'int3' for learner 'test' already defined with \\.AMLRFIX suffix cannot be given")
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "int", c(0, 5)), sp("int3.AMLRFIX1", "int", c(0, 10), req=quote(int1==0))))
expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' cannot have an \\.AMLRFIX suffix")
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "fix", 5, req=quote(int1!=0)), sp("int3.AMLRFIX1", "int", c(0, 10), req=quote(int1==0))))
expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' cannot have an \\.AMLRFIX suffix")
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "fix", 5, req=quote(int1!=0)), sp("int3.AMLRFIX1", "int", c(0, 10), req=quote(int1==0))))
expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' cannot have an \\.AMLRFIX suffix")
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "def", 0, req=quote(int1!=0)), sp("int3.AMLRFIX1", "int", c(0, 10), req=quote(int1==0))))
expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' cannot have an \\.AMLRFIX suffix")
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3.AMLRFIX1", "int", c(0, 10), req=quote(int1==0)), sp("int3", "fix", 5), req=quote(int1!=0)))
expect_error(bl(aftest), "'int3' for learner 'test' already defined with \\.AMLRFIX suffix cannot be given")
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3.AMLRFIX1", "int", c(0, 10), req=quote(int1==0)), sp("int3", "def", 0), req=quote(int1!=0)))
expect_error(bl(aftest), "'int3' for learner 'test' already defined with \\.AMLRFIX suffix cannot be given")
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "int", c(0, 10), req=quote(int1!=0)), sp("int3.AMLRFIX1", "fix", 5, req=quote(int1==0))))
expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' is of type 'fix' but has an \\.AMLRFIX suffix")
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "int", c(0, 10), req=quote(int1!=0)), sp("int3.AMLRFIX1", "def", 0, req=quote(int1==0))))
expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' is of type 'def' but has an \\.AMLRFIX suffix")
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "int", c(0, 10), req=quote(int1!=0)), sp("int3.AMLRFIX1", "int", c(0, 5), req=quote(int1==0), special='dummy')))
expect_error(bl(aftest), "'int3\\.AMLRFIX1' for learner 'test' has an \\.AMLRFIX suffix but is also a DUMMY")
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "def", 0), sp("int2", "int", c(0, 10), req=quote(int1!=0), special="dummy"), sp("int2.AMLRFIX1", "int", c(0, 5), req=quote(int1==0))))
expect_error(bl(aftest), "'int2\\.AMLRFIX1' for learner 'test' cannot have an \\.AMLRFIX suffix")
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "def", 0), sp("int2.AMLRFIX1", "int", c(0, 5), req=quote(int1==0), special="inject"), sp("int2", "int", c(0, 5), req=quote(int1!=0))))
expect_class(bl(aftest), "RLearnerClassif")
aftest = autolearner(tl, list(sp("int1", "int", c(0, 10)), sp("int3", "def", 0), sp("int2", "int", c(0, 5), req=quote(int1==0), special="inject"), sp("int2.AMLRFIX1", "int", c(0, 5), req=quote(int1!=0), special="inject")))
expect_class(bl(aftest), "RLearnerClassif")

tl = testLearner("test", makeParamSet(predefParams$int3), c("numerics", "twoclass"))
parDef = autolearner(tl, list(sp("int3", "def", 0)))
expect_class(bl(parDef), "RLearnerClassif")
parDef = autolearner(setHyperPars(tl, int3=1), list(sp("int3", "def", 0)))
expect_warning(bl(parDef), "learner has it already set to '1'")
parDef = autolearner(setHyperPars(tl, int3=1), list(sp("int3", "def", 1)))
expect_warning(bl(parDef), "learner has it already set to '1'")

tl = testLearner("test", makeParamSet(predefParams$int1, predefParams$int4), c("numerics", "twoclass"))
reqDef = autolearner(tl, list(sp("int4", "int", c(0, 1)), sp("int1", "int", c(0, 1))))
expect_warning(bl(reqDef), "'int4' for learner 'test' has a 'requires' argument but the one given in the search space has not")
