
outerSS = testLearner("test", makeParamSet(params=predefParams[c(
    "int1", "int2", "int3", "int4", "real1", "real2", "real3", "real4",
    "cat1", "cat2", "cat3", "cat4", "bool1", "bool2", "bool3")]), c("numerics", "twoclass"))
ssAL = autolearner(outerSS, list(
    sp("int0", "int", c(0, 10), special="inject"),
    sp("int7", "int", c(0, 10), special="dummy"),
    sp("int3", "int", c(0, 1), req=quote(int7==0)),
    sp("int3.AMLRFIX1", "int", c(2, 3), req=quote(int7==1)),
    sp("int3.AMLRFIX2", "int", c(4, 4), req=quote(int7==2)),
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
    sp("int8", "fix", 0L, special="inject"),
    sp("real0", "real", c(0, 10), special="inject"),
    sp("cat0", "cat", c(0, 10), special="inject"),
    sp("bool0", "bool", special="inject"),
    sp("real7", "real", c(0, 10), special="dummy"),
    sp("cat7", "cat", c(0, 10), special="dummy"),
    sp("bool7", "bool", special="dummy"),
    sp("cat8", "fix", "a", special="inject"),
    sp("real8", "fix", 0.5, special="inject"),
    sp("int4", "int", c(0, 0)),
    sp("real4", "real", c(0.5, 0.5)),
    sp("cat4", "cat", "a")))

expect_warning(lss <- bl(ssAL), "is marked 'inject' and has type 'fix'")
expect_set_equal(getParamIds(getParamSet(lss)),
                 paste0("test.", c("int0", "int7", "int3", "int3.AMLRFIX1", "int2", "real2", "cat2", "bool2", "real0", "cat0", "bool0", "real7", "cat7", "bool7")))

plss = getParamSet(lss)
expect_true(isFeasible(plss, x=list(test.int0=0, test.int2=0, test.real2=0, test.cat2="a", test.bool2=TRUE, test.real0=0, test.cat0=0,
                     test.bool0=FALSE, test.real7=0, test.cat7=0, test.bool7=FALSE, test.int7=0, test.int3=0)))
expect_true(isFeasible(plss, x=list(test.int7=0, test.int3=0)))
expect_false(isFeasible(plss, x=list(test.int7=0, test.int3.AMLRFIX1=2)))
expect_true(isFeasible(plss, x=list(test.int7=1, test.int3.AMLRFIX1=2)))
expect_false(isFeasible(plss, x=list(test.int7=2, test.int3.AMLRFIX1=2)))
expect_true(isFeasible(plss, x=list(test.int7=3)))

dval = c(TRUE, FALSE)
names(dval) = dval
innerSS1 = testLearner("test", makeParamSet(makeDiscreteLearnerParam("cat0", dval), makeLogicalLearnerParam("bool0")), c("numerics", "twoclass"))
issAL1 = autolearner(innerSS1, list(sp("cat0", "bool"), sp("bool0", "cat", dval), sp("dumm0", "bool", special='dummy'), sp("inj0", "cat", c('a', 'b'), special="inject")))
expect_warning(lissAL1 <- bl(issAL1), "and has different \\(but feasible\\) type '")
expect_learner_output(lissAL1, pid.task, "test")
expect_learner_output(setHyperPars(lissAL1, test.cat0=TRUE), pid.task, "test", list(cat0=TRUE))
expect_learner_output(setHyperPars(lissAL1, test.bool0=TRUE), pid.task, "test", list(bool0=TRUE))
expect_learner_output(setHyperPars(lissAL1, test.cat0=FALSE, test.bool0=TRUE), pid.task, "test", list(cat0=FALSE, bool0=TRUE))

expect_learner_output(setHyperPars(lissAL1, test.dumm0=TRUE), pid.task, "test")
expect_learner_output(setHyperPars(lissAL1, test.dumm0=FALSE, test.bool0=TRUE), pid.task, "test", list(bool0=TRUE))

expect_learner_output(setHyperPars(lissAL1, test.inj0='a'), pid.task, "test", list(inj0='a'))
expect_learner_output(setHyperPars(lissAL1, test.inj0='b', test.dumm0=FALSE, test.bool0=TRUE), pid.task, "test", list(inj0='b', bool0=TRUE))

truename = TRUE
names(truename) = truename
innerSS2 = testLearner("test", makeParamSet(params=predefParams), c("numerics", "twoclass"))
issAL2 = autolearner(innerSS2, list(
    sp("int1", "int", c(0, 3)),
    sp("int2", "def", NULL),  # 'true' default is NULL
    sp("int3", "def", 1),     # wrong default
    sp("int4", "int", c(0, 0), req=quote(int1==0)),
    sp("int5", "int", c(0, 2), req=quote(int1==0), dim=3),
    sp("int5.AMLRFIX1", "int", c(2, 4), req=quote(int1==1), dim=3),
    sp("int5.AMLRFIX2", "int", c(5, 5), req=quote(int1>=2), dim=3),
    sp("int5.AMLRFIX3", "int", c(6, 6), req=quote(int1==3), dim=3),
    sp("int6", "def", 0),
    sp("real1", "real", c(0, 3)),
    sp("real2", "def", NULL),  # 'true' default is NULL
    sp("real3", "def", 1),     # wrong default
    sp("real4", "real", c(0, 0), req=quote(real1==0)),
    sp("real5", "real", c(0, 2), req=quote(real1==0), dim=3),
    sp("real5.AMLRFIX1", "real", c(2, 4), req=quote(real1==1), dim=3),
    sp("real5.AMLRFIX2", "real", c(5, 5), req=quote(real1>=2), dim=3),
    sp("real5.AMLRFIX3", "real", c(6, 6), req=quote(real1==3), dim=3),
    sp("real6", "def", 0),
    sp("cat2", "cat", c("a", "b", "c")),
    sp("cat1", "def", NULL),  # 'true' default is NULL
    sp("cat3", "def", "b"),     # wrong default
    sp("cat4", "cat", c("a", "b"), req=quote(cat1=="a")),
    sp("cat5", "cat", c("a", "b"), req=quote(cat2=="a"), dim=3),
    sp("cat5.AMLRFIX1", "cat", "c", req=quote(cat2!="a"), dim=3),
    sp("cat5.AMLRFIX2", "cat", c("b", "c"), req=quote(cat2=="c"), dim=3),
    sp("cat6", "def", "a"),
    sp("bool1", "bool"),
    sp("bool2", "def", NULL),  # 'true' default is NULL
    sp("bool3", "def", TRUE),     # wrong default
    sp("bool4", "cat", truename, req=quote(bool1==TRUE)),
    sp("bool5", "bool", req=quote(bool1==TRUE), dim=3),
    sp("bool5.AMLRFIX1", "cat", truename, req=quote(bool1==FALSE), dim=3),
    sp("bool6", "bool", req=quote(TRUE==TRUE)),
    sp("bool6.AMLRFIX3", "cat", truename, req=quote(bool1==FALSE))))

expect_warning(lissAL2 <- bl(issAL2), "(differs from the true default|and has different \\(but feasible\\) type 'cat)", all=TRUE)

params = list(test.int1=0, test.int5=c(1, 1, 1), test.real1=0, test.real5=c(1, 1, 1), test.cat2="a", test.cat5=list("a", "a", "a"), test.bool1=TRUE, test.bool5=c(TRUE, TRUE, TRUE), test.bool6=TRUE)
isFeasible(getParamSet(lissAL2), params)
setHyperPars(lissAL2, par.vals=params)

# TODO
