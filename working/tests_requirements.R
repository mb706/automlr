
## Test requirements are kept intact and do what they are supposed to do
 ## - [ ] requirements behaviour
 ##   - [ ] parameters with requirements: given requirements are respected, plus 'selected.learner'. (how to test this?) This even works if 'c()' is used an 'c' parameter exists.
 ##   - [ ] requirements consistency
 ##     - [ ] requirements satisfied: generate a random searchspace for a huge setting of wrappers & learners with entanglement of requirements; check internally that these equirements are always satisfied.
 ##     - [ ] requirements satisfied REVERSE: generate the same huge thing; generate variables from the inside and see they are feasible from the outside. THIS MIGHT BE HARD.
 ##   - [ ] requirements simplification
 ##     - [ ] requirements that are always TRUE removed (for wrappers)
 ##     - [ ] parameters with requirements that are always FALSE removed (wrappers and learners)
 ##   - [ ] hidden variables requirements propagation, traversing multiple edges
 ##     - [ ] var reqs can depend on dummy variable values
 ##     - [ ] var reqs can depend on fixed variable values
 ##     - [ ] var reqs can depend on .AMLRFIX fixed var values
 ##     - [ ] var reqs can depend on .AMLRFIX variable var values
 ##   - [ ] circular dependencies cause error


theTask = createTestClassifTask("MissingsNumericsFactorsTask", 200, nNumeric=3, nFactor=3, missings=TRUE)
#
l0 = autolearner(
    testLearner("l0", makeParamSet(predefParams$int1), c("numerics", "twoclass")),
    list(sp("int1", "int", c(0, 10))))
l1 = autolearner(
    testLearner("l1", makeParamSet(predefParams$int1, predefParams$real1, predefParams$bool1, predefParams$cat1),
                c("numerics", "twoclass", "factors", "ordered", "missings")),
    list(sp("int1", "int", c(0, 3), req=quote(automlr.has.missings==FALSE)),
         sp("int1.AMLRFIX1", "int", c(2, 2), req=quote(automlr.has.missings==TRUE && automlr.has.factors == FALSE)),
         sp("int1.AMLRFIX2", "int", c(3, 5), req=quote(automlr.has.missings==TRUE && automlr.has.factors == TRUE)),
         sp("intermediate", "real", c(0, 0), req=quote(int1==2), special="dummy"),
         sp("intermediate2", "bool", req=quote(int1 == 0), special="dummy"),
         sp("intermediate3", "bool", req=quote(int1==2 && intermediate == 0), special="dummy"),
         sp("bool1", "fix", TRUE),
         sp("real1", "real", c(0, 10), req=quote(bool1 == TRUE)),
         sp("real1.AMLRFIX1", "real", c(10, 20), req=quote(bool1 == FALSE)),
         sp("cat1", "cat", c("a", "b"), req=quote(int1==2 && intermediate == 0 && intermediate3)),
         sp("cat1.AMLRFIX1", "cat", "c", req=quote(int1!=2)),
         sp("nevertrue", "bool", special="dummy", req=quote(bool1 == FALSE))))
#
NAFactorRemover1 = autolearner(
    autoWrapper("NAFactorRemover1", function(learner, ...) changeColsWrapper(learner, "NAFactorRemover1", ...),
                function(x) switch(x, missings=c("missings", ""), factors=c("factors", ""))),
    list(sp("NAFactorRemover1.remove.NA", "cat", TRUE, req=quote(automlr.remove.missings == TRUE)),
         sp("NAFactorRemover1.remove.factors", "fix", TRUE, req=quote(automlr.remove.factors == TRUE))),
    "requiredwrapper")
#
NAFactorRemover2 = autolearner(
    autoWrapper("NAFactorRemover2", function(learner, ...) changeColsWrapper(learner, "NAFactorRemover2", ...),
                function(x) switch(x, missings=c("missings", ""), factors=c("factors", ""))),
    list(sp("NAFactorRemover2.remove.NA", "fix", TRUE, req=quote(automlr.remove.missings == TRUE)),
         sp("NAFactorRemover2.intermediate", "int", c(0, 1), req=quote(FALSE == TRUE), special="dummy"),
         sp("NAFactorRemover2.intermediate.AMLRFIX1", "int", c(1, 1), req=quote(automlr.remove.factors == TRUE), special="dummy"),
         sp("NAFactorRemover2.intermediate.AMLRFIX2", "int", c(0, 0), req=quote(automlr.remove.factors == FALSE), special="dummy"),
         sp("NAFactorRemover2.remove.factors", "fix", TRUE, req=quote(NAFactorRemover2.intermediate == 1)),
         sp("NAFactorRemover2.intermediate2", "int", c(0, 3), req=quote(automlr.remove.missings==TRUE), special="dummy"),
         sp("NAFactorRemover2.intermediate2.AMLRFIX1", "int", c(1, 3), req=quote(automlr.remove.missings==FALSE && automlr.remove.factors==FALSE), special="dummy"),
         sp("NAFactorRemover2.intermediate2.AMLRFIX2", "int", c(2, 2), req=quote(automlr.remove.missings==FALSE && automlr.remove.factors==TRUE), special="dummy"),
         sp("NAFactorRemover2.spare1", "int", c(0, 3), req=quote(NAFactorRemover2.intermediate2==1)),
         sp("NAFactorRemover2.spare1.AMLRFIX1", "int", c(4, 4), req=quote(NAFactorRemover2.intermediate2!=1))),
    "requiredwrapper")

l <- buildLearners(list(l0, l1, NAFactorRemover1, NAFactorRemover2), theTask)

which(getParamIds(getParamSet(l)) %nin% c("selected.learner", "automlr.wrappersetup", "automlr.remove.missings", "automlr.wremoving.missings",
                                          "automlr.remove.factors", "automlr.wremoving.factors",
                                          "l0.int1", "l1.int1", "l1.int1.AMLRFIX2", "l1.intermediate2", "l1.intermediate3",
                                          "l1.real1", "l1.cat1",
                                          "NAFactorRemover2.intermediate.AMLRFIX1", "NAFactorRemover2.intermediate.AMLRFIX2",
                                          "NAFactorRemover2.intermediate2", "NAFactorRemover2.intermediate2.AMLRFIX1", "NAFactorRemover2.intermediate2.AMLRFIX2",
                                          "NAFactorRemover2.spare1"))

getParamSet(l)$pars[[8]]$id
getParamSet(l)$pars[[10]]$id
getParamSet(l)$pars[[11]]$id

getParamSet(l)$pars[["l1.real1"]]$requires

debugonce(isFeasible)
#debugonce(automlr:::makeModelMultiplexerParamSetEx)

checkLearnerBehaviour(l, theTask,
                      list(selected.learner="l1", automlr.wrappersetup="NAFactorRemover1$NAFactorRemover2",
                           automlr.remove.missings=TRUE, automlr.remove.factors=TRUE,
                           automlr.wremoving.missings="NAFactorRemover1", automlr.wremoving.factors="NAFactorRemover1",
                           l1.int1=0, # also try others
                           l1.intermediate2=TRUE,
                           l1.real1=5,
                           NAFactorRemover2.intermediate2.AMLRFIX1=1, # also try != 1
                           NAFactorRemover2.spare1=2),
                      "l1", list(int1=0, bool1=TRUE, real1=5, cat1="c"),
                      NAFactorRemover1=list(NAFactorRemover1.spare1=0, NAFactorRemover1.spare2=0,
                          NAFactorRemover1.remove.NA=TRUE, NAFactorRemover1.remove.factors=TRUE),
                      NAFactorRemover2=list(NAFactorRemover2.spare1=2, NAFactorRemover2.spare2=0))

checkLearnerBehaviour(l, theTask,
                      list(selected.learner="l1", automlr.wrappersetup="NAFactorRemover1$NAFactorRemover2",
                           automlr.remove.missings=TRUE, automlr.remove.factors=TRUE,
                           automlr.wremoving.missings="NAFactorRemover2", automlr.wremoving.factors="NAFactorRemover1",
                           l1.int1=2,
                           l1.intermediate3=TRUE,  # also try 'false'
                           l1.real1=5,
                           l1.cat1="b",
                           NAFactorRemover2.intermediate2=2),
                      "l1", list(int1=2, bool1=TRUE, real1=5, cat1="b"),
                      NAFactorRemover1=list(NAFactorRemover1.spare1=0, NAFactorRemover1.spare2=0,
                          NAFactorRemover1.remove.factors=TRUE),
                      NAFactorRemover2=list(NAFactorRemover2.spare1=4L, NAFactorRemover2.spare2=0,
                          NAFactorRemover2.remove.NA=TRUE))

checkLearnerBehaviour(l, theTask,
                      list(selected.learner="l1", automlr.wrappersetup="NAFactorRemover1$NAFactorRemover2",
                           automlr.remove.missings=TRUE, automlr.remove.factors=TRUE,
                           automlr.wremoving.missings="NAFactorRemover1", automlr.wremoving.factors="NAFactorRemover2",
                           l1.int1=2,
                           l1.intermediate3=FALSE,
                           l1.real1=5),
                      "l1", list(int1=2, bool1=TRUE, real1=5),
                      NAFactorRemover1=list(NAFactorRemover1.spare1=0, NAFactorRemover1.spare2=0,
                          NAFactorRemover1.remove.NA=TRUE),
                      NAFactorRemover2=list(NAFactorRemover2.spare1=4L, NAFactorRemover2.spare2=0,
                          NAFactorRemover2.remove.factors=TRUE))

checkLearnerBehaviour(l, theTask,
                      list(selected.learner="l1", automlr.wrappersetup="NAFactorRemover1$NAFactorRemover2",
                           automlr.remove.missings=TRUE, automlr.remove.factors=TRUE,
                           automlr.wremoving.missings="NAFactorRemover2", automlr.wremoving.factors="NAFactorRemover2",
                           l1.int1=3,
                           l1.real1=5,
                           NAFactorRemover2.intermediate2=1,
                           NAFactorRemover2.spare1=2),
                      "l1", list(int1=3, bool1=TRUE, real1=5, cat1="c"),
                      NAFactorRemover1=list(NAFactorRemover1.spare1=0, NAFactorRemover1.spare2=0),
                      NAFactorRemover2=list(NAFactorRemover2.spare1=2, NAFactorRemover2.spare2=0,
                          NAFactorRemover2.remove.NA=TRUE, NAFactorRemover2.remove.factors=TRUE))

checkLearnerBehaviour(l, theTask,
                      list(selected.learner="l1", automlr.wrappersetup="NAFactorRemover1$NAFactorRemover2",
                           automlr.remove.missings=FALSE, automlr.remove.factors=TRUE,
                           automlr.wremoving.factors="NAFactorRemover2",
                           l1.real1=5,
                           l1.intermediate3=FALSE),
                      "l1", list(int1=2, bool1=TRUE, real1=5),
                      NAFactorRemover1=list(NAFactorRemover1.spare1=0, NAFactorRemover1.spare2=0),
                      NAFactorRemover2=list(NAFactorRemover2.spare1=4L, NAFactorRemover2.spare2=0,
                          NAFactorRemover2.remove.factors=TRUE))

checkLearnerBehaviour(l, theTask,
                      list(selected.learner="l1", automlr.wrappersetup="NAFactorRemover1$NAFactorRemover2",
                           automlr.remove.missings=FALSE, automlr.remove.factors=FALSE,
                           l1.int1.AMLRFIX2=3,
                           l1.real1=5,
                           NAFactorRemover2.intermediate2.AMLRFIX1=1,
                           NAFactorRemover2.spare1=2),
                      "l1", list(int1=3, bool1=TRUE, real1=5, cat1="c"),
                      NAFactorRemover1=list(NAFactorRemover1.spare1=0, NAFactorRemover1.spare2=0),
                      NAFactorRemover2=list(NAFactorRemover2.spare1=2, NAFactorRemover2.spare2=0))





