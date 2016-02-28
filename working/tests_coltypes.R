

## Test column types influence behaivour of wrappers and learners in the correct way
 ## - [ ] has.X, removes.X
 ##   - [ ] has.X:
 ##     - [ ] parameters in learner can depend on 'has.X'. X may be missings, factors, ordered but not numerics. this is so for
 ##       - [ ] .AMLRFIX
 ##         - [ ] parameters with .AMLRFIX, fixed: value takes on this value in presence/absence of X
 ##         - [ ] parameters with .AMLRFIX, variable: two external vars, each only valid some times, set the goal variable differently inside the learner.
 ##       - [ ] normal variables
 ##         - [ ] removed (or requirement removed) if it trivially depends on has.X / removes.X and requirement satisfied n / y
 ##     - [ ] how variable
 ##       - [ ] is fixed to NO if the task doesn't have X to begin with
 ##       - [ ] is fixed to YES if the task has it an nothing can convert
 ##       - [ ] there is an external var determining this if X is in the task and a wrapper can remove it
 ##       - [ ] there is another var only available if the above is TRUE, determining which wrapper removes it, if more than one wrapper are present
 ##       - [ ] removes.X is set for the given wrapper. this has influence on .AMLRFIX for fixed and variable learners and changes the external search space accordingly, while setting internally the variable accordingly
 ##       - [ ] has.X is true for all the wrappers before and including the one removing it, is false afterwards; .AMLRFIX etc. behaves accordingly

NumericsTask = createTestClassifTask("NumericsTask", 200, nNumeric=3)
FactorsTask = createTestClassifTask("FactorsTask", 200, nFactor=3)
OrderedTask = createTestClassifTask("OrderedTask", 200, nOrdered=3)
NumericsFactorsTask = createTestClassifTask("NumericsFactorsTask", 200, nNumeric=3, nFactor=3)
NumericsOrderedTask = createTestClassifTask("NumericsOrderedTask", 200, nNumeric=3, nOrdered=3)
MissingsNumericsTask = createTestClassifTask("MissingsNumericsTask", 200, nNumeric=3, missings=TRUE)
MissingsFactorsTask = createTestClassifTask("MissingsFactorsTask", 200, nFactor=3, missings=TRUE)
MissingsOrderedTask = createTestClassifTask("MissingsOrderedTask", 200, nOrdered=3, missings=TRUE)
MissingsNumericsFactorsTask = createTestClassifTask("MissingsNumericsFactorsTask", 200, nNumeric=3, nFactor=3, missings=TRUE)
MissingsNumericsOrderedTask = createTestClassifTask("MissingsNumericsOrderedTask", 200, nNumeric=3, nOrdered=3, missings=TRUE)
#
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
    list(sp("int1", "int", c(0, 10), req=quote(automlr.has.missings==TRUE))))
MissingsFactorsLearner = autolearner(
    testLearner("MissingsFactorsLearner", makeParamSet(predefParams$int1), c("factors", "twoclass", "missings")),
    list())
MissingsFactorsNumericsLearner = autolearner(
    testLearner("MissingsFactorsNumericsLearner", makeParamSet(predefParams$int1), c("numerics", "twoclass", "missings", "factors")),
    list(sp("int1", "int", c(0, 10), req=quote(automlr.has.missings==TRUE)),
         sp("real1", "real", c(10, 10), req=quote(automlr.has.factors==TRUE)),
         sp("bool1", "bool", req=quote(automlr.has.missings != automlr.has.factors))))
FactorsNumericsLearner = autolearner(
    testLearner("FactorsNumericsLearner", makeParamSet(predefParams$int1), c("numerics", "twoclass", "factors")),
    list(sp("real1", "real", c(0, 10), req=quote(automlr.has.factors==FALSE)),
         sp("bool1", "fix", FALSE, req=quote(automlr.has.missings == automlr.has.factors)),
         sp("bool1.AMLRFIX1", "fix", TRUE, req=quote(automlr.has.missings != automlr.has.factors))))
AllLearner = autolearner(
    testLearner("AllLearner", makeParamSet(predefParams$int1), c("numerics", "twoclass", "factors", "ordered", "missings")),
    list(sp("int1", "int", c(0, 10), req=quote(automlr.has.missings==FALSE)),
         sp("real1", "real", c(0, 10), req=quote(automlr.has.factors %in% c(TRUE, FALSE))),
         sp("bool1", "fix", TRUE, req=quote(automlr.has.ordered == automlr.has.factors))))

XRemover = autolearner(
    autoWrapper("XRemover", function(learner, ...) changeColsWrapper(learner, "XRemover", ...), identity),
    list(sp("XRemover.spare1", "int", c(0, 10)),
         sp("XRemover.spare2", "fix", 9, req=quote(automlr.has.missings==TRUE))),
    "requiredwrapper")
#
NARemover = autolearner(
    autoWrapper("NARemover", function(learner, ...) changeColsWrapper(learner, "NARemover", ...), function(x) switch(x, missings=c("missings", ""))),
    list(sp("NARemover.remove.NA", "fix", TRUE, req=quote(automlr.remove.missings == TRUE))),
    "requiredwrapper")
#
FactorRemover = autolearner(
    autoWrapper("FactorRemover", function(learner, ...) changeColsWrapper(learner, "FactorRemover", ...), function(x) switch(x, factors=c("factors", ""))),
    list(sp("FactorRemover.remove.factors", "fix", FALSE, req=quote(automlr.remove.factors == FALSE)),
         sp("FactorRemover.convertFactors", "bool", special="dummy", req=quote(automlr.remove.factors == TRUE && automlr.has.numerics == TRUE)),
         sp("FactorRemover.convert.fac2num", "fix", TRUE, req=quote(automlr.remove.factors && automlr.has.numerics && FactorRemover.convertFactors)),
         sp("FactorRemover.remove.factors.AMLRFIX2", "fix", FALSE, req=quote(automlr.remove.factors && (!automlr.has.numerics || !FactorRemover.convertFactors)))),
    "requiredwrapper")
#
NAFactorRemover = autolearner(
    autoWrapper("NAFactorRemover", function(learner, ...) changeColsWrapper(learner, "NAFactorRemover", ...),
                function(x) switch(x, missings=c("missings", ""), factors=c("factors", ""))),
    list(sp("NAFactorRemover.remove.NA", "fix", TRUE, req=quote(automlr.remove.missings == TRUE)),
         sp("NAFactorRemover.remove.factors", "fix", TRUE, req=quote(automlr.remove.factors == TRUE))),
    "requiredwrapper")

getpars = function(learner) getParamSet(learner)$pars

checkLearnerBehaviour = function(learner, task, params, ...) {
  expect_true(isFeasibleNoneMissing(getParamSet(learner), params))
#  train(setHyperPars(learner, par.vals=params), task)
  expect_learner_output(setHyperPars(learner, par.vals=params), task, ...)
}

l = buildLearners(list(NumericsLearner, FactorsLearner, OrderedsLearner), NumericsTask)
checkLearnerBehaviour(l, NumericsTask, list(NumericsLearner.int1=1), "NumericsLearner", list(int1=1), list())

l = buildLearners(list(NumericsLearner, MissingsNumericsLearner), NumericsTask)
checkLearnerBehaviour(l, NumericsTask, list(selected.learner="MissingsNumericsLearner"),
                      "MissingsNumericsLearner", list(), list())

l = buildLearners(list(NumericsLearner, MissingsNumericsLearner), MissingsNumericsTask)
checkLearnerBehaviour(l, NumericsTask, list(MissingsNumericsLearner.int1=2),
                      "MissingsNumericsLearner", list(int1=2), list())

l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, XRemover, NARemover, FactorRemover), NumericsTask)
checkLearnerBehaviour(l, NumericsTask,
                      list(selected.learner="NumericsLearner", NumericsLearner.int1=3, automlr.wrappersetup="FactorRemover$XRemover$NARemover",
                           XRemover.spare1=3),
                      "NumericsLearner", list(int1=3), list(),
                      FactorRemover=list(FactorRemover.spare1=0, FactorRemover.spare2=0),
                      XRemover=list(XRemover.spare1=3, XRemover.spare2=0),
                      NARemover=list(NARemover.spare1=0, NARemover.spare2=0))

l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, XRemover, NARemover, FactorRemover), MissingsNumericsTask)
checkLearnerBehaviour(l, NumericsTask,
                      list(selected.learner="NumericsLearner", NumericsLearner.int1=3, automlr.wrappersetup="XRemover$FactorRemover$NARemover",
                           XRemover.spare1=3),
                      "NumericsLearner", list(int1=3), list(),
                      XRemover=list(XRemover.spare1=3, XRemover.spare2=9),                      
                      FactorRemover=list(FactorRemover.spare1=0, FactorRemover.spare2=0),
                      NARemover=list(NARemover.spare1=0, NARemover.spare2=0, NARemover.remove.NA=TRUE))

l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, XRemover, NARemover, FactorRemover), MissingsNumericsTask)
checkLearnerBehaviour(l, NumericsTask,
                      list(selected.learner="NumericsLearner", NumericsLearner.int1=3, automlr.wrappersetup="NARemover$XRemover$FactorRemover",
                           XRemover.spare1=3),
                      "NumericsLearner", list(int1=3), list(),
                      NARemover=list(NARemover.spare1=0, NARemover.spare2=0, NARemover.remove.NA=TRUE),                      
                      XRemover=list(XRemover.spare1=3, XRemover.spare2=0),                      
                      FactorRemover=list(FactorRemover.spare1=0, FactorRemover.spare2=0))

l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, XRemover, NARemover, FactorRemover), MissingsNumericsTask)
checkLearnerBehaviour(l, NumericsTask,
                      list(selected.learner="MissingsNumericsLearner", automlr.wrappersetup="NARemover$XRemover$FactorRemover",
                           XRemover.spare1=3, automlr.remove.missings=TRUE),
                      "MissingsNumericsLearner", list(), list(),
                      NARemover=list(NARemover.spare1=0, NARemover.spare2=0, NARemover.remove.NA=TRUE),                      
                      XRemover=list(XRemover.spare1=3, XRemover.spare2=0),                      
                      FactorRemover=list(FactorRemover.spare1=0, FactorRemover.spare2=0))

l = buildLearners(list(NumericsLearner, MissingsNumericsLearner, XRemover, NARemover, FactorRemover), MissingsNumericsTask)
checkLearnerBehaviour(l, NumericsTask,
                      list(selected.learner="MissingsNumericsLearner", automlr.wrappersetup="NARemover$XRemover$FactorRemover",
                           XRemover.spare1=3, automlr.remove.missings=FALSE, MissingsNumericsLearner.int1=9),
                      "MissingsNumericsLearner", list(int1=9), list(),
                      NARemover=list(NARemover.spare1=0, NARemover.spare2=0),                      
                      XRemover=list(XRemover.spare1=3, XRemover.spare2=9),                      
                      FactorRemover=list(FactorRemover.spare1=0, FactorRemover.spare2=0))







