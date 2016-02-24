

### Testing trivial learners are returned when appropriate
taskNormal = createTestClassifTask("t1", 20, nNumeric=1)
taskWWeights = createTestClassifTask("t1", 20, nNumeric=1, weights=rpois(20, 1))
trivialLearner1 = list(autolearner(testLearner("test", makeParamSet(), c("numerics", "twoclass"))))
trivialLearner2 = list(autolearner(testLearner("test", makeParamSet(), c("factors", "twoclass"))))

expect_warning(lrns <- buildLearners(list(), pid.task), 'No model fits the given task')
expect_null(lrns)

expect_class(buildLearners(trivialLearner1, taskNormal), "RLearnerClassif")
expect_error(buildLearners(c(trivialLearner1, trivialLearner1), taskNormal), "Base learners must all have unique ids")
expect_warning(res <- buildLearners(trivialLearner2, taskNormal), 'No model fits the given task')
expect_null(res)
expect_error(buildLearners(trivialLearner1, taskWWeights), "Tasks with weights are currently not supported")
expect_error(buildLearners(trivialLearner2, taskWWeights), "Tasks with weights are currently not supported")
###


### Testing the right kind of learners are filtered out
t.n2 = createTestClassifTask("t.n2", 200, nNumeric=1)
t.f2 = createTestClassifTask("t.f2", 200, nFactor=1)
t.o2 = createTestClassifTask("t.o2", 200, nOrdered=1)
t.nx = createTestClassifTask("t.nx", 200, nNumeric=1, nClasses=25)
t.fx = createTestClassifTask("t.fx", 200, nFactor=1, nClasses=25)
t.ox = createTestClassifTask("t.ox", 200, nOrdered=1, nClasses=25)
t.nm2 = createTestClassifTask("t.nm2", 200, nNumeric=1, missings=TRUE)
t.fm2 = createTestClassifTask("t.fm2", 200, nFactor=1, missings=TRUE)
t.om2 = createTestClassifTask("t.om2", 200, nOrdered=1, missings=TRUE)
t.nmx = createTestClassifTask("t.nmx", 200, nNumeric=1, nClasses=3, missings=TRUE)
t.fmx = createTestClassifTask("t.fmx", 200, nFactor=1, nClasses=3, missings=TRUE)


checkWrapperEffect = function(transformation=list, ...) {
  checkLearnersPresent(t.n2, transformation(c("numerics", "twoclass")), ...)
  checkLearnersPresent(t.f2, transformation(c("factors", "twoclass")), ...)
  checkLearnersPresent(t.o2, transformation(c("ordered", "twoclass")), ...)
  checkLearnersPresent(t.nx, transformation(c("numerics", "multiclass")), ...)
  checkLearnersPresent(t.fx, transformation(c("factors", "multiclass")), ...)
  checkLearnersPresent(t.ox, transformation(c("ordered", "multiclass")), ...)
  checkLearnersPresent(t.nm2, transformation(c("numerics", "twoclass", "missings")), ...)
  checkLearnersPresent(t.fm2, transformation(c("factors", "twoclass", "missings")), ...)
  checkLearnersPresent(t.om2, transformation(c("ordered", "twoclass", "missings")), ...)
  checkLearnersPresent(t.nmx, transformation(c("numerics", "multiclass", "missings")), ...)
  checkLearnersPresent(t.fmx, transformation(c("factors", "multiclass", "missings")), ...)
}

MRemover = autoWrapper("M.R", identity, function(x) switch(x, missings=c("missings", ""), x))
FRemover = autoWrapper("F.R", identity, function(x) switch(x, factors=c("factors", ""), x))
ORemover = autoWrapper("O.R", identity, function(x) switch(x, ordered=c("ordered", ""), x))
MFRemover = autoWrapper("MF.R", identity, function(x) switch(x, missings=c("missings", ""), factors=c("factors", ""), x))
OConverter = autoWrapper("O.C", identity, function(x) switch(x, ordered=c("ordered", "numerics"), x))
FConverter = autoWrapper("F.C", identity, function(x) switch(x, factors=c("factors", "numerics"), x))
MFRemoverFConverter = autoWrapper("MF.R.F.C", identity, function(x) switch(x, factors=c("factors", "numerics", ""), missings=c("missings", ""), x))

autolearnersPL = autolearnersBASIC
checkWrapperEffect()
checkWrapperEffectEx()

for (uneffectiveWrapper in list(MRemover, FRemover, ORemover, MFRemover, OConverter, FConverter, MFRemoverFConverter)) {
  autolearnersPL = c(autolearnersBASIC, list(autolearner(uneffectiveWrapper, stacktype="wrapper")))
  checkWrapperEffect()
  checkWrapperEffectEx()
}

autolearnersPL = c(autolearnersBASIC, list(autolearner(MRemover, stacktype="requiredwrapper")))
checkWrapperEffect(function(x) list(setdiff(x, "missings")))
checkWrapperEffectEx(function(x) list(setdiff(x, "missings")))

autolearnersPL = c(autolearnersBASIC, list(autolearner(FRemover, stacktype="requiredwrapper")))
checkWrapperEffect(function(x) list(x, setdiff(x, "factors")))
checkWrapperEffectEx(function(x) list(x, setdiff(x, "factors")))

autolearnersPL = c(autolearnersBASIC, list(autolearner(ORemover, stacktype="requiredwrapper")))
checkWrapperEffect(function(x) list(x, setdiff(x, "ordered")))
checkWrapperEffectEx(function(x) list(x, setdiff(x, "ordered")))

autolearnersPL = c(autolearnersBASIC, list(autolearner(MFRemover, stacktype="requiredwrapper")))
checkWrapperEffect(function(x) lapply(list(c("factors", "missings"), "missings"), setdiff, x=x))
checkWrapperEffectEx(function(x) lapply(list(c("factors", "missings"), "missings"), setdiff, x=x))

autolearnersPL = c(autolearnersBASIC, list(autolearner(MRemover, stacktype="requiredwrapper"), autolearner(FRemover, stacktype="requiredwrapper")))
checkWrapperEffect(function(x) lapply(list(c("factors", "missings"), "missings"), setdiff, x=x))
checkWrapperEffectEx(function(x) lapply(list(c("factors", "missings"), "missings"), setdiff, x=x))

autolearnersPL = c(autolearnersBASIC, list(autolearner(MRemover, stacktype="requiredwrapper"), autolearner(FRemover, stacktype="wrapper")))
checkWrapperEffect(function(x) list(setdiff(x, "missings")))
checkWrapperEffectEx(function(x) list(setdiff(x, "missings")))

autolearnersPL = c(autolearnersBASIC, list(autolearner(MFRemover, stacktype="requiredwrapper"), autolearner(ORemover, stacktype="requiredwrapper")))
checkWrapperEffect(function(x) lapply(list("ordered", "factors", c("ordered", "factors")), function(y) setdiff(x, c("missings", y))))
checkWrapperEffectEx(function(x) lapply(list("ordered", "factors", c("ordered", "factors")), function(y) setdiff(x, c("missings", y))))

autolearnersPL = c(autolearnersBASIC, list(autolearner(MFRemoverFConverter, stacktype="requiredwrapper")))
checkWrapperEffect(function(x) lapply(list(x, setdiff(x, "factors"), sub("factors", "numerics", x)), setdiff, y="missings"))
checkWrapperEffectEx(function(x) lapply(list(x, setdiff(x, "factors"), sub("factors", "numerics", x)), setdiff, y="missings"))

autolearnersPL = c(autolearnersBASIC, list(autolearner(FConverter, stacktype="requiredwrapper"), autolearner(MFRemover, stacktype="requiredwrapper")))
checkWrapperEffect(function(x) lapply(list(x, setdiff(x, "factors"), sub("factors", "numerics", x)), setdiff, y="missings"))
checkWrapperEffectEx(function(x) lapply(list(x, setdiff(x, "factors"), sub("factors", "numerics", x)), setdiff, y="missings"))

# TODO: it is natural that the following fails, but it would be nice if it worked.
#autolearnersPL = c(autolearnersBASIC, list(autolearner(OConverter, stacktype="requiredwrapper")))
#checkWrapperEffect(function(x) list(x, sub("ordered", "numerics", x)))
#checkWrapperEffectEx(function(x) list(x, sub("ordered", "numerics", x)))
#
#autolearnersPL = c(autolearnersBASIC, list(autolearner(FConverter, stacktype="requiredwrapper")))
#checkWrapperEffect(function(x) list(x, sub("factors", "numerics", x)))
#checkWrapperEffectEx(function(x) list(x, sub("factors", "numerics", x)))
#
#autolearnersPL = c(autolearnersBASIC, list(
#    autolearner(FConverter, stacktype="requiredwrapper"),
#    autolearner(MFRemover, stacktype="requiredwrapper"),
#    autolearner(ORemover, stacktype="requiredwrapper")))
#chgfun = function(x) {
#  urlist = list(x, setdiff(x, "factors"), sub("factors", "numerics", x))
#  c(lapply(urlist, setdiff, y="missings"), lapply(urlist, setdiff, y=c("missings", "ordered")))
#}
# TODO: the following should work
#checkWrapperEffect(chgfun)
#checkWrapperEffectEx(chgfun)
###
