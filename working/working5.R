


#install.packages("Rmpi", configure.args = c("--with-Rmpi-include=/usr/include/openmpi-x86_64/", "--with-Rmpi-libpath=/usr/lib64/openmpi/lib/",
#                   "--with-Rmpi-type=OPENMPI"))

# how to trigger dll error:
library("ParamHelpers")
unloadNamespace("ParamHelpers")
devtools::load_all("../../ParamHelpers")
library("lhs")
lhs::maximinLHS(n=56, k=14)



options(width=150)
unloadNamespace("automlr")
unloadNamespace("mlrMBO")
unloadNamespace("smoof")
unloadNamespace("mlr")
unloadNamespace("ParamHelpers")
devtools::load_all("../../ParamHelpers")
devtools::load_all("../../mlr")
devtools::load_all("../../smoof")
devtools::load_all("../../mlrMBO")
library('testthat')
#
library(roxygen2)
roxygenise('..')

devtools::load_all("..")
options(error=dump.frames)


options(warn=1)

##
pid.task
##

## TODO: try to just set currentBudget to a large number

debugonce(amoptimize.ammbo)

resRand <- automlr(pid.task, budget=c(evals=300), backend="mbo", verbosity=3,
                   searchspace=list(mlrLearners$classif.ctree, mlrLearners$classif.rknn))

debugger()
q

plot(as.data.frame(amfinish(resRand)$opt.path)$y)


resRand <- automlr(pid.task, budget=c(evals=1), backend="random", verbosity=3,
                   max.walltime.overrun=10, max.learner.time=10,
                   searchspace=list(mlrLearners$classif.ctree, mlrLearners$classif.rknn))


configureMlr(on.error.dump = TRUE)

configureMlr(show.info=TRUE, on.error.dump = TRUE)
resRand <- automlr(pid.task, budget=c(walltime=20), backend="random", verbosity=3,
                   max.walltime.overrun=10, max.learner.time=10,
                   searchspace=list(mlrLearners$classif.ctree, mlrLearners$classif.rknn, mlrLearners$ampreproc))

names(mlrLearners)

as.data.frame(amfinish(resRand)$opt.path)

debugger(getOptPathEl(amfinish(resRand)$opt.path, 1)$extra$.dump[[1]][[1]])

automlr:::runWithTimeout

as.data.frame(amfinish(resRand)$opt.path)


tl <- makeRLearnerClassif('testlearner', par.set=makeParamSet(makeIntegerLearnerParam('testparam')), package=character(0))
tl$fix.factors.prediction=TRUE

trainLearner.testlearner = function(.learner, .task, .subset, .weights = NULL, testparam, ...) {
  print(colnames(getTaskData(.task)))
  testparam
}

predictLearner.testlearner = function(.learner, .model, .newdata, ...) {
  print(.model$learner.model)
  factor(rep(.model$task.desc$class.levels[1], nrow(.newdata)),
         levels=.model$task.desc$class.levels)
}

configureMlr(show.info=TRUE, on.error.dump = TRUE)

data = getTaskData(pid.task)
data[1, 1] = NA
pid.task2 = makeClassifTask(data=data, target=getTaskTargetNames(pid.task))


m <- train(setHyperPars(tl, testparam=2), pid.task)
predict(m, newdata=getTaskData(pid.task2))

tlw <- makePreprocWrapperAm(tl, ppa.nzv.cutoff.numeric=10, ppa.impute.numeric="remove.na", ppa.multivariate.trafo="ica")
mw <- train(setHyperPars(tlw, testparam=2), pid.task2)
predict(mw, newdata=getTaskData(pid.task2))


tlw2 <- makeFailImputationWrapper(tl)
mw2 <- train(setHyperPars(tlw2), pid.task2)
predict(mw2, newdata=getTaskData(pid.task2))


debugger(m$dump)
sapply(getTaskData(pid.task), var)


automlr:::trainLearner.PreprocWrapperAm()
debugonce(predictLearner.PreprocWrapperAm)

debugonce(trainLearner.PreprocWrapperAm)



##
configureMlr(show.info=TRUE, on.error.dump = TRUE)
resRand <- automlr(pid.task, budget=c(evals=3), backend="mbo", verbosity=3,
                   searchspace=mlrLightweight)

debug(makePreprocWrapperAm)


adf <- as.data.frame(amfinish(resRand)$opt.path)

names(adf)
adf[, c(1, 298:304)]

resRand2 <- automlr(resRand, budget=c(evals=1190), verbosity=5)

resRand$spent




resRand3 <- automlr(pid.task, budget=c(evals=3), backend="mbo", verbosity=3,
                   searchspace=mlrLightweight)



resRand3 <- automlr(pid.task, budget=c(evals=3), backend="mbo", verbosity=3,
                   searchspace=list(mlrLearners$classif.ctree, mlrLearners$classif.rknn))

debugonce(mlrMBO:::evalMBODesign.OptState)


getNativeSymbolInfo("lhs")

getNativeSymbolInfo("maximinLHS_C")

str(getCallingDLLe(getNamespace("lhs")))

getCallingDLLe(getNamespace("lhs"))$info

getCallingDLLe(getNamespace("lhs"))[[5]]


install.packages("lazyeval")

library("lhs")
lhs::maximinLHS(n=56, k=14)



debugonce(lhs::maximinLHS)
