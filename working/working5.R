


#install.packages("Rmpi", configure.args = c("--with-Rmpi-include=/usr/include/openmpi-x86_64/", "--with-Rmpi-libpath=/usr/lib64/openmpi/lib/",
#                   "--with-Rmpi-type=OPENMPI"))


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

resRand <- automlr(pid.task, budget=c(walltime=20), backend="random", verbosity=3,
                   max.walltime.overrun=10, max.learner.time=10,
                   searchspace=list(mlrLearners$classif.ctree, mlrLearners$classif.rknn))

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

m <- train(setHyperPars(tl, testparam=2), pid.task)
predict(m, newdata=getTaskData(pid.task))

tlw <- makePreprocWrapperAm(tl, ppa.nzv.cutoff.numeric=1)
m <- train(setHyperPars(tlw, testparam=2), pid.task)
predict(m, newdata=getTaskData(pid.task))

debugger(m$dump)
sapply(getTaskData(pid.task), var)


automlr:::trainLearner.PreprocWrapperAm()
