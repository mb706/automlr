


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

resRand <- automlr(pid.task, budget=c(evals=3), backend="random", verbosity=3, max.learner.time=30,
                   searchspace=mlrLightweight)
debugger()

as.data.frame(amfinish(resRand)$opt.path)

resRand <- automlr(pid.task, budget=c(evals=100), backend="random", verbosity=5, max.learner.time=3,
                   searchspace=list(mlrLearners$classif.logreg, mlrLearners$classif.nodeHarvest))

configureMlr(on.error.dump=TRUE)

resRand <- automlr(pid.task, budget=c(evals=2000), backend="irace", verbosity=3,
                   searchspace=list(mlrLearners$classif.ctree, mlrLearners$classif.rknn))

nontarget <- c("mmce.test.mean", "dob", "eol", "error.message", "exec.time")
opp0 <- as.data.frame(amfinish(resRand)$opt.path)
opp <- opp0
opp[, nontarget] <- NULL
xdesc <- apply(opp, 1, collapse)
xlist <- list()
for (x in seq_along(xdesc)) {
  xlist[[xdesc[x]]] <- c(xlist[[xdesc[x]]], x)
}
xmeans <- sapply(xlist, function(ind) {
  mean(opp0[ind, "mmce.test.mean"])
})
sort(xmeans)[1:5]
collapse(colnames(opp))

thel <- automlr:::buildLearners(searchspace=list(mlrLearners$classif.ctree, mlrLearners$classif.rknn), pid.task, 3)

opx0 <- amfinish(resRand)$opt.path

truePerf <- sapply(xlist, function(idxs) {
  replicate(200, {gc() ; resample(setHyperPars(thel, par.vals=removeMissingValues(getOptPathEl(opx0, idxs[1])$x)), pid.task, hout)$aggr})
})


Best configuration:         110    mean value:    0.2619707661
Description of the best configuration:
    .ID. selected.learner classif.ctree.teststat classif.ctree.testtype classif.ctree.maxsurrogate classif.ctree.limitmtry classif.ctree.minbucket
110  110    classif.ctree                    max             Bonferroni                          3                    TRUE                       9
    classif.ctree.minsplit classif.ctree.stump classif.rknn.k classif.rknn.r classif.rknn.mtry classif.ctree.mincriterion
110                     16               FALSE             NA             NA                NA                  0.6899244
    classif.ctree.mincriterion.AMLRFIX1 classif.ctree.mtry .PARENT.
110                                  NA                 17       92


# 2017-02-19 15:04:39 CET: Elite configurations:
    selected.learner classif.ctree.teststat classif.ctree.testtype classif.ctree.maxsurrogate classif.ctree.limitmtry classif.ctree.minbucket
110    classif.ctree                    max             Bonferroni                          3                    TRUE                       9
114    classif.ctree                    max             Bonferroni                          3                    TRUE                       9
120    classif.ctree                    max             Bonferroni                          3                    TRUE                       9
137    classif.ctree                    max             Bonferroni                          3                    TRUE                       9
143    classif.ctree                    max             Bonferroni                          4                    TRUE                       9
    classif.ctree.minsplit classif.ctree.stump classif.rknn.k classif.rknn.r classif.rknn.mtry classif.ctree.mincriterion
110                     16               FALSE             NA             NA                NA                  0.6899244
114                     17               FALSE             NA             NA                NA                  0.6781195
120                     16               FALSE             NA             NA                NA                  0.6783891
137                     17               FALSE             NA             NA                NA                  0.6837511
143                     17               FALSE             NA             NA                NA                  0.6842876
    classif.ctree.mincriterion.AMLRFIX1 classif.ctree.mtry
110                                  NA                 17
114                                  NA                 18
120                                  NA                 19
137                                  NA                 19
143                                  NA                 18


getParamSet(makeLearner("classif.ctree"))

resample(makeLearner("classif.ctree", teststat="max", testtype="MonteCarlo"), pid.task, cv10)



debugonce(automlr:::amoptimize.amirace)

amfinish(resRand)
as.data.frame(amfinish(resRand)$opt.path)

debugonce(automlr:::trainLearner.TimeconstraintWrapper)

getParamSet(makeLearner("classif.lssvm"))

mlr:::trainLearner.classif.lssvm

## Test automlr

library('testthat')
MYVAR = 2
devtools::test(pkg="..", filter="aux")
rm(MYVAR)



##

argsToList = function() {
  defaults = formals(sys.function(-1))
  defaults$`...` = NULL
  assigns = as.list(eval.parent(quote(match.call())))
  assigns[[1]] = NULL
  insert(defaults, assigns)
}

fun <- function(x = 1, y = 2, ...) {
  argsToList()
}

f2 = function() {
  fun(y=100)
}

f2()
do.call(f2, list())

fun(y=100)
do.call(fun, list())
do.call(fun, list(y=100))



formals(fx)

##

automlr::runWithTimeout(Sys.sleep(10), 1)

automlr::runWithTimeout(mean(rnorm(100000000)), 2)

automlr::runWithTimeout(
    automlr::runWithTimeout(
        {stop("fayul") 
    resample(learner=makeLearner("classif.nodeHarvest"), task=pid.task, resampling=cv10)},
    10), 15)

resample(learner=automlr:::makeTimeconstraintWrapper(
             setHyperPars(makeLearner("classif.nodeHarvest"), nodesize=9, nodes=510, maxinter=3, mode="outbag", biascorr=FALSE)
           , 3), task=pid.task, resampling=cv5)

runWithTimeout(train(makeLearner("classif.nodeHarvest"), task=pid.task), 20000)

system.time(R.utils::evalWithTimeout(train(makeLearner("classif.nodeHarvest"), task=pid.task), timeout=10))

undebug(runWithTimeout)


##
bigMeanTO <- function(doTO) runWithTimeout(mean(rnorm(10 + doTO * 1e7)), 1, backend="native")


dorun <- function(doTO) {
  suspendInterruptsFor({
    result = lapply(1:10, function(i) {
      print(i)
      print('noabort')
      print(bigMeanTO(doTO)$result)
      print(handleInterrupts({ print('allowabort') ; bigMeanTO(doTO)$result}, "aborted"))
    })
  }, hardKillInterval = 0.5)
  result
}

devtools::load_all("..")

dorun(1)


(x <- 1) + 1



xf <- function() {
  on.exit(print(i))
  i = 1
  j = 2
  on.exit()
}

xf()
lapply(1:40, function(i) handleInterrupts({ print(i) ; bigMeanTO(1)$result}, "aborted"))


bigMeanTO(0)$result

lapply(1:4, function(i) {
  print("run ***************")
  print(i)
  job = parallel::mcparallel(mean(rnorm(10 + doTO * 1e7)), mc.set.seed=FALSE, silent=FALSE)
  print("JOB*******************")
  print(job)
  print(system.time(result <- parallel::mccollect(job, wait=FALSE, timeout=2), gcFirst=FALSE))
  print("RS1*******************")  
  print(result)
  print(system.time(result2 <- parallel::mccollect(job, wait=FALSE), gcFirst=FALSE))
  print("R23*******************")  
  print(result2)
  print(system.time(result3 <- parallel::mccollect(job, wait=TRUE), gcFirst=FALSE))
  print(result3)
  i
})

library(parallel)
print(.Internal(interruptsSuspended(TRUE)))
job <- parallel::mcparallel(mean(rnorm(10 + 1 * 1e7)), mc.set.seed=FALSE, silent=FALSE)
parallel::mccollect(job, wait=FALSE, timeout=2)

mccollect()
print(system.time(Sys.sleep(1), gcFirst=FALSE))

i = 3
while(i>0) i <- i - 1
i


lrn = makeLearner("classif.rknn")
ctrl = makeTuneControlRandom(maxit=3)
# create long list of learner params
psLength = 200
longLearnerParams = do.call(c, lapply(seq_len(psLength), function(x) {
  makeParamSet(makeIntegerLearnerParam(paste0('some.parameter', x), 1, 10))
}))
lrn$par.set = c(lrn$par.set, longLearnerParams)
tuneParams(lrn, pid.task, cv5, par.set = longLearnerParams, control = ctrl, show.info=TRUE)


##############

debugger(getOptPathEl(amfinish(res)$opt.path, 1)$extra$.dump[[1]]$train)

fx <- function() {
  tryCatch({
    withCallingHandlers({
      tryCatch({
        withCallingHandlers({
          setTimeLimit(elapsed=1)
          interruptibleSleep(10)
        }, error = function(cond) {
          print('inner wch')
          signalCondition(makeS3Obj(c("automlr.timeout", "condition"),
                                    msg = "automlr timeout", call = conditionCall(cond)))
        })
        print('reached inner')
      }, automlr.timeout = function(e) { print('inner') })
    }, error = function(e) {
      print('outer wch')
      signalCondition(makeS3Obj(c("automlr.timeout", "condition"),
                                msg = "automlr timeout", call = conditionCall(cond)))
      
    })
    print('reached outer')
  }, automlr.timeout = function(e) { print('outer') })
  setTimeLimit()
}

fx()

# bad boy:
## selected.learner: classif.plsdaCaret; 
## classif.plsdaCaret.ncomp: 2; 
## classif.plsdaCaret.probMethod: softmax; 
## ppa.nzv.cutoff.numeric: 0.0713111583055064; 
## ppa.univariate.trafo: center; 
## ppa.multivariate.trafo: ica; 
## ppa.feature.filter: off; 


bl <- buildLearners(list(mlrLearners$classif.plsdaCaret, mlrLearners$ampreproc), pid.task, verbosity=6)

resample(setHyperPars(bl, classif.plsdaCaret.ncomp=2, classif.plsdaCaret.probMethod="softmax", ppa.nzv.cutoff.numeric=0.0713111583055064,
                      ppa.univariate.trafo="off", ppa.multivariate.trafo="ica", ppa.feature.filter="off"),
                      pid.task, cv5, show.info=TRUE)



mlr:::trainLearner.classif.plsdaCaret

debugonce(pls:::oscorespls.fit)

debugonce(caret:::plsda.default)

train
library(fastICA)
dm <- prcomp(matrix(runif(6000), ncol=6))$x

dm <- fastICA(matrix(runif(6000), ncol=6), 6, method="C")$S
mkt <- makeClassifTask("orthotask", data.frame(t=sample(factor(c('a', 'b')), 1000, replace=TRUE), dm), target='t')
train(makeLearner('classif.plsdaCaret'), mkt)

resample(makeLearner('classif.plsdaCaret', ncomp=2, probMethod="softmax"), mkt, cv5)

y <- sample(factor(c('a', 'b')), 1000, replace=TRUE)
configureMlr(on.par.without.desc="warn")



matplot(cov(getTaskData(mkt)[, 2:7]))

getTaskData(mkt)

pid.task

library(pls)
y <- cbind(sample(0:1, 1000, replace=TRUE), sample(0:1, 1000, replace=TRUE))


library(fastICA)
dm <- fastICA(matrix(runif(6000), ncol=6), 6, method="C")$S
y <- matrix(rnorm(2000), ncol=2)
mvr(y ~ dm, method="oscorespls")

summary(mvr(y ~ dm, method="kernelpls"))

pls.options()


getParamSet(makeLearner('classif.plsdaCaret'))

caret:::plsda.default
