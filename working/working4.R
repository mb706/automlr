


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


resRand <- automlr(pid.task, budget=c(evals=100), backend="random", verbosity=5, max.learner.time=3,
                   searchspace=list(mlrLearners$classif.logreg, mlrLearners$classif.nodeHarvest))

configureMlr(on.error.dump=TRUE)

resRand <- automlr(pid.task, budget=c(evals=100), backend="irace", verbosity=3,
                   searchspace=list(mlrLearners$classif.ctree, mlrLearners$classif.sda))

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
