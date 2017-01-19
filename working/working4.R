


#install.packages("Rmpi", configure.args = c("--with-Rmpi-include=/usr/include/openmpi-x86_64/", "--with-Rmpi-libpath=/usr/lib64/openmpi/lib/",
#                   "--with-Rmpi-type=OPENMPI"))


options(width=150)
devtools::load_all("../../ParamHelpers")
devtools::load_all("../../mlr")
devtools::load_all("../../smoof")
devtools::load_all("../../mlrMBO")
library('testthat')

library(roxygen2)
roxygenise('..')

devtools::load_all("..")
options(error=dump.frames)

##
pid.task
##

library(utils)
evalWithTimeout

resRand <- automlr(pid.task, budget=c(evals=10), backend="random", verbosity=6, searchspace=list(mlrLearners$classif.logreg, mlrLearners$classif.knn))
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

resample(learner=automlr:::makeTimeconstraintWrapper(makeLearner("classif.nodeHarvest"), 10), task=pid.task, resampling=cv10)

