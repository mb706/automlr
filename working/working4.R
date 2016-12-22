


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

automlr(pid.task, budget=c(evals=10), backend="random")

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
