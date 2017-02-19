


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
