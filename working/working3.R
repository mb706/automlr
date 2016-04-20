


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



# check whether parallelization on the resample level (all)
# and on the tune level (random, ...?) work.

# resampling timeout cases to test:
#
#  - all runs below timeout --> normal behaviour
#  - first run above firstIterTimeout --> error, others not run
#  - first run below firstIterTimeout, above normal timeout --> others are run, first is imputed
#  - first run below firstIterTimeout, above normal timeout --> others imputed when they time out
#  - first run below normal timeout --> others imputed when they time out
#  - run below firstIterTimeout, above normal timeout, resampling only 1 iter --> error
#  - run below firstIterTimeout, above normal timeout, not resampling --> error
#
# CROSS WITH:
#
# - parallelizing the resampling
# - parallelizing some other level
# - not parallelizing
#
