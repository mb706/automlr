


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
