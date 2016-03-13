library(testthat)
library(checkmate)
library(mlr)
library(automlr)
SHORTRUN = TRUE
test_check("automlr", filter="aux")
