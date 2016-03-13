library(testthat)
library(mlr)
library(automlr)
library(checkmate)
# This is an auxiliary file for interactive testing using source().
# Always source this file using `chdir = TRUE`. The helper_*.R files will also
# be sourced and one can start running test_*.R files.

SHORTRUN = TRUE

files = sort(dir("testthat", "^helper_.*\\.[rR]$", full.names = TRUE))
lapply(files, source, chdir = TRUE)
