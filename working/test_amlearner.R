#!/usr/bin/env Rscript

argv = as.numeric(commandArgs(trailingOnly = TRUE)[1])

library("automlr")

learneridx = argv %% length(mlrLearnersNoWrap) + 1


automlr(pid.task,
  budget = c(evals = 5), verbosity=3,
  searchspace = list(mlrLearnersNoWrap[[learneridx]]), backend = "random")


cat("Finished successfully\n")
