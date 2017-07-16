#!/usr/bin/env Rscript

argv = as.numeric(commandArgs(trailingOnly = TRUE)[1])
library("methods")
library("automlr")



system.time(automlr(pid.task,
  budget = c(walltime = 3600), verbosity=5, save.interval = 600,
  savefile = "./", max.learner.time = 600,
  searchspace = mlrLightweightNoWrap, backend = "random"),
  FALSE)

cat("Finished successfully\n")
