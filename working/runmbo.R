library("automlr")
library("mlrMBO")
options(error=dump.frames)
amrun = automlr(pid.task, backend = "mbo", budget = c(walltime = 3600 * 7), max.walltime.overrun=Inf, verbosity = 5, max.learner.time=500, searchspace=mlrLightweight[1:53],
save.interval = 1200, savefile = "./")

