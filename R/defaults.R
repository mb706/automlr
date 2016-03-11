# Constants and default values should be defined here.

default.save.interval = 300

requiredBackendFunctions = c("amaddprior", "amgetprior", "amsetup",
    "amoptimize", "amresult")


# optMBO
# reduce types of parameter set to simple types to avoid mlrMBO bugs
mboSaveMode = TRUE

# optRandom
out.of.budget.string = "out of budget"
# the default resample method to use during optimization.
resampleOptions = list(method = "CV", iters = 5)

# optIRace
irace.nbIterations = 10
irace.newpopulation = 2
