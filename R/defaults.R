# Constants and default values should be defined here.

default.save.interval = 300

requiredBackendFunctions = c("amcombinepriors", "amgetprior", "amsetup",
    "amoptimize", "amresult")

# the default resample method to use during optimization.
resampleOptions = list(method = "CV", iters = 5)