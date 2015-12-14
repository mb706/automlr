# Constants and default values should be defined here.

default.save.interval = 300

requiredBackendFunctions = c("amcombinepriors", "amgetprior", "amsetup", "amoptimize", "amresult")

resampleOptions = list(method="CV", iters=5)  # the default resample method to use during optimization.