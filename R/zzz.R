#' @import BBmisc
#' @import checkmate
#' @import mlr
#' @import ParamHelpers
#' @import parallelMap
#' @import utils
NULL

mlr.predictLearner.ModelMultiplexer = NULL

.onLoad = function(libname, pkgname) {

  mlrWrappers <<- mlrWrappers.gen()
  mlrLearners <<- mlrLearners.gen()
  mlrLightweight <<- mlrLightweight.gen()
  mlrLightweightNoJava <<- mlrLightweightNoJava.gen()

  mlr.predictLearner.ModelMultiplexer <<- mlr:::predictLearner.ModelMultiplexer

  timeoutMessage <<- determineTimeoutMessage()


  # this is necessary for some linux kernels to prevent segfaults.
  # if the JVM is already running, it is too late, however, and we just hope
  # the user knows what he's doing.
  options(java.parameters = c("-Xss2560k", "-Xmx2g"))

  # there is a limit on how many DLLs can be loaded; make sure mlrMBO
  # is loaded before the others; same with irace
  requirePackages("mlrMBO", why = "optMBO", default.method = "load", stop = FALSE,
    suppress.warnings = TRUE)
  requirePackages("irace", why = "optMBO", default.method = "load", stop = FALSE,
    suppress.warnings = TRUE)

}
