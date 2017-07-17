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


  # TODO: this is necessary for some linux kernels to prevent segfaults.
  options(java.parameters = c("-Xss2560k", "-Xmx2g"))


}
