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
  
  mlr.predictLearner.ModelMultiplexer <<- mlr:::predictLearner.ModelMultiplexer

  timeoutMessage <<- determineTimeoutMessage()
  
  
}