#' @import BBmisc
#' @import checkmate
#' @import mlr
#' @import ParamHelpers
#' @import parallelMap
NULL


.onLoad = function(libname, pkgname) {
  mlrWrappers <<- mlrWrappers.gen()
  mlrLearners <<- mlrLearners.gen()
}