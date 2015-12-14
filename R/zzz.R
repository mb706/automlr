#' @import BBmisc
#' @import checkmate
#' @import mlr
#' @import ParamHelpers
#' @import parallelMap
NULL

#' Greet the user with some annoying message
#' 
#' ...to check whether package loads correctly.
#' TODO: remove this
#' @keywords internal
.onAttach = function(libname, pkgname) {
  packageStartupMessage(paste("Loaded package", pkgname, "from lib", libname))
}