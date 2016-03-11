#' @title update the environment's prior by adding the information
#' contained in 'prior'.
#' 
#' @param env [\code{environment}]\cr
#'   The private data of this backend
#' @param prior [any]\cr
#'   A 'prior' object.
amaddprior = function(env, prior) {
  UseMethod("amaddprior")
}

#' @title return whatever kind of object this backend uses as 'prior'.
#' 
#' @param env [\code{environment}]\cr
#'   The private data of this backend.
#' 
#' @return [any]\cr
#' An object representing the prior.
amgetprior = function(env) {
  UseMethod("amgetprior")
}

#' @title Set up the backend private data.
#' 
#' @description
#' This function is called once in the lifetime of the backend private data. All
#' arguments except 'env' will not be passed to amoptimize, so they should be
#' saved in 'env'.
#' 
#' @param env [\code{environment}]\cr
#'   The private data of this backend.
#' @param prior [any]\cr
#'   The prior as passed to the \code{\link{automlr}} invocation.
#' @param learner [\code{Learner}]\cr
#'   The learner object that was built from the declared search space.
#' @param task [\code{Task}]\cr
#'   The task to optimize the \code{Learner} over.
#' @param measure [\code{Measure}]\cr
#'   The measure to optimize.
#' 
#' @return \code{NULL}
amsetup= function(env, prior, learner, task, measure) {
  UseMethod("amsetup")
}

#' @title Perform optimization, respecting the given budget.
#' 
#' return a vector detailing the spent budget.
#' 
#' optimization progress should be saved to 'env'.
#' @param env [\code{environment}]\cr
#'   The private data of this backend.
#' @param stepbudget [\code{numeric}]\cr
#'   The budget for this optimization step with one or several of the entries
#'   \code{walltime}, \code{cputime} \code{modeltime} and \code{evals}. See
#'   \code{\link{automlr}} for details.
#' 
#' @return [\code{numeric(4)}]\cr
#' The budget spent during this invocation.
amoptimize = function(env, stepbudget) {
  UseMethod("amoptimize")
}

#' @title Give information about the optimum found.
#' 
#' @param env [\code{environment}]\cr
#'   The private data of this backend.
#' 
#' @return [\code{list}]\cr
#' A named list which will be inserted into the result object. Required elements
#' are:
#' \describe{
#'   \item{learner [\code{Learner}]}{The (constructed) learner that achieved the
#'     optimum.}
#'   \item{opt.point [\code{list}]}{Optimal hyperparameters for learner.}
#'   \item{opt.val [\code{numeric}]}{Optimum reached for the \code{AMState}'s
#'     \code{measure}.}
#'   \item{opt.path [\code{OptPath}]}{Information about all the evaluations
#'     performed.}
#'   \item{result [any]}{Furthe backend-dependent information about the
#'     optimum.}
#'  }
#' 
#' Further elements are possible, but names should not collide with
#' \code{AMState} / \code{AMResult} slot names.
amresult = function(env) {
  UseMethod("amresult")
}