#' @title update the environment's prior by adding the information
#' contained in 'prior'.
#' 
#' @description
#' This is entirely backend-dependent.
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
#' @description
#' This is called usually after \code{\link{amaddprior}} to retrieve an updated
#' prior object.
#' 
#' @param env [\code{environment}]\cr
#'   The private data of this backend.
#' 
#' @return [any]
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
#' @param opt [\code{AutomlrBackendConfig}]\cr
#'   Options given for the backend. This is a list returned by a function that
#'   was registered for the backend using \code{\link{registerBackend}}.
#' @param prior [any]\cr
#'   The prior as passed to the \code{\link{automlr}} invocation.
#' @param learner [\code{Learner}]\cr
#'   The learner object that was built from the declared search space.
#' @param task [\code{Task}]\cr
#'   The task to optimize the \code{Learner} over.
#' @param measure [\code{Measure}]\cr
#'   The measure to optimize.
#' @param verbosity [\code{numeric(1)}]\cr
#'   Output options.
#' 
#' @return \code{NULL}
amsetup= function(env, opt, prior, learner, task, measure, verbosity) {
  UseMethod("amsetup")
}

#' @title Perform optimization, respecting the given budget.
#' 
#' @description
#' return a vector detailing the spent budget.
#' 
#' optimization progress should be saved to 'env'.
#' @param env [\code{environment}]\cr
#'   The private data of this backend.
#' @param stepbudget [\code{numeric}]\cr
#'   The budget for this optimization step with one or several of the entries
#'   \code{walltime} and \code{evals}. See \code{\link{automlr}} for details.
#' @param verbosity [\code{numeric(1)}]\cr
#'   Output options.
#' @param deadline [\code{numeric(1)}]\cr
#'   The number of seconds of runtime that this call should not exceed. While
#'   the time budget gives a soft limit and tries to finish calculations that
#'   have started by the time the budget is spent, this is a hard limit which
#'   should be kept as closely as possible, even if it means throwing away data.
#' 
#' @return [\code{numeric(4)}]
#' The budget spent during this invocation.
amoptimize = function(env, stepbudget, verbosity, deadline) {
  UseMethod("amoptimize")
}

#' @title Give information about the optimum found.
#' 
#' @description
#' This function can do some backend bound work to generate more information
#' about the found optimum.
#' 
#' @param env [\code{environment}]\cr
#'   The private data of this backend.
#' 
#' @return [\code{list}]
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