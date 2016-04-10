
#' @title Reference implementation that exemplifies the backend interface.
#'
#' @description
#' Counts the number of priors used.
#' 
#' @param env [\code{environment}]\cr
#'   The private data of this backend
#' @param prior [any]\cr
#'   A 'prior' object.
amaddprior.amdummy = function(env, prior) {
  cat("Called 'combinepriors'.\n")
  # our cute prior mechanism
  env$prior = env$prior + prior
  invisible()
}

#' @title Reference implementation that exemplifies the backend interface.
#' 
#' @description
#' Just give the prior object, which will be a number in this case.
#' 
#' @param env [\code{environment}]\cr
#'   The private data of this backend.
#' 
#' @return [any]
#' An object representing the prior.
amgetprior.amdummy = function(env) {
  cat("Called 'extractprior'\n")
  env$prior
}

#' @title Reference implementation that exemplifies the backend interface.
#' 
#' @description
#' The dummy backend here just saves the information necessary to give some
#' bogus result in \code{\link{amresult.amdummy}}.
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
#' @param verbosity [\code{numeric(1)}]\cr
#'   Output options.
#' 
#' @return \code{NULL}
amsetup.amdummy = function(env, prior, learner, task, measure, verbosity) {
  cat("Called 'setup'\n")
  env$prior = coalesce(prior, 1)
  env$learner = learner
  env$evals = 0
  env$measure = measure
  invisible()
}

#' @title Reference implementation that exemplifies the backend interface.
#' 
#' @description
#' Simulate the spending of budget.
#' 
#' optimization progress should be saved to 'env'.
#' @param env [\code{environment}]\cr
#'   The private data of this backend.
#' @param stepbudget [\code{numeric}]\cr
#'   The budget for this optimization step with one or several of the entries
#'   \code{walltime}, \code{cputime} \code{modeltime} and \code{evals}. See
#'   \code{\link{automlr}} for details.
#' @param verbosity [\code{numeric(1)}]\cr
#'   Output options.
#' 
#' @return [\code{numeric(4)}]
#' The budget spent during this invocation.
amoptimize.amdummy = function(env, stepbudget, verbosity) {
  cat("Called 'optimize' with budget:\n")
  print(stepbudget)
  env$evals = env$evals + 1
  env$prior = env$prior + 1
  c(walltime = 10, cputime = 10, modeltime = 10, evals = 1)
}

#' @title Reference implementation that exemplifies the backend interface.
#' 
#' @description
#' For the dummy backend, the result will be a random point in the search space.
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
amresult.amdummy = function(env) {
  cat("Called 'result'\n")
  list(learner = env$learner,
      opt.val = 0,
      opt.point = removeMissingValues(sampleValue(env$learner$searchspace,
              trafo = TRUE)),
      opt.path = makeOptPathDF(env$learner$searchspace, "y",
          env$measure$minimize),
      result = NULL)
}
