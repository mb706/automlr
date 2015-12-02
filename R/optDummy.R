
#' Reference implementation that exemplifies the backend interface.
#'
#' should return whatever kind of object the backend accepts as 'prior'.
#' @param prior A 'prior' object as returned by \code{\link{extractprior.dummy}}.
#'        This is never null.
#' @param newprior same as 'prior'.
combinepriors.dummy = function(prior, newprior) {
  cat("Called 'combinepriors'.\n")
  prior + newprior  # our cute 
}

#' should return whatever kind of object this backend accepts as 'prior'.
#' 
#' @param env The private data of this backend.
extractprior.dummy = function(env) {
  cat("Called 'extractprior'\n")
  env$prior
}

#' return value is ignored; should modify env.
#' 
#' 'prior' and 'learner' will not be passed to optimize.<backend>, so they
#' should probably saved in 'env'.
#' @param env The private data of this backend.
#' @param prior The prior as passed to the \code{\link{automlr}} invocation.
#' @param learner the learner object that was built from the declared search space.
setup.dummy = function(env, prior, learner) {
  cat("Called 'setup'\n")
  env$prior = coalesce(prior, 1)
  env$learner = learner
  env$evals = 0
  invisible()
}

#' return a vector detailing the spent budget.
#' 
#' optimization progress should be saved to 'env'.
#' @param env The private data of this backend
#' @param stepbudget The budget for this optimization step with one or several of the entries \code{walltime}
#'        (time since invocation), \code{cputime} (total cpu time of optimization process), \code{modeltime}
#'        (time spent executing model fits), \code{evals} (number of model fit evaluations). Time is always
#'        given in seconds.
optimize.dummy = function(env, stepbudget) {
  cat("Called 'optimize' with budget:\n")
  print(stepbudget)
  env$evals = env$evals + 1
  env$prior = env$prior + 1
  c(walltime=10, cputime=10, modeltime=10, evals=1)
}

#' must return a named list which will be inserted into the result object.
#' 
#' Required elements are:
#' 
#'  resultstring -- message that gets printed in 'print'
#' 
#' Names should not collide with AMState / AMResult property names.
#' @param env The private data of this backend.
result.dummy = function(env) {
  cat("Called 'result'\n")
  list(resultstring=paste0("Dummy result. Prior grew to ", env$prior, ", evals: ", env$evals))
}
