
#' Reference implementation that exemplifies the backend interface.
#'
#' should update the environment's prior by adding the information
#' contained in 'prior'.
#' @param env should not be used
#' @param prior A 'prior' object as returned by \code{\link{amgetprior.amdummy}}.
#'        This is never null.
amaddprior.amdummy = function(env, prior) {
  cat("Called 'combinepriors'.\n")
  env$prior = env$prior + prior  # our cute prior mechanism
  invisible()
}

#' should return whatever kind of object this backend accepts as 'prior'.
#' 
#' @param env The private data of this backend.
amgetprior.amdummy = function(env) {
  cat("Called 'extractprior'\n")
  env$prior
}

#' return value is ignored; should modify env.
#' 
#' all arguments except 'env' will not be passed to amoptimize, so they
#' should be saved in 'env' in some way.
#' @param env The private data of this backend.
#' @param prior The prior as passed to the \code{\link{automlr}} invocation.
#' @param learner the learner object that was built from the declared search space.
#' @param task the task
#' @param measure the measure to optimize
amsetup.amdummy = function(env, prior, learner, task, measure) {
  cat("Called 'setup'\n")
  env$prior = coalesce(prior, 1)
  env$learner = learner
  env$evals = 0
  env$measure = measure
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
amoptimize.amdummy = function(env, stepbudget) {
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
amresult.amdummy = function(env) {
  cat("Called 'result'\n")
  list(opt.val=0, opt.point=removeMissingValues(sampleValue(env$learner$searchspace, trafo=TRUE)),
      opt.path=makeOptPathDF(env$learner$searchspace, "y", env$measure$minimize))
}
