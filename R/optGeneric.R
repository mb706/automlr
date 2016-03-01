#' should update the environment's prior by adding the information
#' contained in 'prior'
#' 
#' @param env should not be used
#' @param prior A 'prior' object as returned by \code{\link{amgetprior}}.
#'        This is never null.
amaddprior = function(env, prior) {
  UseMethod("amaddprior")
}

#' should return whatever kind of object this backend accepts as 'prior'.
#' 
#' @param env The private data of this backend.
amgetprior = function(env) {
  UseMethod("amgetprior")
}

#' return value is ignored; should modify env.
#' 
#' all arguments except 'env' will not be passed to amoptimize, so they
#' should be saved in 'env' in some way.
#' @param env The private data of this backend.
#' @param prior The prior as passed to the \code{\link{automlr}} invocation.
#' @param learner the learner object that was built from the declared search space.
#' @param task the task.
#' @param measure the measure to optimize
amsetup= function(env, prior, learner, task, measure) {
  UseMethod("amsetup")
}

#' return a vector detailing the spent budget.
#' 
#' optimization progress should be saved to 'env'.
#' @param env The private data of this backend
#' @param stepbudget The budget for this optimization step with one or several of the entries \code{walltime}
#'        (time since invocation), \code{cputime} (total cpu time of optimization process), \code{modeltime}
#'        (time spent executing model fits), \code{evals} (number of model fit evaluations). Time is always
#'        given in seconds.
amoptimize = function(env, stepbudget) {
  UseMethod("amoptimize")
}

#' must return a named list which will be inserted into the result object.
#' 
#' Required elements are:
#' 
#' $opt.val, $opt.point, $opt.path
#' 
#' Names should not collide with AMState / AMResult property names.
#' @param env The private data of this backend.
amresult = function(env) {
  UseMethod("amresult")
}