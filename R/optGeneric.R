#' should return whatever kind of object the backend accepts as 'prior'.
#' 
#' @param env should not be used
#' @param prior A 'prior' object as returned by \code{\link{amgetprior}}.
#'        This is never null.
#' @param newprior same as 'prior'.
amcombinepriors = function(env, prior, newprior) {
  UseMethod("amcombinepriors")
}

#' should return whatever kind of object this backend accepts as 'prior'.
#' 
#' @param env The private data of this backend.
amgetprior = function(env) {
  UseMethod("amgetprior")
}

#' return value is ignored; should modify env.
#' 
#' 'prior' and 'learner' will not be passed to optimize.<backend>, so they
#' should probably saved in 'env'.
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
#'  resultstring -- message that gets printed in 'print'
#' 
#' Names should not collide with AMState / AMResult property names.
#' @param env The private data of this backend.
amresult = function(env) {
  UseMethod("amresult")
}