# Constants and default values should be defined here.

default.save.interval = 300

requiredBackendFunctions = c("amaddprior", "amgetprior", "amsetup",
    "amoptimize", "amresult")


# optMBO
# reduce types of parameter set to simple types to avoid mlrMBO bugs
mboSaveMode = TRUE
mbo.focussearch.points = 1000L
mbo.focussearch.maxit = 5L
mbo.focussearch.restarts = 1L

# optRandom
out.of.budget.string = "out of budget"
# the default resample method to use during optimization.
resampleOptions = list(method = "CV", iters = 5)

# optIRace
irace.nbIterations = 10
irace.newpopulation = 2

# globals

registered.backend = new.env()

#' @title Register a new backend
#' 
#' Make it possible to invoke automlr() with your own custom backend. Besides
#' providing a creator function as an argument here, you need to make the S3
#' methods \code{\link{amaddprior}}, \code{\link{amgetprior}},
#' \code{\link{amsetup}}, \code{\link{amoptimize}} and \code{\link{amresult}}
#' available for an object of class \code{am[backendname]}.
#' 
#' @param name [\code{character(1)}]\cr
#'   The name of your backend. The user will be able to invoke
#'   \code{\link{automlr}} with argument \code{backend} set to this name to
#'   perform optimization using your backend.
#' @param creator [\code{function}]\cr
#'   A function that can be run with no arguments and returns an object that
#'   will be given to the \code{\link{amsetup}} function of the backend. It may
#'   have more options that allow the user to set options for the backend.\cr
#'   Note: The function that is given as an argument should not be used directly
#'   by the user. Instead, the return value of \code{\link{registerBackend}}
#'   must be used for that purpose.
#' 
#' @return [\code{function}]\cr
#'   The function that can be used to create a "BackendOptions" object that can
#'   then be given as the \code{backend} parameter to \code{\link{automlr}}.
#'
#' @examples
#' \dontrun{
#' makeMyBackendOptions = registerBackend("mybackend",
#'     function(opt1 = 1, opt2 = "a") {
#'       list(opt1 = opt1, opt2 = opt2)
#'     })
#' 
#' # the following works if you also defined amsetup.mybackend,
#' # amoptimize.mybackend, etc.
#' amresult1 = automlr(iris.task, budget = c(evals = 10),
#'     backend = makeMyBackendOptions(opt1 = 2, opt2 = "b"))
#' 
#' # the following also works; it uses the defaults as defined in the funciton
#' # header.
#' amresult2 = automlr(iris.task, budget = c(evals = 10), backend = "myBackend)
#' }
registerBackend = function(name, creator) {
  assertString(name)
  assertFunction(creator)
  returnFnc = creator
  body(returnFnc) = expression({
        args = match.call()
        fnchead = args[[1]]
        args[[1]] = quote(list)
        args = eval.parent(args)
        resultObject = do.call(creator, args)
        invocation = as.call(c(list(fnchead), args))
        attr(resultObject, "automlr.backend") = name
        attr(resultObject, "automlr.backend.invocation") = invocation
        addClasses(resultObject, "AutomlrBackendConfig")
      })
  environment(returnFnc) = environment()
  registered.backend[name] = returnFnc
  returnFnc
}

#' @export
print.AutomlrBackendConfig = function(x, ...) {
  print(attr(x, "automlr.backend.invocation"))
}