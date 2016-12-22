# Constants and default values should be defined here.

# default save interval is 5 minutes
default.save.interval = 300

requiredBackendFunctions = c("amaddprior", "amgetprior", "amsetup",
    "amoptimize", "amresult")


registered.backend = new.env()

#' @title Register a new backend
#' 
#' @description
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
#' @note
#' Use auxiliary function \code{\link{argsToList}} to return a list of all
#' given formal arguments.
#'
#' @examples
#' \dontrun{
#' makeMyBackendOptions = registerBackend("mybackend",
#'     function(opt1 = 1, opt2 = "a") {
#'       list(opt1 = opt1, opt2 = opt2)
#'     })
#' 
#' # equivalent:
#' makeMyBackendOptions = registerBackend("mybackend",
#'     function(opt1 = 1, opt2 = "a") {
#'       argsToList()
#'     })
#' 
#' # the following works if you also defined amsetup.mybackend,
#' # amoptimize.mybackend, etc.
#' amresult1 = automlr(iris.task, budget = c(evals = 10),
#'     backend = makeMyBackendOptions(opt1 = 2, opt2 = "b"))
#' 
#' # the following also works; it uses the defaults as defined in the funciton
#' # header.
#' amresult2 = automlr(iris.task, budget = c(evals = 10), backend = "mybackend")
#' }
#' @export
registerBackend = function(name, creator) {
  assertString(name)
  assertFunction(creator)
  returnFnc = creator
  body(returnFnc) = expression({
        if (!isambackend(name)) {
          stopf("The backend '%s' does not have all necessary generics.", name)
        }
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
  registered.backend[[name]] = returnFnc
  returnFnc
}

#' @export
print.AutomlrBackendConfig = function(x, ...) {
  print(attr(x, "automlr.backend.invocation"))
}

#' @title create a list of formal arguments
#' 
#' @description
#' Return a list of formal arguments. Useful for \code{\link{registerBackend}}.
#' 
#' @examples
#' fun <- function(x = 1, y = 2) {
#'   argsToList()
#' }
#' 
#' fun(y = 10)
#' fun()
#' @export
argsToList = function() {
  defaults = formals(sys.function(-1))
  defaults$`...` = NULL
  defaults = sapply(defaults, eval, envir = sys.frame(-1))
  assigns = as.list(eval.parent(quote(match.call())))
  assigns[[1]] = NULL
  insert(defaults, assigns)
}





