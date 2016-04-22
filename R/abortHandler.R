

#' @title Evaluate an expression that won't be killed by Ctrl-C
#'
#' @description
#' This function evaluates its first argument and prevents the user from killing
#' the evaluation by pressing Ctrl-C. A timeout may be specified such that
#' pressing Ctrl-C twice in the given interval will actually kill the
#' evaluation. Together with handleCtrlC, this enables one to selectively
#' abort part of an evaluation without killing the whole program.
#'
#' @param expr [\code{any}]\cr
#'   an expression to be evaluated
#' @param hardKillInterval [\code{numeric(1)}]\cr
#'   The interval in which a second Ctrl-C press after a first one will kill the
#'   run regardless.
#'
#' @return [\code{any}]
#' The result of the evaluation of \code{expr}.
#'
#' @example
#' \dontrun{
#' # This example will lapply expensive_function(i). If the user wants to abort
#' # one of these runs without killing the whole run, he can press Ctrl-C -- 
#' # the 'result' array will then record an "aborted" character. If the user
#' # accidentally presses Ctrl-C in between runs of expensive_function(),
#' # nothing happens.
#' protectFromCtrlC({
#'     result = lapply(1:10, function(i) {
#'         handleCtrlC(expensive_function(i), "aborted")
#'       })
#'     }, hardKillInterval = 0.5)
#' }
protectFromCtrlC = function(expr, hardKillInterval = 0) {
  
}

#' @title Abort evaluation of an expression when Ctrl-C is pressed
#'
#' @description
#' Evaluate an expression and abort its evaluation when Ctrl-C is pressed,
#' without killing the program.
#'
#' @param expr [\code{any}]\cr
#'   The expression to evaluate
#' @param onCtrlC [\code{any}]\cr
#'   The value to return when Ctrl-C was pressed during evaluation of
#'   \code{expr}.
#'
#' @return [\code{any}]
#' The result of the evaluation of \code{expr}.
handleCtrlC = function(expr, onCtrlC) {
  
}