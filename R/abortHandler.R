
#' @title Evaluate an expression that won't be killed by Ctrl-C
#'
#' @description
#' This function evaluates its first argument and prevents the user from killing
#' the evaluation by pressing Ctrl-C. A timeout may be specified such that
#' pressing Ctrl-C twice in the given interval will actually kill the
#' evaluation. Together with handleInterrupts, this enables one to selectively
#' abort part of an evaluation without killing the whole program.
#' 
#' The suspension is not complete, it looks like some socket operations override
#' this. In any case, expensive operations should not happen while interrupts
#' are suspended.
#' 
#' Calls to suspendInterruptsFor can be nested; interrupts will be disabled by
#' the first call and only be reenabled when the first call exits.
#'
#' @param expr [\code{any}]\cr
#'   an expression to be evaluated
#' @param hardKillInterval [\code{numeric(1)}]\cr
#'   The interval (in seconds) in which a second Ctrl-C press after a first one
#'   will kill the run regardless. Ctrl-C presses are only evaluated within
#'   \code{\link{handleInterrupts}} calls; therefore this value is a minimum,
#'   the maximum being the time interval between exiting one
#'   \code{handleInterrupts} call and entering the next one.
#'
#' @return [\code{any}]
#' The result of the evaluation of \code{expr}.
#'
#' @examples
#' \dontrun{
#' # This example will lapply expensive_function(i). If the user wants to abort
#' # one of these runs without killing the whole run, he can press Ctrl-C -- 
#' # the 'result' array will then record an "aborted" character. If the user
#' # accidentally presses Ctrl-C in between runs of expensive_function(),
#' # nothing happens.
#' suspendInterruptsFor({
#'     result = lapply(1:10, function(i) {
#'         handleInterrupts(expensive_function(i), "aborted")
#'       })
#'     }, hardKillInterval = 0.5)
#' }
#' @export
suspendInterruptsFor = function(expr, hardKillInterval = 0) {
  myName = "suspendInterruptsFor"
  myCallName = as.character(sys.call()[[1]])
  if (!identical(myName, myCallName)) {
    # the name of the function on the call stack must be 'suspendInterruptsFor'.
    # If this function was called with a different name (e.g. using things like
    # > x = suspendInterruptsFor
    # > x(1 + 1, 100)
    # ) we need to call it again, this time with the right name.
    return(suspendInterruptsFor(expr, hardKillInterval))
  }

  assertNumeric(hardKillInterval, lower = 0, len = 1, any.missing = FALSE)

  # handleInterrupts reads and writes `lastInterrupt` and lastUIAnounce
  lastInterrupt = -Inf
  lastUIAnounce = -Inf

  ordinaryEnd = FALSE
  on.exit({
        suspension = .Internal(interruptsSuspended(lastState))
        if (!suspension && ordinaryEnd) {
          warning('Interrupts were enabled at the end of suspendInterruptsFor')
        }
        if (!lastState) {
          checkPendingInterrupts()
        }
      })
  lastState = .Internal(interruptsSuspended(TRUE))
  result = expr
  ordinaryEnd = TRUE
  result
}

#' @title Quickly evaluate an expression that won't be killed by Ctrl-C
#'
#' @description
#' Like \code{\link{suspendInterruptsFor}} but more lightweight. Does not allow
#' for internal handleInterrupts.
#'
#' @param expr [\code{any}]\cr
#'   Expression to evaluate that must not be interrupted by Ctrl-C and timeout
#'   events.
#'
#' @return [\code{any}]
#'   The result of evaluating \code{expr}.
#' @export
quickSuspendInterrupts = function(expr) {
  on.exit(.Internal(interruptsSuspended(lastState)))
  lastState = .Internal(interruptsSuspended(TRUE))
  expr
}


#' @title Check whether interrupts are suspended
#' 
#' @description
#' A debug function that gives info whether interrupts are suspended. Some R
#' operations and some library calls reactivate interrupt processing, but this
#' is apparently nowhere documented. This function allows one to take an
#' empirical approach.
#' 
#' @return [logical(1)]
#' Whether interrupts are suspended.
#' @export
areInterruptsSuspended = function() {
  isSuspended = .Internal(interruptsSuspended(TRUE))
  .Internal(interruptsSuspended(isSuspended))
  isSuspended
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
#' the result of \code{expr} if it evaluated successfully, \code{onInterrupt} if
#' an interrupt was caught.
#' @export
handleInterrupts = function(expr, onInterrupt) {
  # Check that 'quickSuspendInterrupts' was not called.
  if (!is.null(getFrameVar("quickSuspendInterrupts", "lastState"))) {
    stop("Calling handleInterrupts inside quickSuspendInterrupts not allowed.")
  }
  # get the previous state without enabling interrupts. If we enable interrupts
  # in the same command as assigning the result to previousState, there is a
  # small time window where interrupts are enabled but the variable is not
  # assigned and hence the on.exit handler fails.
  previousState = areInterruptsSuspended()
  # need to set the interrupt state to 'previousState' here AND inside the
  # tryCatch block since an error thrown by expr would bypass the reinstallation
  # of the state otherwise. When an error happens, there may be a small time
  # window of vulnerability where Ctrl-C kills the program.
  on.exit(.Internal(interruptsSuspended(previousState)))
  
  hardKillInterval = getFrameVar("suspendInterruptsFor", "hardKillInterval")

  lastTime = getFrameVar("suspendInterruptsFor", "lastInterrupt")
  lastUIAnounce = getFrameVar("suspendInterruptsFor", "lastUIAnounce")
  assert(is.null(lastTime) == is.null(lastUIAnounce))
  assert(is.null(lastTime) == is.null(hardKillInterval))
  currentTime = proc.time()[3]
  anounceUI = !is.null(lastTime) && currentTime - lastTime < hardKillInterval

  # need to check whether the user pressed Ctrl-C while interrupts were
  # suspended. If that happened, we set the 'lastInterrupt' to right now but
  # don't abort.

  tryCatch({
        .Internal(interruptsSuspended(FALSE))
        checkPendingInterrupts()
        .Internal(interruptsSuspended(previousState))
      }, interrupt = function(cond) {
        if (!is.null(lastTime)) {
          print("carryover")
          assignFrameVar("suspendInterruptsFor", "lastInterrupt",
              proc.time()[3])
          anounceUI <<- TRUE
        }
      })
  if (anounceUI)  {
    # this is the case if either Ctrl-C was pressed less than HKI before OR
    # a Ctrl-C was detected while interrupts were suspended.
    # But we don't want to spam the user with messages if handleInterrupts gets
    # called in quick succession, so we make sure we display the message only
    # once per hardKillInterval
    if (!currentTime - lastUIAnounce < hardKillInterval) {
      assignFrameVar("suspendInterruptsFor", "lastUIAnounce", proc.time()[3])
      catf("Press Ctrl-C in quick succession (%gs) to abort.", hardKillInterval)
    }
  }
  tryCatch({
        .Internal(interruptsSuspended(FALSE))
        result = expr
        .Internal(interruptsSuspended(previousState))
        result
      }, interrupt = function(cond) {
        # if two interrupts come in very close succession, we cannot prevent the
        # program from dying.
        .Internal(interruptsSuspended(TRUE))
        currentTime = proc.time()[3]
        assignFrameVar("suspendInterruptsFor", "lastInterrupt", currentTime)
        if (is.null(lastTime) || (currentTime - lastTime < hardKillInterval)) {
          print("Ctrl-C Abort")
          signalCondition(cond)
          invokeRestart("abort")
        }
        onInterrupt
      })
}

# Sys.sleep(0) does not sleep but does check for pending interrupts after
# interrupts are reenabled.
checkPendingInterrupts = function() Sys.sleep(0)
