


#' @title Run a given expression with a given (walltime) timeout.
#' 
#' @description
#' Runs `expr` with timeout `time` (in seconds) and returns a logical(1)
#' indicating success will be returned. If `throwError`, the return value will
#' always be TRUE, and an error will be thrown on timeout.
#'
#' runWithTimeout can also be called in a nested fashion.
#' 
#' @param expr [any]\cr
#' The expression that will be run with the given timeout.
#' @param time [\numeric(1)]\cr
#' The runtime, in seconds, after which to abort evaluation of expr.
#' @param throwError [\logical(1)]\cr
#' If \code{TRUE}, throw an error on timeout instead of just returning
#' \code{FALSE}.
#' @return [\code{logical(1)}]\
#' \code{TRUE} if the evaluation of \code{expr} finished successfully.
#' \code{FALSE} if \code{throwError} is \code{FALSE} and the evaluation timed
#' out.
#' The return value will have an attribute 'elapsed' indicating the time spent
#' evaluating expr. The value is guaranteed to be lower or equal to \code{time}
#' if TRUE is returned and to be greater than \code{time} if FALSE is returned.
#' @export
runWithTimeout = function(expr, time, throwError = FALSE) {
  myName = "runWithTimeout"
  myCallName = as.character(sys.call()[[1]])
  if (!identical(myName, myCallName)) {
    # the name of the function on the call stack must be 'runWithTimeout'.
    # If this function was called with a different name (e.g. by things like
    # > x = runWithTimeout
    # > x(1 + 1, 100)
    # ) we need to call it again, this time with the right name.
    return(runWithTimeout(expr, time, throwError))
  }
  assertNumeric(time, lower = 0, length = 1, any.missing = FALSE)
  assertFlag(throwError)
  checkParallelMapAllowed()

  # we use fixed point numbers with ms resolution.
  time = ifelse(is.infinite(time), time, as.integer(round(time * 1000)))

  # if runWithTimeout is called *within another* runWithTimeout, firstCall is
  # FALSE, otherwise it is TRUE
  firstCall = is.null(getFrameVar(myName, "firstCall"))

  if (firstCall) {
    # patch some functions that are sensitive if used inside runWithTimeout.
    patchObj = getPatchFunctions()

    # be extra careful we won't be interrupted in all of this.
    setTimeLimit()
    on.exit(unpatchFunctions(patchObj))
    patchFunctions(patchObj)
    nextTimeout = Inf
  } else {
    patchObj = getFrameVar(myName, "patchObj")
    # same precautions as above
    patchObj$setTimeLimit$orig()

    nextTimeout = getFrameVar(myName, "thisTimeout")
  }
  trueSetTimeLimitMs = function(elaps.millis = Inf) {
    elaps = elaps.millis / 1000
    if (!(elaps > 0)) {
      # zero elaps time switches time limit off, but in this context it means
      # we are out of time.
      patchObj$setTimeLimit$orig()
      stop(timeoutMessage)
    }
    patchObj$setTimeLimit$orig(elapsed = elaps, transient = TRUE)
  }
  trueWithCallingHandlers = if (firstCall) {
    patchObj$withCallingHandlers$orig
  } else {
    function(expression, ...) expression
  }
  trueTryCatch = patchObj$tryCatch$orig

  invocationTime = as.integer(round(proc.time()[3] * 1000))
  thisTimeout = invocationTime + time
  
  timeoutNotNew = thisTimeout >= nextTimeout
  if (timeoutNotNew) {
    # this runWithTimeout is a no-op, since a higher nesting level
    # runWithTimeout will trigger before `time` is spent. We need to make sure
    # the lower nested runWithTimeout have the correct `nextTimeout`, however.
    thisTimeout = nextTimeout
    trueSetTimeLimitMs(nextTimeout - invocationTime)
    expr
    # if we arrive here, time did not run out.
    aborted = FALSE
  } else {
    timeoutError = trueTryCatch({
          trueWithCallingHandlers({
                trueSetTimeLimitMs(thisTimeout - invocationTime)
                expr
                trueSetTimeLimitMs()
              }, error = onTimeout)
          NULL
        }, automlr.timeout = function(e) e)
    aborted = is.null(timeoutError)
  }
  finishTime = as.integer(round(proc.time()[3] * 1000))
  runtime = finishTime - invocationTime
  if (aborted && runtime <= time) {
    # we were aborted even though there was no timeout. This shouldn't happen.
    # If it was for a rounding error, we try to add some slack to the runtime.
    # Otherwise it is an error and we throw.
    runtime %+=% 10
  }
  nextLevelRemainingRuntime = nextTimeout - finishTime

  trueSetTimeLimitMs(nextLevelRemainingRuntime)

  isTimeout = runtime > time

  if (aborted && !isTimeout) {
    stopf("Running expression with timeout %s aborted, but runtime was %s.",
        time, runtime)
  }

  if (isTimeout) {
    # even if the execution was not aborted, if the time is above the limit,
    # we treat it as an abort.
    aborted = TRUE
  }

  if (aborted && throwError) {
    # need to give a message that is guaranteed not to be the timeout message.
    msg = paste("Timeout:", timeoutMessage)
    if (is.null(timeoutError)) {
      stop(msg)
    } else {
      stop(simpleError(msg, conditionCall(timeoutError)))
    }
  }

  structure(!aborted, elapsed = runtime / 1000)
}


# the handler used with withCallingHandlers inside runWithTimeout that converts
# timeout errors into a different class of condition so they can be filtered
# separately by tryCatch.
onTimeout = function(cond) {
  if (conditionMessage(cond) == timeoutMessage) {
    # the error was a time limit error. We throw an error of a condition of a
    # different class than "error" so debugging is still possible.
    signalCondition(makeS3Obj(c("automlr.timeout", "condition"),
            msg = "automlr timeout", call = conditionCall(cond)))
  }
  # if we don't invoke a restart, the condition is signalled to the next
  # handler.
}

# the following is the value in the english locale. Other locales might have a
# different message. Therefore this value will be modified in .onLoad in zzz.R.
timeoutMessage = "reached elapsed time limit"

determineTimeoutMessage = function() {
  on.exit(setTimeLimit())
  # playing with fire here.
  setTimeLimit(elapsed = 0.2, transient = TRUE)
  err = try(Sys.sleep(10), silent = TRUE)
  conditionMessage(attr(err, "condition"))
}

# check whether parallelMap uses a method that is compatible.
checkParallelMapAllowed = function() {
  parmode = getParallelOptions()$settings$mode
  # FIXME: maybe it does work with BatchJobs? Need to test this.
  disallowed = c("mpi", "BatchJobs")

  if (parmode %in% disallowed) {
    stopf("runWithTimeout does not work with parallelMaps in %s mode.", parmode)
  }
}

# The return object should be given to unpatchFunctions on.exit.
getPatchFunctions = function() {
  wrapHandler = function(handler, signal) {
    # 'signal' indicates we signal the condition. if signal is false, we do
    # nothing.
    force(handler)
    function(cond) {
      if (conditionMessage(cond) == timeoutMessage ||
          "automlr.timeout" %in% class(cond)) {
        # if the condition is either the timeout error or the converted timeout
        # error, we pass it through.
        # If 'signal' is true, this is a wrapped tryCatch handler and should
        # signal the condition again to pass it on. Otherwise, it is a
        # withCallingHandlers handler, which doesn't need to do anything to pass
        # on the signal.
        if (signal) {
          signalCondition(cond)
        } else {
          return(NULL)
        }
      }
      handler()
    }
  }
  wrapAllHandlers = function(handlers, signal) {
    classes = names(handlers)
    needsWrapping = which(classes %in% c("automlr.timeout",
            "simpleError", "error", "condition"))
    for (index in needsWrapping) {
      handlers[[index]] = wrapHandler(handlers[[index]], signal)
    }
    handlers
  }
  # the patches list needs to have a name, so the replacement functions can
  # access it.
  patches = list(
      setTimeLimit = list(
          # don't allow setTimeLimit
          ns = "base",
          orig = base::setTimeLimit,
          replacement = function(...) {
            stop(paste("setTimeLimit in the context of automlr::runWithTimeout",
                    "not allowed."))
          }),
      tryCatch = list(
          # make tryCatch ignore 'reached elapsed time limit' errors
          ns = "base",
          orig = base::tryCatch,
          replacement = function(expr, ..., finally) {
            # the replacement function takes all the handlers in `...`, replaces
            # them with functions that ignore our special errors, and then calls
            # the true tryCatch.
            handlers = wrapAllHandlers(list(...), TRUE)
            # need to quote expr and finally, since they are promises
            tcArgs = c(list(expr = quote(expr), finally = quote(finally)),
                handlers)
            eval.parent(substitute(do.call(patches$tryCatch$orig, tcArgs)))
          }),
      withCallingHandlers = list(
          # make withCallingHandlers ignore 'reached elapsed time limit' errors
          ns = "base",
          orig = base::withCallingHandlers,
          replacement = function(expr, ...) {
            # similar to the tryCatch wrapper
            handlers = wrapAllHandlers(list(...), FALSE)
            wchArgs = c(list(expr = quote(expr)), handlers)
            eval.parent(substitute(do.call(patches$withCallingHandlers$orig,
                        wchArgs)))
          }))
  # TODO: maybe checkResultsAndStopWithErrorsMessages in parallelMap needs to
  # be patched; this depends on how well it handles my tryCatch modifications.
  #
  #    checkResultsAndStopWithErrorsMessages = list(
  #        # if a child process in parallelMap threw a 'timeout' error, wait for
  #        # the error to arrive
  #        ns = "parallelMap",
  #        orig = parallelMap:::checkResultsAndStopWithErrorsMessages,
  #        replacement = ))
  patches
}

# patch a few functions to do things a bit differently.
# Strictly speaking we are not patching the functions, we are wrapping them.
patchFunctions = function(patchObj) {
  for (n in names(patchObj)) {
    myAssignInNamespace(n, patchObj[[n]]$replacement, patchObj[[n]]$ns)
  }
}

unpatchFunction = function(patchObj) { 
  for (n in names(patchObj)) {
    myAssignInNamespace(n, patchObj[[n]]$orig, patchObj[[n]]$ns)
  }
}

