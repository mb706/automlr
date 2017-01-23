runWithTimeoutBackend = "native"

#' @title Set the default runWithTimeout backend
#' 
#' @description
#' Sets the default backend used by \code{runWithTimeout}.
#' 
#' @param backend [\code{character(1)}]\cr
#' The backend to use by \code{runWithTimeout}. The two implemented backends are
#' \code{"fork"} (default) and \code{"native"}. The fork backend uses the
#' operating system's \code{fork} functionality. This is safer and protects
#' against crashes of most kind, but it prevents communication by reference
#' (e.g. S4 objects or environments) and it only works on UNIX kinds of
#' operating systems.
#' The native backend uses the crude and unreliable R internal
#' \code{setTimeLimit} functionality to facilitate timeout. This is system
#' independent.
#' @export
setDefaultRWTBackend = function(backend) {
  assertChoice(backend, c("native", "fork"))
  runWithTimeoutBackend <<- backend
}

#' @title Run a given expression with a given (walltime) timeout.
#' 
#' @description
#' Runs `expr` with timeout `time` (in seconds) and returns a logical(1)
#' indicating success. The return value contains the result of the evaluated
#' expression, as well as information about the execution.
#'
#' \code{runWithTimeout} can also be called in a nested fashion.
#' 
#' @param expr [any]\cr
#' The expression that will be run with the given timeout.
#' @param time [\code{numeric(1)}]\cr
#' The runtime, in seconds, after which to abort evaluation of expr. If this is
#' smaller or equal zero, expr will not be run and a timeout will be reported.
#' @param throwError [\logical(1)]\cr
#' If \code{TRUE}, throw an error on timeout instead of just returning
#' \code{FALSE}.
#' @return [\code{list}]\cr
#' a list with three items:\cr
#' \code{$result} contains the result of the evaluated expression \code{expr}.
#' If a timeout occurred and \code{throwError} is \code{FALSE}, this will be
#' \code{NULL}.\cr
#' \code{$timeout} if \code{throwError} is \code{FALSE}, this is \code{TRUE} if
#' a timeout occurred, \code{FALSE} otherwise. If \code{throwError} is
#' \code{TRUE}, this is always \code{TRUE}.\cr
#' \code{$elapsed} contains the time spent evaluating \code{expr}. The value is
#' guaranteed to be lower or equal to \code{time} if the returned
#' \code{$timeout} is \code{FALSE}, and is greater than \code{TIME} otherwise.
#' @export
runWithTimeout = function(expr, time, throwError = FALSE, backend) {
#  FIXME: is the following necessary?
#  myName = "runWithTimeout"
#  myCallName = as.character(sys.call()[[1]])
#  if (!identical(myName, myCallName)) {
#    # the name of the function on the call stack must be 'runWithTimeout'.
#    # If this function was called with a different name (e.g. by things like
#    # > x = runWithTimeout
#    # > x(1 + 1, 100)
#    # ) we need to call it again, this time with the right name.
#    return(runWithTimeout(expr, time, throwError))
#  }

  assertNumeric(time, len = 1, any.missing = FALSE)
  assertFlag(throwError)

  if (runWithTimeoutBackend == "native") {
    runWithTimeoutNative(expr, time, throwError, "runWithTimeoutNative")
  } else {
    runWithTimeoutFork(expr, time, throwError, "runWithTimeoutFork")
  }
}

runWithTimeoutNative = function(expr, time, throwError, myName) {
  checkParallelMapAllowed()
  
  errMsg = paste("Timeout:", timeoutMessage)
  
  if (time <= 0) {
    if (throwError) {
      stop(errMsg)
    } else {
      structure(FALSE, elapsed = 0)
    }
  }

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
    on.exit(quickSuspendInterrupts(unpatchFunctions(patchObj)))
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

  result = NULL

  if (timeoutNotNew) {
    # this runWithTimeout is a no-op, since a higher nesting level
    # runWithTimeout will trigger before `time` is spent. We need to make sure
    # the lower nested runWithTimeout have the correct `nextTimeout`, however.
    thisTimeout = nextTimeout
    trueSetTimeLimitMs(nextTimeout - invocationTime)
    result = expr
    # if we arrive here, time did not run out.
    aborted = FALSE
  } else {
    timeoutError = trueTryCatch({
          trueWithCallingHandlers({
                trueSetTimeLimitMs(thisTimeout - invocationTime)
                result <- expr
                trueSetTimeLimitMs()
              }, error = onTimeout)
          NULL
        }, automlr.timeout = function(e) e)
    aborted = !is.null(timeoutError)
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
        time / 1000, runtime / 1000)
  }

  if (isTimeout) {
    # even if the execution was not aborted, if the time is above the limit,
    # we treat it as an abort.
    aborted = TRUE
    result = NULL
  }

  if (aborted && throwError) {
    # need to give a message that is guaranteed not to be the timeout message.
    
    if (is.null(timeoutError)) {
      stop(errMsg)
    } else {
      stop(simpleError(errMsg, conditionCall(timeoutError)))
    }
  }

  list(result = result,
      timeout = aborted,
      elapsed = runtime / 1000)
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
  err = try({
    setTimeLimit(elapsed = 0.2, transient = TRUE)
    try(Sys.sleep(1), silent = TRUE)
  })
  conditionMessage(attr(err, "condition"))
}

# check whether parallelMap uses a method that is compatible.
checkParallelMapAllowed = function() {
  parmode = parallelGetOptions()$settings$mode
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
      handler(cond)
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
 
            if (missing(finally)) {
              # we do not want to touch 'expr', but we do need to turn the
              # handlers list into `...`, so we have another wrapper.
              tcwrapper <- function(...) {
                patches$tryCatch$orig(expr, ...)
              }
            } else {
              tcwrapper <- function(...) {
                patches$tryCatch$orig(expr, ..., finally)
              }
            }
            do.call(tcwrapper, handlers)
          }),
      withCallingHandlers = list(
          # make withCallingHandlers ignore 'reached elapsed time limit' errors
          ns = "base",
          orig = base::withCallingHandlers,
          replacement = function(expr, ...) {
            # similar to the tryCatch wrapper
            handlers = wrapAllHandlers(list(...), FALSE)
            wcwrapper <- function(...) {
              patches$withCallingHandlers$orig(expr, ...)
            }
            do.call(wcwrapper, handlers)
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

unpatchFunctions = function(patchObj) { 
  for (n in names(patchObj)) {
    myAssignInNamespace(n, patchObj[[n]]$orig, patchObj[[n]]$ns)
  }
}

