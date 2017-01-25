
# This function is called by runWithTimeout() if the backend is set to "fork".
# see runwithtimeout.R
runWithTimeoutFork = function(expr, time, throwError, myName) {

  if (areInterruptsSuspended()) {
    stop(paste("runWithTimeout with backend 'fork'",
            "must not be called with disabled interrupts"))
  }
  
  errMsg = paste("Timeout:", timeoutMessage)
  
  if (time <= 0) {
    if (throwError) {
      stop(errMsg)
    } else {
      structure(FALSE, elapsed = 0)
    }
  }

  invocationTime = as.integer(round(proc.time()[3] * 1000))
  aborted = FALSE
  
  # this call is nested inside another runWithTimeoutFork call. If the outer
  # call has a shorter timeout, we are in danger of creating orphaned
  # processes.
  useOuterTimeout = FALSE
  nextTimeout = getFrameVar(myName, "thisTimeout")
  if (!is.null(nextTimeout)) {
    # leave 4 seconds as buffer for safety
    useOuterTimeout = thisTimeout + 4 > nextTimeout
  }
  
  if (useOuterTimeout) {
    thisTimeout = nextTimeout
    result = expr
    # if we arrive here, time did not run out.
  } else {
    job = list()
    # in case we get interrupted before the job finishes, we prepare to kill
    # it quickly on exit.
    on.exit(if (!is.null(job$pid)) {
          tools::pskill(job$pid, tools::SIGKILL)
          parallel::mccollect(job, wait=FALSE)
        })
    # fork()
    # supposedly silent=TRUE is buggy
    job = parallel::mcparallel(expr, mc.set.seed = FALSE, silent = FALSE)

    thisTimeout = proc.time()[3] + time  # is used by nested runWithTimeoutFork
    # wait()
    result = NULL

    # sometimes waiting for the process fails the first time, so do it in a loop
    while (
        is.null(result) &&
        (remainingTime <- (thisTimeout - proc.time()[3])) > 0) {
      Sys.sleep(0.001)  # yield so that process can be created
      result = parallel::mccollect(job, wait=FALSE, timeout=remainingTime + 1)
    }
    
    if (is.null(result)) {  # timeout
      aborted = TRUE
      tools::pskill(job$pid, tools::SIGTERM)
      Sys.sleep(1)
      tools::pskill(job$pid, tools::SIGKILL)
      parallel::mccollect(job, wait=FALSE)
      on.exit()
    }

    result = result[[1]]
    
    if (is.error(result)) {
      # if errors happen, the caller needs to tryCatch them.
      stop(attr(result, "condition"))
    }
  }

  finishTime = as.integer(round(proc.time()[3] * 1000))
  runtime = finishTime - invocationTime
  
  isTimeout = time * 1000 < runtime
  
  if (aborted && !isTimeout) {
    stopf("Running expression with timeout %s aborted, but runtime was %s.",
        time, runtime / 1000)
  }
  
  if (isTimeout) {
    # even if the execution was not aborted, if the time is above the limit,
    # we treat it as an abort.
    aborted = TRUE
    result = NULL
  }
  
  if (aborted && throwError) {
    # need to give a message that is guaranteed not to be the timeout message.
    # Otherwise we confuse runWithTimeoutNative.
    stop(errMsg)
  }
  
  list(result = result,
      timeout = aborted,
      elapsed = runtime / 1000)
  
}