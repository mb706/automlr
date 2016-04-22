
#install.packages("Rmpi", configure.args = c("--with-Rmpi-include=/usr/include/openmpi-x86_64/", "--with-Rmpi-libpath=/usr/lib64/openmpi/lib/",
#                   "--with-Rmpi-type=OPENMPI"))


options(width=150)
devtools::load_all("../../ParamHelpers")
devtools::load_all("../../mlr")
devtools::load_all("../../smoof")
devtools::load_all("../../mlrMBO")
library('testthat')

library(roxygen2)
roxygenise('..')

devtools::load_all("..")
options(error=dump.frames)



# check whether parallelization on the resample level (all)
# and on the tune level (random, ...?) work.

# resampling timeout cases to test:
#
#  - all runs below timeout --> normal behaviour
#  - first run above firstIterTimeout --> error, others not run
#  - first run below firstIterTimeout, above normal timeout --> others are run, first is imputed
#  - first run below firstIterTimeout, above normal timeout --> others imputed when they time out
#  - first run below normal timeout --> others imputed when they time out
#  - run below firstIterTimeout, above normal timeout, resampling only 1 iter --> error
#  - run below firstIterTimeout, above normal timeout, not resampling --> error
#
# CROSS WITH:
#
# - parallelizing the resampling
# - parallelizing some other level
# - not parallelizing
#

# TODO: check whether isResampleParallel works inside the parallelization

# can check whether on slave by 'getPMOption("on.slave")'

# wrap tryCatch so that it doesn't catch out of time errors.
# wrap checkResultsAndStopWithErrorsMessages to 
 
assignInNamespace("checkResultsAndStopWithErrorsMessages", ns = "parallelMap", function(...) { Sys.sleep(2) ; print("checkresults") })

####
# the setTimeLimit stuff

library("parallelMap")

# modes:
# local, multicore, socket, mpi, BatchJobs


setTimeLimit()
parallelLibrary("parallelMap")
parallelStartSocket(2)  # frame not visible
parallelStartLocal()  # frame visible
parallelStartMulticore(2)  # frame visible
#parallelStartMPI(2)  # frame not visible
#parallelStartBatchJobs(2)  # does not work
z <- NULL
(z <- withRestarts({withCallingHandlers(y <- dopar(), error = exitOnTimeout); TRUE }, automlr.timeout = function() { print("oto"); FALSE }))
z
parallelStop()

y
z

identical(parallelGetOptions()$settings$mode, "mpi")

parallelGetOptions()

parallelGetOptions

parallelMap:::print.ParallelMapOptions

f = function(i) {
  getFrameVar = function(fname, varname) {
    calls = sys.calls()
    calls[[length(calls) - 1]] = NULL
    callnames = sapply(calls, function(x) try(as.character(x[[1]]), silent = TRUE))
    frameno = tail(which(callnames == fname), n = 1)
    if (length(frameno) < 1) {
      return(NULL)
    }
    sys.frame(frameno)[[varname]]
  }
  print("starting sleep")
  Sys.sleep(5)
  print("done sleeping")
  getFrameVar("dopar", "a")
}

dopar = function() {
  a = 1
  on.exit(setTimeLimit())
  setTimeLimit(elapsed = 10, transient = TRUE)
  parallelMap(f, 1:2)
}

exitOnTimeout = function(cond) {
  setTimeLimit()
  timeoutMessage = "reached elapsed time limit"
  print("hi from ontimeout. message was:")
  print(conditionMessage(cond))
  if (conditionMessage(cond) == timeoutMessage) {
    return(FALSE)
    invokeRestart("automlr.timeout")
  }
  signalCondition(cond)
}


f2 = function(i) {
  asin(0)
}

dopar2 = function() {
  myasin = base::asin
  myAIN = function(what, value, ns) {
    w = options("warn")
    on.exit(options(w))
    options(warn = -1)
    where = as.environment(paste("package", ns, sep = ":"))
    if (bindingIsLocked(what, where)) {
      unlockBinding(what, where)
      assign(what, value, where)
      lockBinding(what, where)
    } else {
      assign(what, value, where)
    }
  }
  on.exit(myAIN("asin", myasin, "base"))
  myAIN("asin", function(...) { print('asin called') ; print(sys.calls()) ; FALSE }, "base") 
  asin(0)
  parallelMap(f2, 1:2)
}

dopar2()

parallelStartSocket(2)  # frame not visible, modifications not visible, [timeouterror caught?]
parallelStartLocal()  # frame visible, modification visible
parallelStartMulticore(2)  # frame visible, modification visible
#parallelStartMPI(2)  # frame not visible
#parallelStartBatchJobs(2)  # does not work
dopar2()
parallelStop()


f3 = function(i) {
  tryCatch(Sys.sleep(4), error = function(e) print("yo"))
}

dopar3 = function() {
  on.exit(setTimeLimit())
  setTimeLimit(elapsed = 1, transient = TRUE)
  parallelMap(f3, 1:4)
}


parallelStartSocket(2)  # frame not visible, modifications not visible, timeouterror does not carry over but stays local, callinghandlers stay local.
parallelStartLocal()  # frame visible, modification visible, timeouterror carries over, callinghandlers carry over
parallelStartMulticore(2)  # frame visible, modification visible, timeouterror carries over AND stays local, callinghandlers stay local --> good for jumping back.
#parallelStartMPI(2)  # frame not visible
#parallelStartBatchJobs(2)  # can't test this right now.
dopar3()
parallelStop()


# TODO: test if callinghandlers carry over.


fx = function() {
  setTimeLimit(elapsed = 1, transient = TRUE)
  on.exit(print("a"), add = TRUE)
  on.exit(Sys.sleep(3), add = TRUE)
  on.exit(print("b"), add = TRUE)
  print("hi")
#  Sys.sleep(3)
  print("end")
}


fx()
setTimeLimit()


TRUE && stop("test")

withCallingHandlers(
    withRestarts(
        withCallingHandlers(fx(), error = function(...) { print(list(...)) ; print("inner ch") ; print(sys.calls()); invokeRestart("automlr.timeout", 'yo')} ),
        automlr.timeout = function(x) { print(x); FALSE }),
    error = function(...) { print(list(...)) ; print("outer ch") ; print(sys.calls())} )


b = function() {
  Sys.sleep(1)
  on.exit(setTimeLimit(elapsed = 1e-15, transient = TRUE))
}

a = function() {
  y = 2
  Sys.sleep(3)
  b()
  Sys.sleep(2.1)
  Sys.sleep(2.2)
}

a()


oto = function(cond) {
  if (conditionMessage(cond) == "test") {
    invokeRestart("test")
  }
}

oto2 = function(cond) {
  if (conditionMessage(cond) == "test2") {
    invokeRestart("test2")
  }
}


withRestarts({
    withCallingHandlers({
        withRestarts({
            withCallingHandlers({
                stop("test2")
                0
              }, error = oto)
          }, test = function() 2)
      }, error = oto2)
  }, test2 = function() 3)



z <- 1
withRestarts({
    x <- z
    y <- 3
    print("here we go")
    if (x < y) {
      invokeRestart("zts")
    }
    print("done")
  }, zts = function() {z <<- z + 1})

gz



tryCatch({
    a()
    try(stop(structure(list(message="msg"), class=c("myerrclass", "condition"))))
  },
#         error = function(e) print("error"),
         myerrclass = function(e) print("myerr"))

try
