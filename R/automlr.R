#' @title Automatically choose a model with parameters to fit.
#' 
#' @description
#' This is the main entry point of automlr.
#' 
#' @param task [\code{Task} | \code{AMState} | \code{character(1)}]\cr
#'   Either: The mlr \code{Task} object to fit a model on. Or: An \code{AMState}
#'   object, or a \code{character(1)} containing the file name of an \code{.rds}
#'   file containing such an object. The AMState usually contains progress of a
#'   past optimization run that was aborted by the user, a crash, or which ran
#'   out of budget.
#' @param measure [\code{Measure}]\cr
#'   The mlr \code{Measure} object to optimize for. If this is not given and the
#'   first argument is a \code{Task} object, uses the task's default measure.
#' @param budget [\code{numeric} | list of \code{numeric}]\cr
#'   A named list or named vector with one or several of the entries
#'   \describe{
#'     \item{\code{walltime}}{time since invocation}
#'     \item{\code{cputime}}{total cpu time of optimization process}
#'     \item{\code{modeltime}}{time spent executing model fits}
#'     \item{\code{evals}}{number of model fit evaluations}
#'   }
#'   (Time is always given in seconds.)\cr
#'   When any of the budget criteria is exceeded, the optimization process will
#'   halt at the next possible point and return. In the current implementation,
#'   this is only checked between model fits and evaluations, so the time
#'   budgets may be exceeded, in some cases substantially. If the \code{taks}
#'   argument is an \code{AMState} object or a \code{character(1)} referring to
#'   an \code{AMState} rds-file, this is optional and defaults to the referenced
#'   \code{AMState}'s budget \emph{minus the already used up budget}. To
#'   continue an already finished run, therefore, one needs to pass a higher
#'   budget than the \code{$spent} slot indicates. Passing \code{0} will return
#'   an AMState object without performing any optimization or touching the file
#'   system.
#' @param searchspace [list of \code{Autolearner}]\cr
#'   Declaration of the searchspace: The mlr \code{Learner}s to use and the
#'   parameter domains to consider for optimization. \code{Learner}s can be
#'   chosen manually, either by creating custom \code{Autolearner} objects using
#'   \code{\link{autolearner}}, using elements of the provided
#'   \code{link{mlrLearners}} list, searching all implemented \code{Learner}s by
#'   using \code{link{mlrLearners}} (default), or searching all \code{Learner}s
#'   without considering preprocessing using \code{link{mlrLearnersNoWrap}}.\cr
#'   From the provided list, only \code{Learner}s that fit the task
#'   characteristics and type will be used, the others will be ignored without
#'   notification.
#' @param prior [any]\cr
#'   A black box that contains some form of knowledge about the world at large
#'   that may help speed up the optimization process. Effect of this parameter
#'   depends on the backend implementation. Currently, this is ignored by all
#'   backends.
#' @param savefile [\code{character(1)}]\cr
#'   Name of a file or folder in which intermediate progress will be saved.
#'   This is will prevent data getting lost in case of a crash. The data written
#'   is an \code{AMState} object in an \code{.rds} file that can be read and run
#'   with another \code{automlr} call to resume optimization.\cr
#'   If \code{savefile} ends with a forward slash (\code{/}), it is assumed to
#'   refer to a directory in which a new file will be created. Otherwise it is
#'   assumed to refer to a specific file name, in which case the file will be
#'   created or overwritten \emph{without warning}.\cr
#'   If the \code{task} argument is an \code{AMState} object, \code{savefile}
#'   will \emph{not} default to the \code{AMState}'s \emph{savefile} but must be
#'   supplied again; this is to prevent accidental file overwrites. If the first
#'   argument is a character, \code{savefile} defaults to \code{amstate} and
#'   therefore offers to seamlessly continue optimization runs.
#' @param backend [\code{character(1)}|\code{BackendOptions}]\cr
#'   Refers to the back end used for optimization. Currently implemented and
#'   provided by automlr are \code{"random"}, \code{"irace"} and \code{"mbo"}.
#'   To list all backends, run \code{\link{lsambackends}}.
#' @param save.interval [code{numeric(1)}]\cr
#'   The inteval, in seconds, in between which to save the intermediate result
#'   to \code{savefile}. Ignored if \code{savefile} is \code{NULL}; set to
#'   \code{0} to only save at the end of optimization runs.
#' @param new.seed [\code{logical(1)}]\cr
#'   If \code{TRUE}, the random seed saved in the AMState object will not be
#'   used; instead the RNG state at time of the invocation will be used.
#'   The default behaviour (\code{FALSE}) is to use the saved rng state so that
#'   invocations with the same AMState object give a more deterministic result
#'   (insofar as execution time does not influence behaviour).\cr
#'   \emph{Warning}: This is not yet tested and likely does not work with
#'   \code{Learner}s that use external RNGs.
#' @param max.walltime.overrun [\code{numeric(1)}]\cr
#'   Defines a time in seconds for the automlr runtime beyond the
#'   \code{walltime} budget after which a learner function will be killed.
#'   Since the walltime (and other) budget is only checked in certain stages of
#'   the evaluation, this can sometimes lead to run times far greater than the
#'   walltime budget. Setting \code{max.walltime.overrun} to a finite value will
#'   agressively kill learner runs, potentially throwing away intermediate
#'   progress already made. There may still be a few seconds overhead runtime,
#'   especially when the learner code runs into a C function that can not be
#'   interrupted.
#' @param max.learner.time [\code{numeric(1)}]\cr
#'   Maximum time that one combined \code{train()}-\code{predict()} evaluation
#'   of a learner may take after which it is aborted.
#' @param verbosity [\code{integer(1)}]\cr
#'   Level of warning and info messages which to show.
#'   \describe{
#'     \item{0}{Default: Only give essential warning messages and errors.}
#'     \item{>=1}{Output info about evaluated points.}
#'     \item{>=2}{Output memory usage stats.}
#'     \item{>=3}{Detailed warning messages about search space.}
#'     \item{>=4}{Detailed warning messages from learners.}
#'     \item{>=5}{Output from all learners.}
#'     \item{>=6}{Stop on learner error.}
#'   } 
#' @param ... No further arguments should be given.
#' 
#' @return [\code{AMState}]
#' Object containing the result as well as info about the run. Use
#' \code{\link{amfinish}} to extract the results.
#' 
#' Object members:
#' \describe{
#'   \item{task [\code{Task}]}{The task being trained for.}
#'   \item{measure [\code{Measure}]}{The measure for which is being ptimized.}
#'   \item{budget [\code{numeric}]}{The budget of the current run.}
#'   \item{spent [\code{numeric(4)}]}{The budget already spent.}
#'   \item{searchspace [list of \code{Autolearner}]}{The \code{Learner}s being
#'     considered for optimization.}
#'   \item{prior [any]}{The prior of the current run. If the backend supports
#'     this, the prior is being updated during a run and can be given to another
#'     \code{automlr} invocation as an argument.}
#'   \item{backend [\code{character(1)}]}{The backend of the optimization run.}
#'   \item{creation.time [\code{numeric(1)}]}{The time at which the object was
#'     created.}
#'   \item{finish.time [\code{numeric(1)}]}{The time at which the object was
#'     last touched by \code{automlr}; either by saving it on disk or by
#'     returning a result.}
#'   \item{previous.versions [list of \code{AMState}]}{Backlog of previous
#'     invocations of \code{automlr} using this object. The objects in this list
#'     are reduced instances of \code{AMState}.}
#'   \item{seed [\code{numeric}]}{The value of \code{.Random.seed} which to use
#'     for continuation.}
#'  }
#'     
#' 
#' @examples
#' \dontrun{
#' library(mlr)
#' # almost minimal invocation. Will save progress to './iris.rds'.
#' automlr(iris.task, budget = c(evals = 1000), backend = "random",
#'   savefile = "iris")
#' > SOME RESULT YOU GUYS
#' 
#' # optimize for another 1000 evaluations, loading the 'iris.rds' savefile
#' # automatically and saving back to it during evaluation.
#' automlr("iris", budget = c(evals = 2000))
#' > MORE RESULTS
#' }
#' 
#' @include mlrLearners.R lsambackends.R defaults.R
#' @export
automlr = function(task, ...) {
  UseMethod("automlr")
}

#' @title Create an \code{AMState} object and run automlr.
#' 
#' @rdname automlr
#' @export
automlr.Task = function(task, measure = NULL, budget = 0,
    searchspace = mlrLearners, prior = NULL, savefile = NULL,
    save.interval = default.save.interval, backend,
    max.walltime.overrun = if ("walltime" %in% names(budget))
      budget['walltime'] * 0.1 + 30 else 3600, max.learner.time = Inf,
    verbosity = 0, ...) {
  # Note: This is the 'canonical' function signature.
  assertClass(task, "Task")
  if (is.null(measure)) {
    measure = getDefaultMeasure(task)
  } else {
    assertClass(measure, "Measure")
  }
  if (testString(backend)) {
    if (is.null(registered.backend[[backend]])) {
      stopf(paste0("Backend '%s' not found.\n",
              "You can list available backends with lsambackends()."), backend)
    }
    backend = registered.backend[[backend]]()
    attr(backend, "automlr.backend.invocation") =
        attr(backend, "automlr.backend")
  }
  assertClass(backend, "AutomlrBackendConfig")
  budget = unlist(budget, recursive = FALSE)
  checkBudgetParam(budget)
  assertList(searchspace, types = "Autolearner", min.len = 1)
  # need at least one learner
  assert(any(extractSubList(searchspace, "stacktype") == "learner"))
  if (!is.null(savefile)) {
    assertString(savefile)
    assertNumber(save.interval, lower = 0)
  }
  assertNumeric(max.walltime.overrun, lower = 0, len = 1)
  assertNumeric(max.learner.time, lower = 0, len = 1)
  assertCount(verbosity)
  
  assert(identical(list(...), list()))
  # a delegated problem is a solved problem.
  automlr(makeS3Obj(c("AMState", "AMObject"),
          task = task,
          measure = coalesce(measure, getDefaultMeasure(task)),
          budget = budget,
          spent = c(walltime = 0, cputime = 0, modeltime = 0, evals = 0),
          searchspace = searchspace,
          prior = prior,
          backend = attr(backend, "automlr.backend"),
          backendoptions = backend,
          backendprivatedata = setClasses(
              new.env(parent = emptyenv()),
              paste0("am", attr(backend, "automlr.backend"))),
          seed = .Random.seed,
          creation.time = Sys.time(),
          finish.time = NULL,
          previous.versions = list(),
          isInitialized = FALSE,
          max.walltime.overrun = max.walltime.overrun,
          max.learner.time = max.learner.time),
      savefile = savefile, save.interval = save.interval, verbosity = verbosity)
}

#' @title Continue automlr search from an \code{.rds} savefile, given as a
#' \code{character(1)}.
#' 
#' @rdname automlr
#' @export
automlr.character = function(task, budget = NULL, prior = NULL, savefile = task,
    save.interval = default.save.interval, new.seed = FALSE, 
    max.walltime.overrun = NULL, verbosity = 0, ...) {
  assertString(task)
  truefilename = gsub("(\\.rds|)$", ".rds", task)
  assert(identical(list(...), list()))
  # yes, one could load an RDS file that contains a character(1) referring to
  # another RDS file...
  automlr(readRDS(truefilename),
      budget = budget,
      prior = prior,
      savefile = savefile,
      save.interval = save.interval,
      new.seed = new.seed,
      max.walltime.overrun = max.walltime.overrun,
      verbosity = verbosity)
}


#' @title Continue automlr search the result of a previous \code{automlr} run.
#' 
#' @rdname automlr
#' @export
automlr.AMState = function(task, budget = NULL, prior = NULL, savefile = NULL,
    save.interval = default.save.interval, new.seed = FALSE,
    max.walltime.overrun = NULL, verbosity = 0, ...) {
  if (!is.null(budget)) {
    budget = unlist(budget, recursive = FALSE)
    checkBudgetParam(budget)
  }
  if (!is.null(savefile)) {
    assertString(savefile)
    assertNumber(save.interval, lower = 0)
  }
  if (!is.null(max.walltime.overrun)) {
    assertNumeric(max.walltime.overrun, lower = 0, len = 1)
  }
  assertCount(verbosity)
  assertFlag(new.seed)
  assert(identical(list(...), list()))
  aminterface(task, budget, prior, savefile, save.interval, new.seed,
      max.walltime.overrun, verbosity)
}

#' @title Converte the \code{AMState} object as returned by
#'   \code{\link{automlr}} to an \code{AMResult} object.
#' 
#' @description
#' The result object contains information about the solution that is relatively
#' backend-independent.
#' 
#' @param amstate [\code{AMState}]\cr
#'   The AMState object which is to be converted.
#' 
#' @return [\code{AMResult}]
#' Object representing the optimum found by the \code{\link{automlr}} run.
#' 
#' Object members:
#' \describe{
#'   \item{learner [\code{Learner}]}{The (constructed) learner that achieved the
#'     optimum.}
#'   \item{opt.point [\code{list}]}{Optimal hyperparameters for learner.}
#'   \item{opt.val [\code{numeric}]}{Optimum reached for the \code{AMState}'s
#'     \code{measure}.}
#'   \item{opt.path [\code{OptPath}]}{Information about all the evaluations
#'     performed.}
#'   \item{result [any]}{Furthe backend-dependent information about the
#'     optimum.}
#'   \item{... (further elements)}{Elements of the \code{AMState} object.}
#'  }
#' 
#' @export
amfinish = function(amstate) {
  assertClass(amstate, "AMState")
  if (!amstate$isInitialized) {
    message("No optimization has been done. Empty result.")
    return(NULL)
  }
  amstate = insert(amstate, amresult(amstate$backendprivatedata))
  # inserts:
  # $opt.val, $opt.point, $opt.path
  amstate$backendprivatedata = NULL
  class(amstate) = c("AMResult", "AMObject")
  amstate
}

#' @title Give some cute info about a given AMState
#' 
#' @description
#' Optionally give a little or a lot (if \code{verbose == TRUE}) of info.
#' 
#' @param x [\code{AMState}|\code{AMResult}]\cr
#'   What to print
#' @param verbose [\code{logical(1)}]\cr
#'   Print detailed info
#' @param ... ignored
#' 
#' @method print AMObject
#' 
#' @export
print.AMObject = function(x, verbose = FALSE, ...) {
  allversions = c(x$previous.versions, list(x))
  catf("automlr %s.\nBackend: %s",
      ifelse("AMState" %in% class(x), "optimization state", "result"),
      x$backend)
  if ("AMResult" %in% class(x)) {
    catf("Optimum %s found: %f", x$measure$id, x$opt.val)
    print(x$opt.point)
  }
  if (verbose) {
    catf("First created: %s\nLast finished: %s",
        allversions[[1]]$creation.time, x$finish.time)
    cat("Total budget:\n")
    print(x$budget)
    cat("Total spent:\n")
    print(x$spent)
    if (length(allversions) > 1) {
      cat("All invocations using this object:\n")
      # the following does a few gymnastics with do.call(c, ...)
      # to keep the POSIXct type.
      allversionsdf = data.frame(invocation.time = unlist(
              extractSubList(allversions, "creation.time", simplify = FALSE),
              recursive = FALSE),
          return.time = unlist(
              extractSubList(allversions, "finish.time", simplify = FALSE),
              recursive = FALSE))
      spentmatrix = t(sapply(allversions,
              function(v) v$spent[names(x$spent)]))
      budgetmatrix = t(sapply(allversions,
              function(v) v$budget[names(x$spent)]))
      colnames(spentmatrix) = paste("sp", names(x$spent), sep = ".")
      colnames(budgetmatrix) = paste("bg", names(x$spent), sep = ".")
      print(cbind(allversionsdf, budgetmatrix, spentmatrix))
    }
    cat("*****\nMeasure:\n")
    print(x$measure)
    cat("*****\nTask:\n")
    print(x$task)
    cat("*****\n")
  }
}