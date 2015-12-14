#' Automatically choose a model with parameters to fit.
#' 
#' This is the main entry point of automlr.
#' 
#' @param task Either: the mlr Task object to fit a model on. Or: An \code{AMState} object, or a character(1)
#'        containing the file name of an \code{.rds} file containing
#'        such an object. The AMState usually contains progress of a past optimization run that was aborted by the
#'        user, a crash, or which ran out of budget.
#' @param measure the mlr Measure object to optimize for. If this is not given and the first argument is a
#'        Task object, uses the task's default measure. If this is not given and the first argument is an AMState object
#'        or a character referring to an AMState rds file, the measure of the AMState is used.
#' @param budget a list or named vector with one or several of the entries \code{walltime} (time since invocation),
#'        \code{cputime} (total cpu time of optimization process), \code{modeltime} (time spent executing model fits), 
#'        \code{evals} (number of model fit evaluations). Time is always given in seconds. When any of the budget criteria
#'        is exceeded, the optimization process will halt at the next possible point and return. In the current
#'        implementation, this is only checked between model fits and evaluations, so the time budgets may be exceeded,
#'        in some cases, substantially. If the first argument is an AMState object or a character referring to an
#'        AMState rds file, this is optional and defaults to the AMState's budget \emph{minus the already used up
#'        budget}. To continue an already finished run, therefore, one needs to pass a higher budget than during the
#'        previous run. Passing 0 will return an AMState object without performing any optimization or touching the
#'        file system.\\TODO ideally the budget should have an influence in the backends behaviour, so that it tries
#'        to do many cheap, parallel evaluations if only \code{walltime} restriction is given, or few high information
#'        gain evaluations if only \code{evals} restriction is given. If this ever happens, we may want to have an
#'        additional parameter that has an influence on this orthogonal to providing a budget.
#' @param searchspace a list of \code{Autolearner} objects which mainly wrap mlr \code{Learner} objects (see
#'        \code{\link{autolearner}}. The \code{par.set} attribute will define the
#'        hyperparameter search space for the individual learners. If not supplied, \code{\link{autolearners}} will
#'        be used for the Task S3-method, and the AMState's searchspace will be used for the AMState and character S3
#'        method. It is thus possible to expand or restrict the search space on subsequent calls to \code{automlr()};
#'        if the models in a restricted search space perform less well than a previously found model, however, the
#'        previous model not in the new search space any more may well be returned as an optimum.\\It is recommended 
#'        to use a subset of \code{\link{autolearners}}. Only learners that fit the task type
#'        will be used, the others will be ignored without notification.
#' @param prior a black box that contains some form of knowledge about the world at large that may help speed
#'        up the optimization process. Effect of this parameter depends on the backend implementation.
#' @param savefile A file in which intermediate progress will be saved. This is will prevent date getting
#'        lost in case of a crash. The data written is an \code{AMState} object in an \code{.rds} file that can be
#'        read and run with \code{automlr()} to resume optimization.\\This may refer to a specific file name, in which
#'        case the file will be created or overwritten \emph{without warning}, or it may refer to a directory (ending
#'        with a '/'), in which
#'        case a unique filename will be created that is guaranteed not to overwrite other files written by other
#'        automlr processes, if such guarantee can be provided by the file system.\\If the first argument is an
#'        \code{AMState} object, \code{savefile} will \emph{not} default to the \code{AMState}'s \emph{savefile}
#'        but must be supplied again; this is to prevent accidental file overwrites. If the first argument
#'        is a character, \code{savefile} defaults to \code{amstate} and therefore offers to seamlessly continue
#'        optimization runs.
#' @param backend A character(1) referring to the back end used for optimization. Must be one of \code{\link{lsambackends}}
#'        results.
#' @param save.interval the inteval, in seconds, in between which to save the intermediate result to \code{savefile}.
#' @param new.seed if TRUE, the random seed saved in the AMState object will not be used; instead the rng state at time
#'        of the invocation will be used. The default behaviour (FALSE) is to use the saved rng state so that invocations
#'        with the same AMState object give a more deterministic result (insofar as execution time does not influence
#'        behaviour).
#' @param ... I don't know how to get rid of this warning, therefore I'm documenting the ellipsis here. TODO
#'        there has to be a different way around this.
#' @return AMState object containing the result as well as info about the run.
#' 
#' @examples
#' \dontrun{
#' library(mlr)
#' # almost minimal invocation. Will save progress to './iris.rds'.
#' automlr(iris.task, budget=c(evals=1000), backend="random", savefile="iris")
#' > SOME RESULT YOU GUYS
#' 
#' # optimize for another 1000 evaluations, loading the 'iris.rds' savefile
#' # automatically and saving back to it during evaluation.
#' automlr("iris", budget=c(evals=2000))
#' > MORE RESULTS, THE WORLD IS BEAUTIFUL
#' }
#' 
#' @include autolearners.R lsambackends.R defaults.R
#' @export
automlr = function(task, ...) {
  UseMethod("automlr")
}

#' Create an \code{AMState} object and run automlr.
#' 
#' @rdname automlr
#' @export
automlr.Task = function(task, measure=NULL, budget=0, searchspace=autolearners, prior=NULL, savefile=NULL,
                        save.interval=default.save.interval, backend, ...) {
  # Note: This is the 'canonical' function signature.
  automlr(makeS3Obj(c("AMState", "AMObject"),
                    task=task,
                    measure=coalesce(measure, getDefaultMeasure(task)),
                    budget=budget,
                    spent=c(walltime=0, cputime=0, modeltime=0, evals=0),
                    searchspace=searchspace,
                    prior=prior,
                    backend=backend,
                    backendprivatedata=setClasses(new.env(parent=emptyenv()), backend),
                    seed=.Random.seed,
                    creation.time=Sys.time(),
                    finish.time=NULL,
                    previous.versions=list()),
          savefile=savefile, save.interval=save.interval)  # a delegated problem is a solved problem.
}

#' Continue automlr search from an \code{.rds} savefile, given as a character.
#' 
#' @rdname automlr
#' @export
automlr.character = function(task, budget=NULL, searchspace=NULL, prior=NULL, savefile=task,
                             save.interval=default.save.interval, new.seed=FALSE, ...) {
  truefilename = gsub('(\\.rds|)$', '.rds', task)
  # yes, one could load an RDS file that contains a character(1) referring to another RDS file...
  automlr(readRDS(truefilename),
          budget=budget,
          searchspace=searchspace,
          prior=prior,
          savefile=savefile,
          save.interval=save.interval,
          new.seed=new.seed)
}

#' Continue automlr search from saved \code{AMState} object.
#' 
#' @rdname automlr
#' @export
automlr.AMState = function(task, budget=NULL, searchspace=NULL, prior=NULL, savefile=NULL,
                           save.interval=default.save.interval, new.seed=FALSE, ...) {
  # TODO: maybe check there's nothing in the '...'.
  aminterface(task, budget, searchspace, prior, savefile, save.interval, new.seed)
}

#' Extract a prior from an AMState or AMResult object
#' 
#' The extracted prior can be used as an argument to further calls of \code{\link{automlr}}.
#' @param amstate The AMState or AMResult object from which to extract the prior.
#' @export
extractprior = function(amstate) {
  UseMethod("extractprior")
}

#' @rdname extractprior
#' @export
extractprior.AMState = function(amstate) {
  newprior = amgetprior(amstate$backendprivatedata)
  if (is.null(amstate$prior)) {
    newprior
  } else {
    amcombinepriors(amstate$backendprivatedata, amstate$prior, newprior)
  }
}

#' @rdname extractprior
#' @export
extractprior.AMResult = function(amstate) {
  amstate$newprior
}

#' Converte the AMState object as returned by \code{\link{automlr}} to a result object.
#' 
#' The result object contains information about the solution that is relatively backend-independent.
#' @param amstate The AMState object which is to be converted.
#' @export
amfinish = function(amstate) {
  amstate$newprior = extractprior(amstate)
  amstate = insert(amstate, amresult(amstate$backendprivatedata))
  class(amstate) = c("AMResult", "AMObject")
  amstate
}

#' Give some cute info about a given AMState
#' @param x what to print. WTF I have to do this to get rid of R CMD check warnings?
#' @param ... See \code{x}.
#' @method print AMObject
#' @export
print.AMObject = function(x, ...) {
  allversions = c(x$previous.versions, list(x))
  catf("automlr %s.\nBackend: %s", ifelse("AMState" %in% class(x), "optimization state", "result"), x$backend)
  catf("First created: %s\nLast finished: %s", allversions[[1]]$creation.time, x$finish.time)
  cat("Total budget:\n")
  print(x$budget)
  cat("Total spent:\n")
  print(x$spent)
  if (length(allversions) > 1) {
    cat("All invocations using this object:\n")
    # the following does a few gymnastics with do.call(c, ...) to keep the POSIXct type.
    allversionsdf = data.frame(invocation.time=do.call(c, extractSubList(allversions, "creation.time", simplify=FALSE)),
                               return.time=do.call(c, extractSubList(allversions, "finish.time", simplify=FALSE)))
    spentmatrix = t(sapply(allversions, function(v) v$spent[names(x$spent)]))
    budgetmatrix = t(sapply(allversions, function(v) v$budget[names(x$spent)]))
    colnames(spentmatrix) = paste("sp", names(x$spent), sep=".")
    colnames(budgetmatrix) = paste("bg", names(x$spent), sep=".")
    print(cbind(allversionsdf, budgetmatrix, spentmatrix))
  }
  cat("Measure: *****\n")
  print(x$measure)
  cat("*****\nTask: *****\n")
  print(x$task)
  cat("*****\n")
  if ("AMResult" %in% class(x)) {
    cat(x$resultstring)
    cat("\n")
  }
}