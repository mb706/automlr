# Small functions that have no place elsewhere

# syntactic sugar

`%+=%` = function(t, s) eval.parent(substitute(t <- t + s))
`%-=%` = function(t, m) eval.parent(substitute(t <- t - m))

# return 'budget' - 'spent', respecting the budget==0 special case
remainingbudget = function(budget, spent) {
  # the special case in which budget is unnamed vector with value 0.
  if (length(budget) == 1 && budget == 0) {
    return(0)
  }
  b = budget[names(spent)] - spent
  b[!is.na(b)]
}

# true if budget <= spent
stopcondition = function(budget, spent) {
  # if budget is 0, it may be an unnamed vector; we check for this separately
  any(remainingbudget(budget, spent) <= 0)
}

deepcopy = function(obj) {
  unserialize(serialize(obj, NULL))
}

checkfile = function(filename, basename) {
  assert(nchar(filename) > 0)
  if (substring(filename, 1, 1) != "/") {
    filename = paste0("./", filename)
  }
  givenAsDir = substring(filename, nchar(filename)) == "/"
  if (file.exists(paste0(filename, "/"))) {
    if (!givenAsDir) {
      stopf(paste0("Target file '%s' is a directory. To create a file inside ",
              " the directory, use '%s/' (trailing slash)."),
          filename, filename)
    }
    basepath = filename
    filename = tempfile(paste0(basename, "_"), basepath, ".rds")
    messagef("Will be saving to file %s", filename)
  } else {
    if (givenAsDir) {
      stopf(paste0("Directory '%s' does not exist. If you want to write to it ",
              "as a FILE, remove the trailing '/'."),
          filename)
    }
  }
  filename
}

# write 'object' to file 'filename'. if filename ends with a '/', it is assumed
# to refer to a directory in which the file should be created using name
# 'basename', postfixed with a possible postfix to avoid collision and '.rds'.
writefile = function(filename, object, basename) {
  basepath = dirname(filename)
  if (basepath == "") {
    # to ensure 'tempfile' doesnt give something in the root directory.
    basepath = "."
  }
  outfile = tempfile(paste0(basename, "_"), basepath, ".rds")
  saveRDS(object, outfile)
  file.rename(outfile, filename)
  invisible()
}

# append opt path op2 to opt path op1. This happens in-place.
appendOptPath = function(op1, op2) {
  # FIXME: handle extra: is a LIST
  # FIXME: check equality of par.set etc.
  for (vect in c("error.message", "exec.time", "dob", "eol")) {
    op1$env[[vect]] = c(op1$env[[vect]], op2$env[[vect]])
  }
  op1$env$path = rbind(op1$env$path, op2$env$path)
}

# copied this from mlr logFunOpt.R
# we give info about everything except memory, since reporting that takes lots
# of time
# FIXME: this is obsolete as soon as I do that PR for mlr.
logFunDefault = function(learner, task, resampling, measures, par.set, control,
    opt.path, dob, x.string, y, remove.nas, stage, prev.stage, prefixes) {
  if (stage == 1L) {
    start.time = Sys.time()
    messagef("[%s] %i: %s", prefixes[stage], dob, x.string)
    return(list(start.time = start.time))
  } else if (stage == 2L) {
    end.time = Sys.time()
    diff.time = difftime(time1 = end.time,
        time2 = prev.stage$start.time, units = "mins")
    messagef("[%s] %i: %s; time: %.1f min",
        prefixes[stage], dob, perfsToString(y), diff.time[[1]])
    return(NULL)
  }
}

# similarly copied from mlr logFunOpt.R
logFunTune = function(learner, task, resampling, measures, par.set, control,
    opt.path, dob, x, y, remove.nas, stage, prev.stage) {
  
  x.string = paramValueToString(par.set, x, show.missing.values = !remove.nas)
  # shorten tuning logging a bit. we remove the sel.learner prefix from params
  if (inherits(learner, "ModelMultiplexer"))
    x.string = gsub(paste0(x$selected.learner, "\\."), "", x.string)
  
  logFunDefault(learner, task, resampling, measures, par.set, control, opt.path,
      dob, x.string, y, remove.nas, stage, prev.stage,
      prefixes = c("Tune-x", "Tune-y"))
}

# similarly copied from mlr logFunOpt.R
logFunQuiet = function(learner, task, resampling, measures, par.set, control,
    opt.path, dob, x, y, remove.nas, stage, prev.stage) {
  
  if (stage == 1L) {
    list(start.time = Sys.time())
  }
}

# copied from mlr helpers.R
perfsToString = function(y) {
  collapse(paste(names(y), " = ", formatC(y, digits = 3L), sep = ""), sep= ",")
}

# make extractSubList commute with c():
# extractSubList(c(a, b), e) == c(extractSubList(a, e), extractSubList(b, e))
extractSubList = function(xs, element, element.value, simplify = TRUE,
    use.names = TRUE) {
  res = BBmisc::extractSubList(xs, element, element.value, simplify, use.names)
  if (simplify && is.list(res) && length(res) == 0) {
    # don't return an empty list
    logical(0)
  } else {
    res
  }
}

checkBudgetParam = function(budget) {
  if (!identical(budget, 0) && !identical(budget, 0L)) {
    assertNamed(budget)
    assertNumeric(budget, lower = 0, min.len = 1, max.len = 4)
    legalnames = c("walltime", "cputime", "evals", "modeltime")
    budgetNamesOk = names(budget) %in% legalnames
    assert(all(budgetNamesOk))
  }
}

amlrTransformName = function(name) {
  sub("\\.AMLRFIX[0-9]+$", "", name)
}

generateRealisticImputeVal = function(measure, learner, task) {
  naked = dropFeatures(task, getTaskFeatureNames(task))
  retval = bootstrapB632(learner, naked, iters = 100, show.info = FALSE)$aggr
  # and because convertYForTuner is retarded:
  retval * ifelse(measure$minimize, 1 , -1)
}

# take a language object (call or expression), turn it into a call
deExpression = function(language) {
  if (is.null(language)) {
    return(NULL)
  }
  if (is.expression(language) && length(language) == 1) {
    language = language[[1]]
  }
  if (is.call(language)) {
    return(language)
  }
  substitute(eval(x), list(x = language))
}

patchMlrPredict = function() {
# The following function is a slightly modified version of the
# predictLearner.ModelMultiplexer function in the mlr project
# (https://github.com/mlr-org/mlr).
#
# This file was distributed with a BSD 2 clause license as follows.
#
# Copyright (c) 2013-2016, Bernd Bischl
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
# 
#     Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in
#     the documentation and/or other materials provided with the
#     distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  myAssignInNamespace("predictLearner.ModelMultiplexer", ns = "mlr",
      function(.learner, .model, .newdata, ...) {
        # simply predict with the model
        sl = .learner$par.vals$selected.learner
        bl = .learner$base.learners[[sl]]
        # we need to pass the changed setting of the base learner for the
        # predict function further down
        args = list(.learner = bl, .model = .model$learner.model$next.model,
            .newdata = .newdata)
        args = c(args, getHyperPars(bl, for.fun = c("predict", "both")))
        do.call(predictLearner, args)
      })
}

unpatchMlr = function() {
  myAssignInNamespace("predictLearner.ModelMultiplexer", ns = "mlr",
      mlr.predictLearner.ModelMultiplexer)
}

# whether to output optimization trace info
verbosity.traceout = function(verbosity) {
  verbosity >= 1
}

#whether to output memory info
verbosity.memtraceout = function(verbosity) {
  verbosity >= 2
}

# whether to output detailed search space warnings
verbosity.sswarnings = function(verbosity) {
  verbosity >= 3
}

# whether to output learner warnings
verbosity.learnerwarnings = function(verbosity) {
  verbosity >= 4
}

# whether to give learner output
verbosity.learneroutput = function(verbosity) {
  verbosity >= 5
}

# stop on learner error
verbosity.stoplearnerror = function(verbosity) {
  verbosity >= 6
}


# getLearnerOptions without polluting the result with getMlrOptions()
getLLConfig = function(learner) {
  if (inherits(learner, "BaseWrapper")) {
    getLLConfig(learner$next.learner)
  } else {
    as.list(learner$config)
  }
}

# setLearnerOptions, basically
setLLConfig = function(learner, config) {
  if (identical(getLLConfig(learner), config)) {
    # avoid too much copy-on-write action for nothing
    learner
  } else {
    (function(l) {
        if (inherits(l, "BaseWrapper")) {
          l$next.learner = Recall(l$next.learner)
        } else {
          l$config = config
        }
        l
      })(learner)
  }
}

adjustLearnerVerbosity = function(learner, verbosity) {
  config = getLLConfig(learner)
  # show.info is not used, but in case this changes at some point...
  config$show.show.info = verbosity.learneroutput(verbosity)
  config$on.learner.error = if (verbosity.stoplearnerror(verbosity))
      "stop"
    else if (verbosity.learnerwarnings(verbosity))
      "warn"
    else
      "quiet"
  config$on.learner.warning = if (verbosity.learnerwarnings(verbosity))
      "warn"
    else
      "quiet"
  config$show.learner.output = verbosity.learneroutput(verbosity)
  setLLConfig(learner, config)
}

# return the value of `varname` within the function named `fname`. Use the most
# recent invocation of `fname` if names collide.
# Returns NULL if the function was not found.
getFrameVar = function(fname, varname) {
  # can not call getFrameNo, because then the last call will be in the list also.
  calls = sys.calls()
  calls[[length(calls) - 1]] = NULL
  callnames = sapply(calls,
      function(x) try(as.character(x[[1]]), silent = TRUE))
  frameno = tail(which(callnames == fname), n = 1)
  if (length(frameno) < 1) {
    return(NULL)
  }
  sys.frame(frameno)[[varname]]
}

getFrameNo = function(fname) {
  calls = sys.calls()
  calls[[length(calls) - 1]] = NULL
  callnames = sapply(calls,
      function(x) try(as.character(x[[1]]), silent = TRUE))
  tail(which(callnames == fname), n = 1)
}

# assign the value of `varname` within the function named `fname`. Use the most
# recent invocation of `fname` if names collide.
# Returns TRUE if successful, FALSE if the function `fname` was not in the call
# stack.
assignFrameVar = function(fname, varname, value) {
  calls = sys.calls()
  calls[[length(calls) - 1]] = NULL
  callnames = sapply(calls,
      function(x) try(as.character(x[[1]]), silent = TRUE))
  frameno = tail(which(callnames == fname), n = 1)
  if (length(frameno) < 1) {
    return(FALSE)
  }
  assign(varname, value, sys.frame(frameno))
  TRUE
}


isInsideResampling = function() {
  !is.null(getResampleIter())
}

getResampleIter = function() {
  frameno = getFrameNo('train')
  if (length(frameno) < 1) {
    return(NULL)
  }
  sys.frame(frameno - 1)[['i']]
}

getResampleMaxIters = function() {
  frameno = getFrameNo('train')
  if (length(frameno) < 1) {
    return(NULL)
  }
  sys.frame(frameno - 1)[['rin']]$desc$iters
}

# test whether mlr parallelizes resample() calls. This does NOT entail that
# isInsideResampling()!
isResampleParallel = function() {
  pmlevel = parallelGetOptions()$level
  !is.null(pmlevel) && pmlevel == "mlr.resample"
}

isFirstResampleIter = function() {
  rin = getResampleIter()
  if (is.null(rin)) {
    stop("'doResampleIteration' not found in call stack when it was expected.")
  }
  rin == 1
}

# assign functions in locked namespaces. This is the same mechanism that R
# trace() uses.
myAssignInNamespace = function(what, value, ns) {
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

#' @title Retrieve a suggested search space of the given learner
#' 
#' @param learner [\code{Learner}]\cr
#'   Learner
#' @export
getSearchspace = function(learner) {
  UseMethod("getSearchspace")
}

getSearchspace.BaseWrapper = function(learner) {
  getSearchspace(learner$next.learner)
}
