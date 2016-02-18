# Small functions that have no place elsewhere

# return 'budget' - 'spent', respecting the budget==0 special case
remainingbudget = function(budget, spent) {
  if (length(budget) == 1 && budget == 0) {  # the special case in which budget is unnamed vector with value 0.
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

# write 'object' to file 'filename'. if filename ends with a '/', it is assumed
# to refer to a directory in which the file should be created using name 'basename', 
# postfixed with a possible postfix to avoid collision and '.rds'. 
writefile = function(filename, object, basename) {
  basepath = dirname(filename)
  if (basepath == "") {  # to ensure 'tempfile' doesnt give something in the root directory.
    basepath = "."
  }
  outfile = tempfile(paste0(basename, '_'), basepath, ".rds")
  saveRDS(object, outfile)
  
  if (substring(filename, nchar(filename)) == "/") {
    # TODO: if there is some way to atomically create a file only if it does not already exist,
    #  we could iteratively try to create <basename>_<n>.rds for n = 1, 2, 3, ...
    #  Instead, the current implementation just uses the tempfile() R function result.
    filename = outfile
  } else {
    file.rename(outfile, filename)
  }
  filename
}

# append opt path op2 to opt path op1. This happens in-place.
appendOptPath = function(op1, op2) {
  # TODO: handle extra: is a LIST
  # TODO: check equality of par.set etc.
  for (vect in c("error.message", "exec.time", "dob", "eol")) {
    op1$env[[vect]] = c(op1$env[[vect]], op2$env[[vect]])
  }
  op1$env$path = rbind(op1$env$path, op2$env$path)
}

# copied this from mlr logFunOpt.R
# we give info about everything except memory, since reporting that takes lots of time
logFunDefault = function(learner, task, resampling, measures, par.set, control, opt.path, dob,
      x.string, y, remove.nas, stage, prev.stage, prefixes) {
  if (stage == 1L) {
    start.time = Sys.time()
    messagef("[%s] %i: %s", prefixes[stage], dob, x.string)
    return(list(start.time = start.time))
  } else if (stage == 2L) {
    end.time = Sys.time()
    diff.time = difftime(time1 = end.time, time2 = prev.stage$start.time, units = "mins")
    messagef("[%s] %i: %s; time: %.1f min",
              prefixes[stage], dob, perfsToString(y), diff.time[[1]])
    return(NULL)
  }
}

# similarly copied from mlr logFunOpt.R
logFunTune = function(learner, task, resampling, measures, par.set, control, opt.path, dob,
      x, y, remove.nas, stage, prev.stage) {
  
  x.string = paramValueToString(par.set, x, show.missing.values = !remove.nas)
  # shorten tuning logging a bit. we remove the sel.learner prefix from params
  if (inherits(learner, "ModelMultiplexer"))
    x.string = gsub(paste0(x$selected.learner, "\\."), "", x.string)
  
  logFunDefault(learner, task, resampling, measures, par.set, control, opt.path, dob,
          x.string, y, remove.nas, stage, prev.stage, prefixes = c("Tune-x", "Tune-y")
  )
}

# copied from mlr helpers.R
perfsToString = function(y) {
  paste(paste(names(y), "=", formatC(y, digits = 3L), sep = ""), collapse = ",")
}

extractSubList = function(xs, element, element.value, simplify = TRUE, use.names = TRUE) {
  res = BBmisc::extractSubList(xs, element, element.value, simplify, use.names)
  if (simplify && is.list(res) && length(res) == 0) {  # don't return an empty list
    return(logical(0))
  }
  res
}

checkBudgetParam = function(budget) {
  if (!identical(budget, 0) && !identical(budget, 0L)) {
    assertNamed(budget)
    assertNumeric(budget, lower=0, min.len=1, max.len=4)
    assert(all(names(budget) %in% c("walltime", "cputime", "evals", "modeltime")))
  }
}

amlrTransformName = function(name) {
  sub("\\.AMLRFIX[0-9]+$", "", name)
}