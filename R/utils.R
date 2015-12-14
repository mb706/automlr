# Small functions that have no place elsewhere

# return 'budget' - 'spent', respecting the budget==0 special case
remainingbudget = function(budget, spent) {
  if (budget == 0) {  # the special case in which budget is unnamed vector with value 0.
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
  # TODO: handle extra
  # TODO: check equality of par.set etc.
  for (vect in c("error.message", "exec.time", "dob", "eol")) {
    op1$env[[vect]] = c(op1$env[[vect]], op2$env[[vect]])
  }
  op1$env$path = rbind(op1$env$path, op2$env$path)
}