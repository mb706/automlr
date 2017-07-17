
#' @title List the backends currently implemented which can be passed to
#' \code{\link{automlr}}.
#' 
#' @description
#' This lists all the backends that can be used by \code{\link{automlr}}, even
#' user defined ones.
#' 
#' @param fenv [\code{NULL}]\cr
#'   Should be NULL
#' 
#' @examples
#' lsambackends()
#' 
#' @export
lsambackends = function(fenv = NULL) {
  # TODO: check registered.backend.
  # FIXME: maybe this weird recursion listing is not a good idea
  if (is.null(fenv)) {
    fenv = parent.env(environment())
  }
  if (identical(emptyenv(), fenv)) {
    return(character(0))
  }
  results = NULL
  for (rf in requiredBackendFunctions) {  # as defined in defaults.R
    pat = paste0("^", rf, "\\.am")
    matches = sub(pat, "", ls(fenv, pattern = pat))
    if (is.null(results)) {
      results = matches
    } else {
      results = intersect(matches, results)
    }
  }
  c(results, lsambackends(parent.env(fenv)))
}

#' @title Check if the given name is a valid automlr backend
#' 
#' @description
#' Test whether whether all necessary functions for a backend are implemented.
#' 
#' @param name [\code{character(1)}]\cr
#'   Name the name of the backend to check for.
#' @export
isambackend = function(name) {
  searchstrings = paste(requiredBackendFunctions, name, sep = ".am")
  !any(sapply(mget(searchstrings, inherits = TRUE, mode = "function",
              ifnotfound = replicate(length(requiredBackendFunctions), NULL)),
          is.null))
}