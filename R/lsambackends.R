
#' List the backends currently implemented which can be passed to \code{\link{automlr}}.
#' 
#' @param fenv should be NULL
#' @examples
#' lsambackends()
#' @export
lsambackends = function(fenv=NULL) {
  # TODO: maybe this weird recursion listing is not a good idea
  if (is.null(fenv)) {
    fenv = parent.env(environment())
  }
  if (identical(emptyenv(), fenv)) {
    return(character(0))
  }
  results = NULL
  for (rf in requiredBackendFunctions) {  # as defined in defaults.R
    pat = paste0("^", rf, "\\.am")
    matches = sub(pat, "", ls(fenv, pattern=pat))
    if (is.null(results)) {
      results = matches
    } else {
      results = intersect(matches, results)
    }
  }
  c(results, lsambackends(parent.env(fenv)))
}

#' Check if the given name is a valid automlr backend
#' 
#' @param name the name of the backend to check for.
#' @export
isambackend = function(name) {
  searchstrings = paste(requiredBackendFunctions, name, sep=".am")
  !any(sapply(mget(searchstrings, inherits=TRUE, mode="function",
                   ifnotfound=replicate(length(requiredBackendFunctions), NULL)),
              is.null))
}