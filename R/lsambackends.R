
#' List the backends currently implemented which can be passed to \code{\link{automlr}}.
#' 
#' @examples
#' lsambackends()
#' @export
lsambackends = function() {
  requiredFunctions = c("combinepriors", "extractprior", "setup", "optimize", "result")
  results = NULL
  for (rf in requiredFunctions) {
    pat = paste0("^", rf, "\\.")
    matches = sub(pat, "", ls(pattern=pat))
    if (is.null(results)) {
      results = matches
    } else {
      results = intersect(matches, results)
    }
  }
  matches
}