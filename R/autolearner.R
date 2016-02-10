#' Create \code{Autolearner} objects
#' 
#' \code{Autolearner} objects wrap mlr \code{Learner} objects as well as preprocessing functions
#' and provide additional
#' meta information. This is used to define the automlr searchspace.
#' @param learner An mlr Learner object or similar, depending on stacktype. May also be the ID of
#'        learner (postpones allocation of memory and loading of packages and may therefore be
#'        preferred.
#' @param searchspace a \code{ParamSet} object reqpresenting the relevant parameter search space.
#' @param stacktype character(1) describing how this object can be connected with other learners.
#' Must be one of \code{requiredwrapper} (e.g. feature selection), \code{learner}, \code{wrapper}
#' (is optionally combined with another learner, e.g. bagging)
#' @export
autolearner = function(learner, searchspace=list(), stacktype="learner") {
  
  assertChoice(stacktype, c("learner", "wrapper", "requiredwrapper"))
  names = extractSubList(searchspace, "name")
  if (any(duplicated(names))) {
    stopf("Duplicated names %s for learner '%s'", paste(unique(names[duplicated(names)]), collapse=", "),
        if(is.character(learner)) learner else learner$id) 
  }
  makeS3Obj("Autolearner",
            learner=learner,
            searchspace=searchspace,
            stacktype=stacktype)
}

#' @export
wrapper = function(name, constructor, conversion) {
  assertFunction(constructor)
  assertFunction(conversion, nargs=1)
  
  input = c("factors", "ordered", "numerics", "missings")
  output = c(input, "")
  for (inp in c("factors", "ordered", "numerics", "missings")) {
    assert(all(conversion(inp) %in% output))
  }
  
  list(name=name, constructor=constructor, conversion=conversion)
}

#' @export
print.Autolearner = function(x, ...) {
  cat(sprintf("<automlr learner '%s'>\n", ifelse(is.character(x$learner), x$learner, x$learner$id)))
}


#' Define the searchspace parameter in a short form
#' 
#' @param name the name of the parameter, must match the id of the \code{Param} it refers to.
#' @param values if \code{type} is \code{real} or \code{int}, it gives the lower and upper bound.
#'        if \code{type} is \code{cat}, it is a character vector of possible values. If \code{type}
#'        is \code{fix}, only one value (the one to be fixed) must be given. If \code{type} is
#'        code{bool} or \code{def}, it is ignored.
#' @param type the type of the parameter: \code{real} (numeric), \code{int} (integer), \code{cat}
#'        (discrete), \code{bool} (logical), \code{fix} (fixed value of whatever type), \code{def}
#'        using default value / not setting the value.
#' @param trafo may be "exp" for exponential transformation. Transforms integer parameters in a 
#'        smart way. Only applies for \code{real} and \code{int} values. May also be an R function.
#' @param id may be given to identify parameters of different learners having the same function.
#' @param dummy this is a dummy variable that has no equivalent in the learners searchspace.
#' @param req A requirement for the variable to have effect
#' @param dim the number of dimensions of this variable
sp = function(name, type="real", values=NULL, trafo=NULL, id=NULL, dummy=FALSE, req=NULL, dim=1) {
  assertChoice(type, c("real", "int", "cat", "bool", "fix", "def"))

  assertString(name)
  assert(nchar(name) > 0)

  if (type %in% c("real", "int")) {
    assertNumeric(values, any.missing=FALSE, len=2)
    assert(values[2] > values[1])
    if (type == "int") {
      assert(all(as.integer(values) == values))
      values = as.integer(values)
    }
  } else if (type %in% c("fix", "def")) {
    is.null(values) || assertVector(values, strict=TRUE, len=1)
  } else if (type == "cat"){
    assertVector(values, strict=TRUE, min.len=1)
  } else {  # type == "bool"
    assertNull(values)
  }

  if (!is.null(trafo) && !identical(trafo, "exp")) {
    assertFunction(trafo, nargs=1)
  }

  if (!is.null(id)) {
    assertString(id)
    assert(type %nin% c("fix", "def"))
    assert(nchar(id) > 0)
  }

  assertLogical(dummy, len=1)
  if (!is.null(req)) {
    assert(checkClass(req, "call"), checkClass(req, "expression"))
  }

  assert(all(as.integer(dim) == dim))
  dim = as.integer(dim)

  assertInteger(dim, lower=1, len=1)

  makeS3Obj("searchparam", name=name, values=values, type=type, trafo=trafo, id=id, dummy=dummy,
      req=req, dim=dim)
}

makeNamedAlList = function(...) {  # make a named list, more convenient to use
  l = list(...)
  n = sapply(l, function(item) ifelse(is.character(item$learner), item$learner, item$learner$id))
  names(l) = n
  l
}