#' @title Create \code{Autolearner} objects
#'
#' @description
#' \code{Autolearner} objects wrap mlr \code{Learner} objects as well as
#' preprocessing functions and provide additional meta information. This is used
#' to define the automlr searchspace.
#' 
#' @param learner [\code{Learner}|list]\cr
#'   An mlr \code{Learner} object for a \code{"learner"} stacktype, otherwise
#'   an object returned by \code{\link{autoWrapper}}.\cr
#'   If a \code{Learner} is required, it may also be the ID of the
#'   \code{Learner} which postpones allocation of memory and loading of
#'   packages and may therefore be preferred.
#' @param searchspace [list of \code{searchparam}]\cr
#'   List of \code{searchparam}s reqpresenting the relevant parameter search
#'   space.
#' @param stacktype [\code{character(1)}]\cr
#'   Describing how this object can be connected with other learners. Must be
#'   one of \code{"wrapper"}, \code{"requiredwrapper"} (e.g. feature selection)
#'   or \code{"learner"}.
#'
#' @export
autolearner = function(learner, searchspace = list(), stacktype = "learner") {
  
  assertChoice(stacktype, c("learner", "wrapper", "requiredwrapper"))
  names = extractSubList(searchspace, "name")
  if (any(duplicated(names))) {
    stopf("Duplicated names %s for learner '%s'",
        paste(unique(names[duplicated(names)]), collapse = ", "),
        ifelse(is.character(learner), learner,
            coalesce(learner$id, learner$name)))
  }
  makeS3Obj("Autolearner",
            learner = learner,
            searchspace = searchspace,
            stacktype = stacktype)
}

#' @title Create a wrapper object for \code{\link{autolearner}}.
#' 
#' @description
#' Build a wrapper object that can be plugged into \code{\link{autolearner}} to
#' give wrapper functions to \code{\link{buildLearners}}.
#' 
#' @param name [\code{character(1)}]\cr
#'   the name of the wrapper. Must not contain a \code{$} character.
#' @param constructor [\code{function}]\cr
#'   The function that will be called with the learner as a single argument and
#'   construct another \code{Learner}.
#' @param conversion [\code{function}]\cr
#'   A function giving information about the data conversion this wrapper
#'   performs. Takes one of \code{"factors"}, \code{"ordered"},
#'   \code{"numerics"}, \code{"missing"} as an argument and returns a character
#'   vector containing a subset of these and optionally the empty string
#'   \code{""}. A function returning a set B when giving the argument A
#'   indicates that the wrapper is able to convert data from format A to any of
#'   the format B. The wrappers parameter set must then adhere to
#'   automlr.remove.XXX (for XXX being each element of B) pseudo parameters in
#'   their parameter requirements (see \code{\link{makeAMExoWrapper}}).\cr
#'   \emph{Currently, only removal of features is supported; therefore, this
#'   should only return its argument and possibly \code{""}. Otherwise, the
#'   behaviour is buggy.}
#' 
#' @export
autoWrapper = function(name, constructor, conversion) {
  assertString(name)
  assert(identical(grep("$", name, fixed = TRUE), integer(0)))
  assertFunction(constructor)
  assertFunction(conversion, nargs = 1)
  
  input = c("factors", "ordered", "numerics", "missings")
  output = c(input, "")
  for (inp in c("factors", "ordered", "numerics", "missings")) {
    assert(all(conversion(inp) %in% output))
  }
  
  list(name = name, constructor = constructor, conversion = conversion)
}

#' @export
print.Autolearner = function(x, ...) {
  cat(sprintf("<automlr learner '%s'>\n",
          ifelse(is.character(x$learner), x$learner,
              coalesce(x$learner$id, x$learner$name))))
}

#' @title Define the searchspace parameter in a short form
#' 
#' @param name [\code{character(1)}]\cr
#'   The name of the parameter which must match the id of the \code{Param} it
#'   refers to. May be suffixed with \code{.AMLRFIX#}, where \code{#} is a
#'   number, to expose one varible to the outside with different search space
#'   depending on requirements.
#' @param type [\code{character(1)}]\cr
#'   The type of the parameter. One of: \code{real} (numeric), \code{int}
#'   (integer), \code{cat} (discrete), \code{bool} (logical), \code{fix} (fixed
#'   value of whatever type), \code{def} (using default value / not setting the
#'   value).
#' @param values [\code{numeric}|\code{character}]\cr
#'   If \code{type} is \code{real} or \code{int}, this gives the lower and upper
#'   bound. If \code{type} is \code{cat}, it is a character vector of possible
#'   values. If \code{type} is \code{fix}, only one value (the one to be fixed)
#'   must be given. If \code{type} is code{bool} or \code{def}, it must be
#'   \code{NULL}.
#' @param trafo [\code{character}|\code{function}]\cr
#'   May be "exp" for exponential transformation. Transforms integer parameters
#'   in a smart way. Only applies for \code{real} and \code{int} values. May
#'   also be an R function.
#' @param id [\code{character(1)}]\cr
#'   May be given to identify parameters of different learners having the same
#'   function.\cr
#'   This currently has no effect beyond warnings if the \code{id} is only given
#'   once in a search space.
#' @param special [\code{character(1)}]\cr
#'   May be \code{NULL}, \code{"dummy"} or \code{"inject"}. Set this to
#'   \code{"dummy"} if this is a dummy variable that will be hidden from the
#'   learner itself but visible to the outside. Set this to \code{"inject"} to
#'   create the parameter in the learners \code{ParamSet} if it does not exist.
#' @param req [\code{language}]\cr
#'   A requirement for the variable to have effect.
#' @param dim [\code{integer(1)}]\cr
#'   The number of dimensions of this variable.
#'
#' @export
sp = function(name, type = "real", values = NULL, trafo = NULL, id = NULL,
    special = NULL, req = NULL, dim = 1) {
  assertChoice(type, c("real", "int", "cat", "bool", "fix", "def"))

  assertString(name)
  assert(nchar(name) > 0)

  if (type %in% c("real", "int")) {
    assertNumeric(values, any.missing = FALSE, len = 2)
    assert(values[2] >= values[1])
    if (type == "int") {
      assert(all(as.integer(values) == values))
      values = as.integer(values)
    }
  } else if (type %in% c("fix", "def")) {
    if (!is.null(values)) {
      assertVector(values, strict = TRUE, len = 1)
    }
  } else if (type == "cat"){
    assertVector(values, strict = TRUE, min.len = 1)
  } else {  # type == "bool"
    assertNull(values)
  }

  if (!is.null(trafo)) {
    if (identical(trafo, "exp")) {
      assert(type %in% c("real", "int"))
    } else {
      assertFunction(trafo, nargs = 1)
    }
  }

  if (!is.null(id)) {
    assertString(id)
    assert(type %nin% c("fix", "def"))
    assert(nchar(id) > 0)
  }

  if (!is.null(special)) {
    assertChoice(special, c("dummy", "inject"))
  }

  if (!is.null(req)) {
    assert(checkClass(req, "call"), checkClass(req, "expression"))
  }

  assert(all(as.integer(dim) == dim))
  dim = as.integer(dim)

  assertInteger(dim, lower = 1, len = 1)

  makeS3Obj("searchparam",
      name = name,
      values = values,
      type = type,
      trafo = trafo,
      id = id,
      special = special,
      req = req,
      dim = dim)
}

# make a named list, more convenient to use
makeNamedAlList = function(...) {
  l = list(...)
  n = sapply(l, function(item) ifelse(is.character(item$learner),
            item$learner, coalesce(item$learner$id, item$learner$name)))
  names(l) = n
  l
}