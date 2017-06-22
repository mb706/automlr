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
#' @param searchspace [list of \code{Searchparam}]\cr
#'   List of \code{Searchparam}s reqpresenting the relevant parameter search
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
  if (stacktype != "learner") {
    assertClass(learner, "AutoWrapper")
  } else if (!is.character(learner)) {
    assertClass(learner, "Learner")
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
  
  makeS3Obj("AutoWrapper",
      name = name,
      constructor = constructor,
      conversion = conversion)
}

#' @export
print.Autolearner = function(x, ...) {
  catf("<automlr learner '%s' [%s]>",
      ifelse(is.character(x$learner), x$learner,
          coalesce(x$learner$id, x$learner$name)),
      if (length(x$searchspace) == 0) "" else {
            sprintf("\n  %s\n", collapse(sapply(x$searchspace,
                        function(x) collapse(capture.output(print(x)), sep="")),
                        sep = ",\n  "))
          })
}

#' @export
print.AutoWrapper = function(x, ...) {
  catf("AutoWrapper %s", x$name)
}

#' @title Define the searchspace parameter in a short form
#' 
#' @description
#' This function is used to define the search space related to a given learner.
#' The priority here is that the function should be saving space: Much of the
#' information about a parameter is inferred from the learner itself; In
#' principle only the name, the type, and a range of a parameter are needed.
#' 
#' However, to get notifications about changes in the mlr package, also all the
#' parameters that are not given should be referenced with an \code{sp()} of
#' type \code{"def"}; otherwise, a warning will be given upon instantiation of
#' the learner.  
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
#'   value), \code{fixdef} (fixed value, but warn if this is not the default)
#' @param values [\code{numeric}|\code{character}|\code{list}]\cr
#'   If \code{type} is \code{real} or \code{int}, this gives the lower and upper
#'   bound. If \code{type} is \code{cat}, it is a character vector of possible
#'   values. If \code{type} is \code{fix}, only one value (the one to be fixed)
#'   must be given. If \code{type} is code{bool} or \code{def}, it must be
#'   \code{NULL}.\cr
#'   It is possible to give expression values instead of constant values,
#'   usually using \code{quote}. These expressions can involve the special
#'   values \code{n} (number of observations), \code{p} (number of features),
#'   the automlr pseudoparameters described in the docs of
#'   \code{\link{makeAMExoWrapper}}, and \code{PARAM.x} to refer to the value of
#'   parameter \code{x}. If at least one expression is given, \code{values}
#'   needs to be a list containing the expressions and singleton vectors of
#'   the appropriate type.\cr
#'   A special value for \dQuote{def} type parameters is \dQuote{##}, which
#'   leads to using the current default of the function without specifying it.
#' @param trafo [\code{character}|\code{function}|\code{NULL}]\cr
#'   May be \dQuote{exp} for exponential transformation, or \dQuote{invexp} for
#'   exponential transformation of the difference of the value and its upper
#'   bound. Transforms integer parameters in a smart way. Only applies for
#'   \code{real} and \code{int} values. May also be an R function.
#' @param id [\code{character(1)}|\code{NULL}]\cr
#'   May be given to identify parameters of different learners having similar
#'   effects in similar learners.\cr
#'   This currently has no effect beyond warnings if different parameters with
#'   the same \code{id} have different value ranges.
#' @param special [\code{character(1)}|\code{NULL}]\cr
#'   May be \code{NULL}, \code{"dummy"} or \code{"inject"}. Set this to
#'   \code{"dummy"} if this is a dummy variable that will be hidden from the
#'   learner itself but visible to the outside. Set this to \code{"inject"} to
#'   create the parameter in the learners \code{ParamSet} if it does not exist.
#' @param req [\code{language}|\code{NULL}]\cr
#'   A requirement for the variable to have effect. May reference other
#'   parameters of the learner, as well as the automlr pseudoparameters
#'   described in \code{\link{makeAMExoWrapper}}. Must not call any parameter
#'   values as functions!
#' @param dim [\code{integer(1)}]\cr
#'   The number of dimensions of this variable.
#' @param version[\code{character(1)}|\code{NULL}]\cr
#'   Version of MLR to apply this parameter to. If not \code{NULL}, this must
#'   be a \code{character} made up of a comparison operator (one of \dQuote{>},
#'   \dQuote{<}, \dQuote{>=}, \dQuote{<=}, or \dQuote{==}) and an MLR version
#'   number of the form \dQuote{MAJOR.MINOR}. If the mlr version that was found
#'   does not satisfy the requirement, the parameter will be ignored.
#'
#' @export
sp = function(name, type = "real", values = NULL, trafo = NULL, id = NULL,
    special = NULL, req = NULL, dim = 1, version = NULL) {
  assertChoice(type, c("real", "int", "cat", "bool", "fix", "def", "fixdef"))

  assertString(name)
  assert(nchar(name) > 0)

  numexp = 0
  if (type %in% c("real", "int")) {

    if (is.list(values)) {
        # filter out expressions
        expression.idx = vlapply(values, is.language)
        expressions = values[expression.idx]
        lapply(expressions, function(e)
                    assert(checkClass(e, "call"),
                            checkClass(e, "expression"),
                            checkClass(e, "name")))
        nonexp.values = unlist(values[!expression.idx], recursive = FALSE)
        numexp = length(values) - length(nonexp.values)
        assert(numexp == sum(expression.idx))
    } else {
      nonexp.values = values
      values = as.list(values)
    }
    assertNumeric(nonexp.values, any.missing = FALSE, len = 2 - numexp)
    if (numexp == 0) {
      assert(values[[2]] >= values[[1]])
    }
    if (type == "int") {
      assert(all(as.integer(nonexp.values) == nonexp.values))
      values = lapply(values, function(x)
            if (is.language(x)) x else as.integer(x))
    }
  } else if (type %in% c("fix", "def", "fixdef")) {
    if (!is.null(values)) {
      assertVector(values, strict = TRUE, len = 1)
    }
  } else if (type == "cat"){
    assertVector(values, strict = TRUE, min.len = 1)
  } else {  # type == "bool"
    assertNull(values)
  }

  if (!is.null(trafo)) {
    if (identical(trafo, "exp") || identical(trafo, "invexp")) {
      assert(type %in% c("real", "int"))
    } else {
      assertFunction(trafo, nargs = 1)
    }
  }

  if (!is.null(id)) {
    assertString(id)
    assert(type %nin% c("fix", "def", "fixdef"))
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

  makeS3Obj("Searchparam",
      name = name,
      values = values,
      type = type,
      trafo = trafo,
      numexp = numexp,
      id = id,
      special = special,
      req = req,
      dim = dim)
}

#' @export
print.Searchparam = function(x, ...) {
  toString = function(val) {
    if (is.character(val)) {
      val
    } else if (is.list(val)) {
      sapply(val, function(x)
            collapse(deparse(x, width.cutoff=500), sep = "\n"))
    } else {
      collapse(deparse(val, width.cutoff=500), sep = "\n")
    }
  }
  
  catf("%s%s %s%s%s%s%s%s%s", x$type,
      if (x$dim > 1) sprintf("^%s", x$dim) else "", x$name,
      if (!is.null(x$trafo))
            sprintf(" %s(", toString(x$trafo))
      else if (x$type == "bool") "" else " ",
      if (x$type %in% c("int", "real")) {
        sprintf("[%s]", collapse(toString(x$values), sep = ".."))
      } else if (x$type != "bool") {
        sprintf("{%s}", collapse(toString(x$values)))
      } else {
        ""
      }, if (!is.null(x$trafo)) ")" else "",
      if (!is.null(x$id)) sprintf(" {%s}", x$id) else "",
      if (!is.null(x$special)) sprintf(" (%s)", x$special) else "",
      ifelse(is.null(x$req), "", " *"))
}

# make a named list, more convenient to use
makeNamedAlList = function(...) {
  l = list(...)
  n = sapply(l, function(item) ifelse(is.character(item$learner),
            item$learner, coalesce(item$learner$id, item$learner$name)))
  names(l) = n
  l
}


# This is an interface for makeXYZParam
# where X is the type (Numeric, Integer, ...)
#       Y may be "Vector"
#       Z may be "Learner" 
# param: a "Searchparam" created with sp()
# learnerid: for debug messages
# do.trafo
# facingOutside
# forWrapper
createParameter = function(param, learnerid, do.trafo = TRUE,
    facingOutside = TRUE, forWrapper = FALSE) {
  # facingOutside == FALSE means
  # 1) create LearnerParam instead of Param
  # 2) remove .AMLRFIX# suffix
  if (param$type %in% c("int", "real")) {
    pmin = param$values[1]
    pmax = param$values[2]
  }
  if (!facingOutside) {
    param$name = removeAmlrfix(param$name)
  }
  if (!do.trafo) {
    if (!is.null(param$trafo)) {
      if (param$type %in% c("int", "real") && !is.character(param$trafo)) {
        # if the transformation is not our own trafo, then we can't say
        # for sure what the real bounds are, or even what the param type is.
        pmin = -Inf
        pmax = Inf
        param$type = "real"
      }
      param$trafo = NULL
    }
  }
  if (!is.null(param$trafo) && identical(param$trafo, "exp")) {
    if (param$type == "real") {
      aux = createTrafo(pmin, pmax, FALSE)
    } else {
      # param$type == "int"
      aux = createTrafo(pmin, pmax, TRUE)
    }
    ptrafo = aux$trafo
    pmin = aux$newmin
    pmax = aux$newmax
  } else {
    ptrafo = param$trafo  # will either be a function or NULL
  }
  surrogatetype = param$type
  if (param$type == "fix" && !forWrapper) {
    # don't warn for wrappers, for them fixed injects are the norm
    warningf(paste("Parameter '%s' for learner '%s' is marked",
            "'dummy/inject' and has type 'fix'; This usually does not make",
            "sense."),
        param$name, learnerid)
    if (!is.null(param$values)) {
      if (is.numeric(param$values[1])) {
        surrogatetype = "real"
        pmin = param$values[1]
        pmax = param$values[1]
      } else if (is.logical(param$values[1])) {
        surrogatetype = "bool"
      }
    }
  }
  if (param$dim > 1) {
    if (!facingOutside) {
      constructor = switch(surrogatetype,
          real = makeNumericVectorLearnerParam,
          int = makeIntegerVectorLearnerParam,
          cat = makeDiscreteVectorLearnerParam,
          bool = makeLogicalVectorLearnerParam,
          fix = makeDiscreteVectorLearnerParam,
          NULL)
    } else {
      constructor = switch(surrogatetype,
          real = makeNumericVectorParam,
          int = makeIntegerVectorParam,
          cat = makeDiscreteVectorParam,
          bool = makeLogicalVectorParam,
          fix = makeDiscreteVectorParam,
          NULL)
    }
    paramlist = list(id = param$name, len = param$dim, requires = param$req)
  } else {
    if (!facingOutside) {
      constructor = switch(surrogatetype,
          real = makeNumericLearnerParam,
          int = makeIntegerLearnerParam,
          cat = makeDiscreteLearnerParam,
          bool = makeLogicalLearnerParam,
          fix = makeDiscreteLearnerParam,
          NULL)
    } else {
      constructor = switch(surrogatetype,
          real = makeNumericParam,
          int = makeIntegerParam,
          cat = makeDiscreteParam,
          bool = makeLogicalParam,
          fix = makeDiscreteParam,
          NULL)
    }
    paramlist = list(id = param$name, requires = param$req)
  }
  paramlist = c(paramlist, switch(surrogatetype,
          int = list(lower = pmin, upper = pmax, trafo = ptrafo),
          real = list(lower = pmin, upper = pmax, trafo = ptrafo),
          cat = list(values = {
                x = param$values
                if (!test_named(x)) {
                  names(x) = param$values
                }
                x}),
          bool = list(),
          fix = list(values = {
                x = param$values
                if (!test_named(x)) {
                  names(x) = param$values
                }
                x}),
          def = stopf(paste("Parameter '%s' for learner '%s' marked as",
                  "'inject' must not have type 'def'."),
              param$name, learnerid),
          stopf("Unknown type '%s'; parameter '%s', learner '%s'", param$type,
              param$name, learnerid)))
  if (!facingOutside) {
    paramlist$trafo = NULL
  }
  pobject = do.call(constructor, paramlist, quote = TRUE)
  if (!is.null(pobject$trafo)) {
    environment(pobject$trafo) = list2env(as.list(info.env, all.names = TRUE),
        parent = environment(pobject$trafo))
    if (identical(param$trafo, "exp")) {
      pobject$amlr.origValues = param$values
    }
    pobject$trafo = guardTrafoNa(pobject$trafo)
  }
  pobject$amlr.isDummy = identical(param$special, "dummy")
  pobject
}

guardTrafoNa = function(origTrafo) {
  force(origTrafo)
  function(x) if (length(x) == 1 && is.na(x)) x else origTrafo(x)
}

createTrafo = function(min, max, isint) {
  if (isint) {
    assert(min >= 0)
    addzero = if (min == 0) 0
    if (min == 0) {
      min = 1
    }
    ratio = sqrt((min + 1) / min)
    sequence = unique(c(addzero,
            round(min * ratio ^ (seq(from = 0,
                          to = floor(log(max / min, base = ratio))))),
            max))
    return(list(trafo = function(x) ifelse(is.na(x), NA, sequence[x]),
            newmin = 1, newmax = length(sequence)))
  } else {
    force(min)
    force(max)
    assert(min > 0)
    assert(max > 0)
    
    return(list(trafo = function(x) min * (max / min)^x,
            newmin = 0, newmax = 1))
  }
}


