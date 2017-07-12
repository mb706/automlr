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
#'   one of \code{"wrapper"} or \code{"learner"}.
#'
#' @export
autolearner = function(learner, searchspace = list(), stacktype = "learner") {
  assertChoice(stacktype, c("learner", "wrapper"))
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
#' @param cpo [\code{CPO}]\cr
#'   The cpo that will be attached to the learner and construct another
#'   \code{Learner}.
#' @param datatype [\code{character(1)}]\cr
#'   The data this wrapper operates on.
#' @param convertfrom [\code{character(1)} | \code{NULL}]\cr
#'   If this wrapper converts data from one type to another, \dQuote{datatype}
#'   must be the target type, and \dQuote{convertfrom} must be the source type.
#'   If the wrapper is an imputing wrapper, \dQuote{convertfrom} must be
#'   \dQuote{missings}, and \dQuote{datatype} must be the type of columns that
#'   have their missings imputed. A given wrapper may only impute missings of
#'   one column type, even though it sees all columns.
#'
#' @export
autoWrapper = function(name, cpo, datatype, convertfrom = NULL) {
  assertString(name)
  assert(identical(grep("$", name, fixed = TRUE), integer(0)))
  assertClass(cpo, "CPO")

  cpoprops = getCPOProperties(cpo)

  if (!is.null(convertfrom)) {
    assertChoice(convertfrom, c("factors", "ordered", "numerics", "missings"))
    assertChoice(convertfrom, cpoprops$properties.adding)
    if (convertfrom != "missings") {
      assert(datatype == cpoprops$properties.needed)
    }
  } else {
    assertChoice(datatype, cpoprops$properties)
  }
  if (is.null(convertfrom) || convertfrom == "missings") {
    assert(length(cpoprops$properties.needed) == 0 ||
            identical(cpoprops$properties.needed, datatype))
  }
  assertChoice(datatype, c("factors", "ordered", "numerics"))
  assert(!identical(convertfrom, datatype))



  makeS3Obj("AutoWrapper",
      name = name,
      cpo = cpo,
      is.imputer = identical(convertfrom, "missings"),
      is.converter = !is.null(convertfrom) && convertfrom != "missings",
      convertfrom = convertfrom,
      datatype = datatype)
}

#' @export
print.Autolearner = function(x, ...) {
  catf("<automlr %s '%s' [%s]>",
      if (x$stacktype == "learner") "learner" else {
            if (x$learner$is.imputer) {
              sprintf("imputer (%s)", x$learner$datatype)
            } else if (x$learner$is.converter) {
              sprintf("converter (%s -> %s)", x$learner$convertfrom,
                  x$learner$datatype)
            } else {
              sprintf("preproc (%s)", x$learner$datatype)
            }
          },
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

  if (type %in% c("fix", "def", "fixdef") && !is.null(req)) {
    stop("Requirements not allowed when type is 'fix', 'def', or 'fixdef'.")
  }

  if (!is.null(req)) {
    assert(checkClass(req, "call"), checkClass(req, "expression"))
  }

  if (identical(values, "##") && type != "def") {
    stop("Only type 'def' parameters can have value '##'.")
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
# makeLearnerParam
# forWrapper
createParameter = function(param, learnerid, makeLearnerParam = FALSE,
    forWrapper = FALSE) {
  # makeLearnerParam == TRUE means
  # 1) create LearnerParam instead of Param
  # 2) remove .AMLRFIX# suffix
  # 3) no Trafo
  if (param$type %in% c("int", "real")) {
    pmin = param$values[[1]]
    pmax = param$values[[2]]
  }
  expression.trafo = NULL
  if (makeLearnerParam) {
    param$name = removeAmlrfix(param$name)
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
  } else if (param$numexp) {
    assertChoice(param$type, c("int", "real"))
    if (!is.null(param$trafo) && !identical(param$trafo, "exp")) {
      stop("Expression bounds with trafo that is not 'exp' not yet supported.")
    }
    expression.trafo = createExpressionTrafo(pmin, pmax,
        param$type == "int", identical(param$trafo, "exp"))
    pmin = 0
    pmax = 1
    param$type = "real"
    param$trafo = NULL
  }
  if (is.character(param$trafo)) {
    assertChoice(param$trafo, c("exp", "invexp"))
    aux = createTrafo(pmin, pmax,
        invert = param$trafo == "invexp", is.int = param$type == "int")
    ptrafo = aux$trafo
    pmin = aux$newmin
    pmax = aux$newmax
  } else {
    ptrafo = param$trafo  # will either be a function or NULL
  }
  surrogatetype = param$type
  if (param$type == "fix" && !forWrapper) {
    # we should realistically only get here because we are creating the
    # LearnerParam.
    assert(makeLearnerParam)
    # don't warn for wrappers, for them fixed injects are the norm
    warningf(paste("Parameter '%s' for learner '%s' is marked",
            "'inject' and has type 'fix'; This usually does not make",
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
    if (makeLearnerParam) {
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
    if (makeLearnerParam) {
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
  paramlist %c=% switch(surrogatetype,
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
          param$name, learnerid))
  if (makeLearnerParam) {
    paramlist$trafo = NULL
  }
  pobject = do.call(constructor, paramlist, quote = TRUE)
  if (!is.null(pobject$trafo)) {
    pobject$trafo = guardTrafoNa(pobject$trafo)
  }
  pobject$amlr.isDummy = identical(param$special, "dummy")
  pobject$amlr.lrnid = learnerid
  pobject$amlr.origValues = param$values
  pobject$amlr.expressionTrafo = expression.trafo
  pobject
}

guardTrafoNa = function(origTrafo) {
  force(origTrafo)
  function(x) if (length(x) == 1 && is.na(x)) x else origTrafo(x)
}

createTrafo = function(min, max, invert, is.int) {
  if (is.int) {
    if (invert) {
      stop("invexp int not supported yet.")
    }
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
    if (invert) {
      max = 1 - max
      min = 1 - min
    }
    force(min)
    force(max)
    assert(min > 0)
    assert(max > 0)

    return(list(trafo = function(x) {
              res = min * (max / min)^x
              if (invert) 1 - res else res
            }, newmin = 0, newmax = 1))
  }
}

createExpressionTrafo = function(pmin, pmax, is.int, is.exp) {
  force(pmin)
  force(pmax)
  force(is.int)
  force(is.exp)
  # env must contain:
  # PARAM.x, p (number of features), n (number of rows)
  function(x, env) {
    if (is.language(pmin)) {
      pmin = eval(pmin, envir = env, enclose = globalenv())
    }
    if (is.language(pmax)) {
      pmax = eval(pmax, envir = env, enclos = globalenv())
    }
    assert(pmax >= pmin)
    if (is.int) {
      assertIntegerish(pmin, any.missing = FALSE, len = 1)
      assertIntegerish(pmax, any.missing = FALSE, len = 1)
      pmax = pmax + 1
      if (is.exp) {
        addzero = pmin == 0
        if (addzero) {
          pmin = 1
          pmax %+=% 1
        }
        ratio = sqrt((pmin + 1) / pmin)
        res = pmin * ratio ^ (x * log(pmax / pmin, base = ratio))
        if (addzero) {
          res %-=% 1
          pmax %-=% 1
        }
      } else {
        res = x * (pmax - pmin) + pmin
      }
      min(floor(res), pmax - 1)
    } else {
      trafofn = identity
      if (is.exp) {
        trafo = createTrafo(pmin, pmax, FALSE, FALSE)
        trafofn = trafo$trafo
        pmin = trafo$newmin
        pmax = trafo$newmax
      }
      assertNumeric(pmin, any.missing = FALSE, len = 1)
      assertNumeric(pmax, any.missing = FALSE, len = 1)
      
      trafofn(x * (pmax - pmin) + pmin)
    }
  }
}


makeLearnerPars = function(learnerPars) {
  for (p in getParamIds(learnerPars)) {
    if (!is.null(learnerPars$pars[[p]]$trafo) &&
        learnerPars$pars[[p]]$type %in%
        c("numeric", "numericvector", "integer", "integervector")) {
      # there is a trafo --> need to change limits
      if (is.null(learnerPars$pars[[p]]$amlr.origValues)) {
        learnerPars$pars[[p]]$lower = -Inf
        learnerPars$pars[[p]]$upper = Inf
      } else {
        learnerPars$pars[[p]]$lower = learnerPars$pars[[p]]$amlr.origValues[[1]]
        learnerPars$pars[[p]]$upper = learnerPars$pars[[p]]$amlr.origValues[[2]]
      }
      # convert type to "numeric(vector)", since after trafo we are not sure
      # it is still an int
      learnerPars$pars[[p]]$type = switch(learnerPars$pars[[p]]$type,
          integer = "numeric",
          integervector = "numericvector",
          learnerPars$pars[[p]]$type)
    }
    learnerPars$pars[[p]]$trafo = NULL
    # as the things stand now we don't wrap setHyperPars and therefore all
    # hyperpars need to be given to the train function. If this ever changes
    # (and we e.g. call predictLearner() instead of predict(), and we wrap
    # setHyperPars() also) we need to copy the $when property of the
    # corresponding LearnerParam inside the modelmultiplexer object.
    learnerPars$pars[[p]]$when = "train"
    learnerPars$pars[[p]] = addClasses(learnerPars$pars[[p]], "LearnerParam")
    # satisfying a weird constraint of mlr:
    req = learnerPars$pars[[p]]$requires
    if (!is.null(req) && is.expression(req)) {
      if (length(req) == 1) {
        learnerPars$pars[[p]]$requires = req[[1]]
      } else {
        learnerPars$pars[[p]]$requires = substitute(eval(x), list(x = req))
      }
    }
  }
  learnerPars
}



