


# Creates the seach space, suitable for tuning, from lists of 'sp' created
# Searchparam objects.
# sslist: list of Searchparam
# l: the learner
# verbosity: controls warning on conflicts
buildTuneSearchSpace = function(sslist, lrn, verbosity) {
  if (verbosity.sswarnings(verbosity)) {
    lwarn = function(...) warningf(...)
  } else {
    lwarn = function(...) {}
  }
  lrnid = lrn$id
  lpids = getParamIds(getParamSet(lrn))
  allParams = extractSubList(sslist, "name")
  untouchedParams = setdiff(lpids, allParams)
  if (length(untouchedParams)) {
    lwarn(paste("Learner '%s' has parameters %s that are not mentioned in",
            "search space."),
        lrnid, paste(untouchedParams, collapse = ", "))
  }
  if (any(duplicated(allParams))) {
    stopf("Duplicate parameter names %s for learner %s.",
        collapse(unique(allParams[duplicated(allParams)])), lrnid)
  }
  
  checkAmlrFix(sslist, lrn, verbosity)
  checkDummies(sslist, lrn)

  lrn = injectParams(sslist, lrn)

  sslist = makeParametersFeasible(sslist, lrn, verbosity)
  
  sslist = adjustSSDefaults(sslist, lrn, verbosity)
  
  defParams = extractSubList(Filter(function(x) x$type == "def", sslist),
      "name")
  
  lrn = removeHyperPars(lrn, intersect(names(getHyperPars(lrn)), defParams))


  lptypes = getParamTypes(getParamSet(lrn), use.names = TRUE)

  nondefParamNames = character(0)

  tuneSearchSpace = list()

  for (param in sslist) {
    origParamName = removeAmlrfix(param$name)
    if (param$type != "def") {
      nondefParamNames %c=% origParamName
    }
    if (param$type == "fix") {
      if (!is.null(param$values)) {
        if (lptypes[[origParamName]] == "discretevector") {
          assignment = list(rep(list(param$values), param$dim))
        } else {
          assignment = list(
              if (param$dim > 1) rep(param$values, param$dim) else param$values)
        }
        names(assignment) = origParamName
        lrn = setHyperPars(lrn, par.vals = assignment)
      }
    } else {
      if (origParamName %in% names(getHyperPars(lrn))) {
        # make sure this is not set at a default.
        lwarn(paste("Parameter '%s' for learner '%s' was already set to a",
                "value; this value has been removed."),
            param$name, lrnid)
        lrn = removeHyperPars(lrn, origParamName)
      }
      if (param$type != "def") {  # variable parameter
        newparam = createParameter(param, learnerid = lrnid)
        newparam$amlr.id = param$id
        if (param$type == "cat" && param$dim > 1 &&
            !is.null(lptypes[[origParamName]]) && 
            lptypes[[origParamName]] != "discretevector") {
          # if the underlying type is a VectorParam but not discrete, we will
          # have to unlist().
          newparam$amlr.isNotCat = TRUE
        }
        tuneSearchSpace %c=% list(newparam)
      }
    }
  }
  list(tss = makeParamSet(params = tuneSearchSpace), lrn = lrn,
      nondefParamNames = unique(nondefParamNames))
}


checkAmlrFix = function(sslist, lrn, verbosity) {
  if (verbosity.sswarnings(verbosity)) {
    lwarn = function(...) warningf(...)
  } else {
    lwarn = function(...) {}
  }
  lrnid = lrn$id

  canNotBeAMLRFIX = character(0)
  areAlreadyAMLRFIX = character(0)
  presentWithoutAMLRFIX = character(0)
  for (param in sslist) {
    origParamName = removeAmlrfix(param$name)
    # AMLRFIX must:
    #  - have requirements
    #  - not be 'fix' or 'def' or 'fixdef'
    #  - not be DUMMY
    #  - not have the same prefix as a parameter does any of this
    if (param$name != origParamName) {
      if (param$type %in% c("fix", "def", "fixdef")) {
        stopf(paste("Parameter '%s' for learner '%s' is of type '%s' but has",
                "an .AMLRFIX suffix."),
            param$name, lrnid, param$type)
      }
      if (is.null(param$req)) {
        stopf(paste("Parameter '%s' for learner '%s' has an .AMLRFIX suffix",
                "but no requirements"),
            param$name, lrnid)
      }
      
      if (identical(param$special, "dummy")) {
        stopf(paste("Parameter '%s' for learner '%s' has an .AMLRFIX suffix",
                "but is also a DUMMY parameter."),
            param$name, lrnid, param$type)
      }
      if (origParamName %in% canNotBeAMLRFIX) {
        stopf(paste("Parameter '%s' for learner '%s' cannot have an .AMLRFIX",
                "suffix. Possible Reasons:\nanother parameter with that name",
                "has type 'def' or 'fix', has no requirements or is a",
                "'dummy'."),
            param$name, lrnid)
      }
      areAlreadyAMLRFIX = union(areAlreadyAMLRFIX, origParamName)
    } else {
      presentWithoutAMLRFIX %c=% origParamName
    }
    
    if (is.null(param$req) || identical(param$special, "dummy") ||
        param$type %in% c("def", "fix", "fixdef")) {
      if (origParamName %in% areAlreadyAMLRFIX) {
        stopf(paste("Parameter '%s' for learner '%s' already defined with",
                ".AMLRFIX suffix cannot be given without requirement or as",
                "dummy, def or fix."),
            param$name, lrnid)
      }
      canNotBeAMLRFIX = union(canNotBeAMLRFIX, origParamName)
    }
  }
  
  notWithoutAMLRFIX = setdiff(areAlreadyAMLRFIX, presentWithoutAMLRFIX)
  if (length(notWithoutAMLRFIX)) {
    stopf("Parameter(s) %s of learner %s not present without .AMLRFIX.",
        collapse(notWithoutAMLRFIX, lrnid))
  }
}

checkDummies = function(sslist, lrn) {
  lpids = getParamIds(getParamSet(lrn))
  lrnid = lrn$id

  dummyParams = Filter(function(param)
        identical(param$special, "dummy"), sslist)
  baddummy = intersect(extractSubList(dummyParams, "name"), lpids)
  if (length(baddummy)) {
    stopf(paste("Parameter(s) '%s' present in learner '%s' but also marked",
            "as 'dummy' in search space."),
        collapse(baddummy), lrnid)
  }
  dummytype = extractSubList(dummyParams, "type") 
  badDummyType = extractSubList(
      dummyParams[dummytype %in% c("def", "fix", "fixdef")],
      "name")
  if (length(badDummyType)) {
    stopf(paste("Dummy parameter(s) '%s' given for learner '%s' must not be",
            "of type 'fix', 'def', or 'fixdef'."),
        collapse(badDummyType), lrnid)
  }
}

# the only change this introduces is when a 'cat' has character values but
# the underlying parameter has not.
makeParametersFeasible = function(sslist, lrn, verbosity) {
  lrnid = lrn$id
  if (verbosity.sswarnings(verbosity)) {
    lwarn = function(...) warningf(...)
  } else {
    lwarn = function(...) {}
  }
  lp = getParamSet(lrn)
  lpids = getParamIds(lp)
  lptypes = getParamTypes(lp, use.names = TRUE)
  
  lapply(sslist, function(param) {
        if (identical(param$special, "dummy") ||
            identical(param$special, "inject")) {
          return(param)
        }
        if (param$type == "bool") {
          # useful since now we can automatically check for feasibility 
          # by checking whether everything in param$values is feasible.
          param$values = c(TRUE, FALSE)
        }
        origParamName = removeAmlrfix(param$name)
        if (origParamName %nin% lpids) {
          stopf(paste("Parameter '%s' as listed in search space is not",
                  "available for learner '%s'."),
              param$name, lrnid)
        }
        if (param$type == "def" && identical(param$values, "##")) {
          next
        }
        if (!allfeasible(lp, param$values, origParamName, param$dim)) {
          # there is one 'special case': param$values might be names that
          # index into lp$pars[[param$name]]$values.
          vals = getValues(lp)[[origParamName]]
          if (isSubset(param$values, names(vals)) &&
              param$dim == getParamLengths(lp)[[origParamName]]) {
            param$values = vals[param$values]
            assert(allfeasible(lp, param$values, origParamName, param$dim))
          } else {
            stopf(paste("Parameter '%s' as listed in search space has",
                    "infeasible bounds '%s' for learner '%s'."),
                param$name,
                paste(param$values, collapse = "', '"),
                lrnid)
          }
        }
        partype = lptypes[[origParamName]]
        if ((partype %nin% c("numeric", "numericvector", "untyped")) &&
            (param$type == "real" || (param$type == "int" &&
                partype %nin% c("integer", "integervector")))) {
          stopf(paste("Parameter '%s' as listed in search space has",
                  "wrong type '%s' for learner '%s'"),
              param$name, param$type, lrnid)
        }
        if ((partype != "untyped") &&
            ((param$type == "int" &&
                partype %nin% c("integer", "integervector")) ||
              (param$type == "bool" &&
                partype %nin% c("logical", "logicalvector")) ||
              (param$type == "cat" &&
                partype %nin% c("discrete", "discretevector",
                    "character", "charactervector")))) {
          lwarn(paste("Parameter '%s' for learner '%s' is of type '%s'",
                  "and has different (but feasible) type '%s'",
                  "listed in search space."),
              param$name, lrnid, partype, param$type)
        }
        if (param$type %nin% c("fix", "def", "fixdef") &&
            is.null(param$special) && hasRequires(lp$pars[[origParamName]]) &&
            is.null(param$req)) {
          lwarn(paste("Parameter '%s' for learner '%s' has a 'requires'",
                  "argument but the one given in the search space has not."),
              param$name, lrnid)
        }
        param
      })
}

injectParams = function(sslist, lrn) {
  lrnid = lrn$id
  lpids = getParamIds(getParamSet(lrn))
  injectParams = Filter(function(param)
        removeAmlrfix(param$name) == param$name &&
            identical(param$special, "inject"), sslist)
  badinject = intersect(extractSubList(injectParams, "name"), lpids)
  if (length(badinject)) {
    stopf(paste("Parameter(s) '%s' present in learner '%s' but also marked",
            "as 'inject' in search space."),
        collapse(badinject), lrnid)
  }
  for (param in injectParams) {
    lrn$par.set = c(lrn$par.set, makeParamSet(createParameter(param, lrnid,
                makeLearnerParam = TRUE)))
  }
  lrn
}

adjustSSDefaults = function(sslist, lrn, verbosity) {
  lrnid = lrn$id
  if (verbosity.sswarnings(verbosity)) {
    lwarn = function(...) warningf(...)
  } else {
    lwarn = function(...) {}
  }
  defaults = getDefaults(getParamSet(lrn))
  parvals = getHyperPars(lrn)
  lapply(sslist, function(param) {
        if (param$type == "def" && identical(param$values, "##")) {
          next
        }
        if (param$type %in% c("def", "fixdef")) {
          # check whether this is /actually/ the default
          truedefault = defaults[[param$name]]
          defaultcandidate = parvals[[param$name]]
          if (!is.null(defaultcandidate)) {
            # we try to use the default, but apparently the value is already set
            # in the learner object.
            if (!identical(truedefault, defaultcandidate)) {
              lwarn(paste("Parameter '%s' for learner '%s' is of type",
                      "'default', but the learner has it already set to a",
                      "different value.\nThis value hase been removed; the",
                      "default will be used."),
                  param$name, lrnid)
            }
          }
          if (!identical(truedefault, param$values)) {
            lwarn(paste("Parameter '%s' for learner '%s' is of type 'default'",
                    "but its alleged default '%s'",
                    "differs from the true default '%s'."),
                param$name, lrnid, convertToShortString(param$values),
                convertToShortString(truedefault))
            if (param$type == "fixdef") {
              param$type = "fix"
            }
          } else {
            if (param$type == "fixdef") {
              param$type = "def"
            }
          }
        }
        param
      })
}


allfeasible = function(ps, totest, name, dimension) {
  testlist = list(0)
  names(testlist) = name
  for (t in totest) {
    if (is.language(t)) {
      # bound is an expression
      # TODO in theory we could check this, with reasonable / extreme values
      # inserted for all unknowns
      next
    }
    if (getParamTypes(ps, use.names = TRUE)[[name]] == "discretevector") {
      t = list(t)
    }
    testlist[[1]] = if (dimension > 1) rep(t, dimension) else t
    if (!isFeasible(ps$pars[[name]], testlist[[1]])) {
      # this would be easier if there was a way to disable requirements
      # checking.
      return(FALSE)
    }
  }
  TRUE
}

