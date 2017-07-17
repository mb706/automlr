
#################################
# mbo shims                     #
#################################

# mostly because mbo doesn't handle parameters well that are neither numeric nor
# categorical.

# the whole stack down: simplify requirements, types, vectors
simplifyParams = function(parset) {
  parset = mboRequirements(parset)
  parset = untypeParams(parset)
  parset = unvectorParams(parset)
  parset
}

# after generating concrete parameters using the simplifyParams() simplified
# parameters, convert these back so the learner receives them as the original
# parameter set.
complicateParams = function(params, origparset) {
  simpleTypeOrig = untypeParams(origparset)

  types = getParamTypes(simpleTypeOrig, df.cols = TRUE)
  for (parin in seq_along(params)) {
    params[[parin]] = switch(types[parin],
        integer = as.integer,
        numeric = as.numeric,
        factor = as.character,
        stop("complicateParam got bad type"))(params[[parin]])
  }
  
  params = as.data.frame(params, stringsAsFactors = FALSE)
  params = dfRowsToList(params, simpleTypeOrig)[[1]]
  params = removeMissingValues(params)
  
  ret = lapply(names(params), function(parname) {
        vals = origparset$pars[[parname]]$values
        type = origparset$pars[[parname]]$type
        par = params[[parname]]
        if (is.null(vals)) {
          # don't change anything
          return(par)
        }
        switch(type,
            logicalvector = unlist(vals[unlist(par)]),
            logical = vals[[par]],
            discretevector = lapply(par, function(x) vals[[x]]),
            discrete = vals[[par]])
      })
  names(ret) = names(params)
  ret
}

# convert discrete to discrete-string parameters
# also convert logical to discrete parameters
untypeParams = function(parset) {
  parset$pars = lapply(parset$pars, function(par) {
        if (!is.null(par$values)) {
          par$values = as.list(names(par$values))
          names(par$values) = par$values
          par$type = switch(par$type,
              logical = "discrete",
              logicalvector = "discretevector",
              par$type)
        }
        par
      })
  parset
}

# convert vector parameters to multiple nonvector parameters
unvectorParams = function(parset) {
  parset$pars = unlist(recursive = FALSE, lapply(parset$pars, function(par) {
            if (!ParamHelpers::isVector(par)) {
              return(list(par))
            }
            lapply(seq_len(par$len), function(i) {
                  parcpy = par
                  parcpy$len = 1L
                  parcpy$id = paste0(parcpy$id, i)
                  if (!is.null(parcpy$lower)) {
                    parcpy$lower = parcpy$lower[i]
                  }
                  if (!is.null(parcpy$upper)) {
                    parcpy$upper = parcpy$upper[i]
                  }
                  parcpy$type = switch(parcpy$type,
                      integervector = "integer",
                      numericvector = "numeric",
                      discretevector = "discrete",
                      stopf("Unsupported parameter type '%s'", parcpy$type))
                  parcpy
                })
          }))
  parset
}

# adapt requirements to parameter simplification we are doing above.
mboRequirements = function(searchspace) {
  replacements = list()
  for (param in searchspace$pars) {
    type = param$type
    if (type %in% c("numeric", "integer") ||
        (type == "discrete" &&
          all(sapply(param$values, test_character, any.missing = FALSE)))) {
      # int, num and character nonvector were not affected
      next
    }
    replaceStr = "c(%s)"
    if (ParamHelpers::isVector(param)) {
      if (!test_integer(param$len, len = 1, lower = 1, any.missing = FALSE)) {
        stopf("Parameter '%s' is a vector param with undefined length'",
            param$id)
      }
      replaceStr = sprintf(replaceStr,
          paste0(param$id, seq_len(param$len), collapse = ", "))
    } else {
      replaceStr = sprintf(replaceStr, param$id)
    }
    replaceQuote = asQuoted(replaceStr)
    if (isDiscrete(param)) {
      objectText = capture.output(dput(param$values))
      fullObject = try(asQuoted(collapse(objectText, sep = "\n")),
          silent = TRUE)
      if (is.error(fullObject)) {
        fullObject = substitute(stop(sprintf(
                    "Parameter %s cannot be used in requirements.", pname)),
            list(pname = param$id))
      }
      insert =  list(fullObject = fullObject, index = replaceQuote)
      template = switch(type,
          # we index into fullObject to get a list
          discretevector = quote(fullObject[index]),
          # unlike for 'discretevector', we want to get a vector out of this.
          logicalvector =  quote(unlist(fullObject[index])),
          # index is a single element, and we want to get a single element.
          quote(fullObject[[index]]))
      replaceQuote = do.call(substitute, list(template, insert))
    }
    replacements[[param$id]] = replaceQuote
  }
  for (param in names(searchspace$pars)) {
    req = searchspace$pars[[param]]$requires
    if (is.null(req)) {
      next
    }
    searchspace$pars[[param]]$requires = replaceRequires(req, replacements)
  }
  searchspace
}

#################################
# irace shims                   #
#################################

# handle requirements and multidimensional parameters

# irace treats everything that is not a number as a string, which breaks our
# requirements. Here we repair these requirements by turning <logical> into
# (<logical> == TRUE) and <discrete> into discreteValuesList[<discrete>].
# We also handle vectors.
iraceRequirements = function(searchspace) {
  replacements = list()
  for (param in searchspace$pars) {
    type = param$type
    if (type == "discrete" &&
        all(sapply(param$values, test_character, any.missing = FALSE))) {
      # irace handles character discrete vectors well, so go right through
      next
    }
    replaceStr = switch(type,
        numeric = "%s",
        numericvector = "c(%s)",
        integer = "as.integer(%s)",
        integervector = "as.integer(c(%s))",
        logical = "(%s == 'TRUE')",
        logicalvector = "(c(%s) == 'TRUE')",
        discrete = "%s",
        discretevector = "c(%s)",
        stopf("Unsupported type '%s' of parameter '%s'.", type, param$id))
    if (ParamHelpers::isVector(param)) {
      if (!test_numeric(param$len, len = 1, lower = 1, any.missing = FALSE)) {
        stopf("Parameter '%s' is a vector param with undefined length'",
            param$id)
      }
      replaceStr = sprintf(replaceStr,
          paste0(param$id, seq_len(param$len), collapse = ", "))
    } else {
      replaceStr = sprintf(replaceStr, param$id)
    }
    replaceQuote = asQuoted(replaceStr)
    
    if (type %in% c("discrete", "discretevector")) {
      assertList(param$values, names = "named")
      objectText = capture.output(dput(param$values))
      fullObject = try(asQuoted(collapse(objectText, sep = "")), silent = TRUE)
      if (!is.error(fullObject)) {
        
        
      }
      if (is.error(fullObject) || length(all.vars(fullObject)) > 0) {
        # irace does not like '\n' in their requirements.
        # but it DOES accept 'eval(parse("\\n"))'
        # this also fixes the problem that parameters in lists of functions show
        # up on 'all.vars' even though they shouldn't, for irace's sake.
        objectText = collapse(objectText, sep = "\n")
        fullObject = asQuoted(paste0("eval(parse(text = ",
                capture.output(dput(objectText)), "))"))
      }
      if (type == "discrete") {
        replaceQuote = substitute(fullObject[[index]],
            list(fullObject = fullObject, index = replaceQuote))
      } else { # discretevector
        replaceQuote = substitute(fullObject[index],
            list(fullObject = fullObject, index = replaceQuote))
      }
    }
    replacements[[param$id]] = replaceQuote
  }
  for (param in names(searchspace$pars)) {
    req = searchspace$pars[[param]]$requires
    if (is.null(req)) {
      next
    }
    searchspace$pars[[param]]$requires = replaceRequires(req, replacements)
  }
  searchspace
}

# This function is a slightly modified version of the convertParamSet.R file in 
# the ParamHelpers project (https://github.com/berndbischl/ParamHelpers).
#
# This file was distributed with a BSD 3 clause license as follows. Changes that
# I made are shown in the git history of this file.
#
# Copyright (c) 2013-2014, Bernd Bischl, Michel Lang, Daniel Horn
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
# 
#     Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in
#     the documentation and/or other materials provided with the
#     distribution.
# 
#     Neither the name of the TU Dortmund University nor the names of its
#     contributors may be used to endorse or promote products derived
#     from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

convertParamSetToIrace = function(par.set, as.chars = FALSE) {
  assertClass(par.set, "ParamSet")
  assertFlag(as.chars)
  if (!is.null(par.set$forbidden))
    stopf("Operation not allowed for param set with forbidden region currently!")
  if (!hasFiniteBoxConstraints(par.set))
    stop("convertParamSetToIrace requires finite box constraints for all numeric and integer params!")
  requirePackages("irace", why = "convertParamSetToIrace", default.method = "load")
  lines = character(0)
  count = 1L
  for (i in seq_along(par.set$pars)) {
    p = par.set$pars[[i]]
    type = switch(
              p$type,
              numeric = "r",
              numericvector = "r",
              integer = "i",
              integervector = "i",
              discrete = "c",
              discretevector = "c",
              logical = "c",
              logicalvector = "c",
              ordered = "o"
    )
    for (j in seq_len(p$len)) {
      id = if (p$len == 1) p$id else paste(p$id, j, sep = "")
      if (p$type %in% c("numeric", "numericvector"))
        line = sprintf('%s "" %s (%g, %g)', id, type, p$lower[j], p$upper[j])
      else if (p$type %in% c("integer", "integervector"))
        line = sprintf('%s "" %s (%i, %i)', id, type, p$lower[j], p$upper[j])
      else if (p$type %in% c("discrete", "discretevector", "logical", "logicalvector")) {
        v = paste("\"", names(p$values), "\"", sep = "")
        line = sprintf('%s "" %s (%s)', id, type, collapse(v))
      } else  {
        stopf("Unknown parameter type: %s", p$type)
      }
      if (!is.null(p$requires)) {
        line = paste(line, collapse(capture.output(p$requires), sep=""), sep = " | ")
      }
      lines[count] = line
      count = count + 1L
    }
  }
  if (as.chars) {
    return(lines)
  } else {
    lines = collapse(lines, "\n")
    params = irace::readParameters(text = lines, digits = .Machine$integer.max)
    # assert that the boundaries have the correct class and values
    for (i in seq_along(par.set$pars)) {
      if (par.set$pars[[i]]$type %in% c("numeric", "numericvector"))
        params$boundary[[i]] = as.numeric(unlist(par.set$pars[[i]][c("lower", "upper")]))
      else if (par.set$pars[[i]]$type %in% c("integer", "integervector"))
        params$boundary[[i]] = as.integer(params$boundary[[i]])
    }
    return(params)
  }
}
