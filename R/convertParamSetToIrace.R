
# This file is a slightly modified version of the convertParamSet.R file in 
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
