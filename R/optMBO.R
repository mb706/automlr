

amaddprior.ammbo = function(env, prior) {
  NULL
}

amgetprior.ammbo = function(env) {
  NULL
}

amsetup.ammbo = function(env, prior, learner, task, measure) {
  requirePackages("mlrMBO", why = "optMBO", default.method = "load")
  requirePackages("smoof", why = "optMBO", default.method = "load")
  # things to adapt:
  #  resampling: holdout, cv, or something adaptive?
  #  infill control: focussearch, something else? how many points?
  env$runtimeEnv = environment()
  
  zeroWalltime = 0
  zeroModeltime = 0
  zeroEvals = 0
  numcpus = parallelGetOptions()$settings$cpus
  numcpus[is.na(numcpus)] = 1
  
  budget = 0
  
  isOutOfBudget = function(opt.state) {
    stopcondition(budget, spentBudget(opt.state, parent.env(environment())))
  }
  
  objectiveFun = function(x) {
    l = setHyperPars(learner, par.vals=removeMissingValues(complicateParams(x, learner$searchspace)))
    resample(l, task, resDesc, list(measure), show.info=FALSE)$aggr
  }
  
  simpleParset = simplifyParams(learner$searchspace)
  resDesc = makeResampleDesc("Holdout")
  objective = smoof::makeSingleObjectiveFunction(
      name="automlr learner optimization",
      id="automlr.objective",
      has.simple.signature=FALSE,
      vectorized=FALSE,
      noisy=TRUE,
      minimize=measure$minimize,
      par.set=simpleParset,
      fn=objectiveFun)

  control = mlrMBO::makeMBOControl()
  control = mlrMBO::setMBOControlInfill(control, opt="focussearch", opt.focussearch.points = 1000)
  control = mlrMBO::setMBOControlTermination(control, iters=NULL, more.stop.conds=list(function(opt.state) {
            if (isOutOfBudget(opt.state)) {
              list(term=TRUE, message="automlr term", code="iter")
            } else {
              list(term=FALSE, message=NA_character_, code="iter")
            }
          }))
  
  mboLearner = mlrMBO:::checkLearner(NULL, simpleParset, control)
  mboLearner$config = list(on.learner.error="stop", on.learner.warning="warn", show.learner.output=TRUE)
  mboLearner = makePreprocWrapperAm(mboLearner, ppa.impute.factor="distinct", ppa.impute.numeric="median")
  
  myMBO = mlrMBO::mbo
  environment(myMBO) = new.env(parent=asNamespace("mlrMBO"))
  environment(myMBO)$mboFinalize2 = identity
  env$opt.state = myMBO(objective, learner=mboLearner, control=control, show.info=TRUE)
  rm(myMBO)
  
  rm(prior, env)
}


amresult.ammbo = function(env) {
  mboResult = mlrMBO:::mboFinalize2(env$opt.state)
  
  list(opt.point=removeMissingValues(mboResult$x),
      opt.value=mboResult$y,
      opt.path=mboResult$opt.path,
      result=mboResult)
}

# now this is where the fun happens
amoptimize.ammbo = function(env, stepbudget) {
  zero = env$runtimeEnv
  zero$numcpus = parallelGetOptions()$settings$cpus
  zero$numcpus[is.na(zero$numcpus)] = 1
  zero$budget = stepbudget

  env$opt.state = mlrMBO:::mboTemplate(env$opt.state)

  spent = spentBudget(env$opt.state, zero)
  zero$zeroWalltime = spent["walltime"]
  zero$zeroModeltime = spent["modeltime"]
  zero$zeroEvals = spent["evals"]

  spent
}

spentBudget = function(opt.state, zero) {
  spent = numeric(0)
  spent["walltime"] = as.numeric(opt.state$time.used, units="secs") - zero$zeroWalltime
  spent["cputime"] = spent["walltime"] * zero$numcpus
  spent["modeltime"] = sum(getOptPathExecTimes(opt.state$opt.path)) - zero$zeroModeltime
  spent["evals"] = getOptPathLength(opt.state$opt.path) - zero$zeroEvals
  spent
}

# convert discrete to discrete-string parameters
# also convert logical to discrete parameters
simplifyParams = function(parset) {
  parset$pars = lapply(parset$pars, function(par) {
        if (!is.null(par$values)) {
          par$values = names(par$values)
          names(par$values) = par$values
        }
        if (par$type == "logical") {
          par$type = "discrete"
        }
        if (par$type == "logicalvector") {
          par$type = "discretevector"
        }
        par
      })
  parset
}

# undo the 'simplifyParams' operation
complicateParams = function(params, origparset) {
  ret = lapply(names(params), function(parname) {
        if (origparset$pars[[parname]]$type == "logicalvector") {
          params[[parname]] = unlist(params[[parname]])
        }
        vals = origparset$pars[[parname]]$values
        if (!is.null(vals)) {
          if (origparset$pars[[parname]]$type == "discretevector") {
            return(lapply(params[[parname]], function(x) vals[[x]]))
          }
          if (origparset$pars[[parname]]$type == "discrete") {
            return(vals[[params[[parname]]]])
          }
          return(unlist(vals[params[[parname]]]))
        }
        params[[parname]]
      })
  names(ret) = names(params)
  ret
}

simplifyParams.ifMBOWasntBroken = function(parset) {
  parset$pars = lapply(parset$pars, function(par) {
        if (par$type == "logical") {
          par$type = "discrete"
        }
        if (par$type == "logicalvector") {
          par$type = "discretevector"
        }
        par
      })
  parset
}

complicateParams.ifMBOWasntBroken = function(params, origparset) {
  ret = lapply(names(params), function(parname) {
        if (origparset$pars[[parname]]$type == "logicalvector") {
          unlist(params[[parname]])
        } else {
          params[[parname]]
        }
      })
  names(ret) = names(params)
  ret
}


# adapt requirements to parameter simplification we are doing above.
mboRequirements = function(searchspace) {
  # TODO
  replacements = list()
  for (param in searchspace$pars) {
    if (param$type %in% c("discrete", "discretevector")) {
      if (param$type == "discrete" && all(sapply(param$values, test_character, any.missing=FALSE))) {
        # irace handles character discrete vectors well, so go right through
        next
      }
      assertList(param$values, names="named")
      fullObject = asQuoted(collapse(capture.output(dput(param$values)), sep=""))
      if (param$type == "discrete") {
        replacements[[param$id]] = substitute(fullObject[[index]], list(fullObject=fullObject, index=asQuoted(param$id)))
      } else { # discretevector
        if (!test_numeric(param$len, len=1, lower=1, any.missing=FALSE)) {
          stopf("Parameter '%s' is a vector param with undefined length'", param$id)
        }
        paramvec = asQuoted(sprintf("c(%s)", paste0(param$id, seq_len(param$len), collapse=", ")))
        replacements[[param$id]] = substitute(fullObject[index], list(fullObject=fullObject, index=paramvec))
      }
    } else {
      replacePattern = switch(param$type,
          numeric=NULL,
          numericvector="c(%s)",
          integer="as.integer(%s)",
          integervector="as.integer(c(%s))",
          logical="(%s == TRUE)",
          logicalvector="(c(%s) == TRUE)",
          # the following code should not be reachable as of yet
          charactervector="c(%s)", # since we don't generate character vectors
          if (param$type %nin% c("function", "untyped")) { # ditto, hopefully
            stopf("Unknown type '%s' of parameter '%s'.", param$type, param$id)
          })
      if (is.null(replacePattern)) {
        # nothing to do
        next
      }
      if (param$type %in% c("numericvector", "integervector", "logicalvector", "charactervector")) {
        if (!test_numeric(param$len, len=1, lower=1, any.missing=FALSE)) {
          stopf("Parameter '%s' is a vector param with undefined length'", param$id)
        }
        replaceStr = sprintf(replacePattern, paste0(param$id, seq_len(param$len), collapse=", "))
      } else {
        replaceStr = sprintf(replacePattern, param$id)
      }
      replacements[[param$id]] = asQuoted(replaceStr)
    }
  }
  for (param in names(searchspace$pars)) {
    req = searchspace$pars[[param]]$requires
    if (is.null(req)) {
      next
    }
    searchspace$pars[[param]]$requires = deExpression(replaceRequires(req, replacements))
  }
  searchspace
}

