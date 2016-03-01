

amaddprior.ammbo = function(env, prior) {
  NULL
}

amgetprior.ammbo = function(env) {
  NULL
}

amsetup.ammbo = function(env, prior, learner, task, measure) {
  # things to adapt:
  #  resampling: holdout, cv, or something adaptive?
  #  infill control: focussearch, something else? how many points?
  env$runtimeEnv = environment()
  
  zeroWalltime = 0
  zeroModeltime = 0
  zeroEvals = 0
  numcpus = parallelGetOptions()$settings$cpus
  numcpus = ifelse(is.na(numcpus), 1, numcpus)
  
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
  objective = makeSingleObjectiveFunction(
      name="automlr learner optimization",
      id="automlr.objective",
      has.simple.signature=FALSE,
      vectorized=FALSE,
      noisy=TRUE,
      minimize=measure$minimize,
      par.set=simpleParset,
      fn=objectiveFun)
  
  control = setMBOControlInfill(control, opt="focussearch", opt.focussearch.points = 1000)
  control = setMBOControlTermination(control, iters=NULL, more.stop.conds=list(function(opt.state) {
            if (isOutOfBudget(opt.state)) {
              list(term=TRUE, message="automlr term", code="iter")
            } else {
              list(term=FALSE, message=NA_character_, code="iter")
            }
          }))
  
  mboLearner = mlrMBO::checkLearner(NULL, simpleParset, control)
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
  zero$numcpus = ifelse(is.na(zero$numcpus), 1, zero$numcpus)
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
        vals = origparset$pars[[parname]]$values
        if (!is.null(vals) && params[[parname]] %in% names(vals)) {
          return(vals[[params[[parname]]]])
        }
        params[[parname]]
      })
  names(ret) = names(params)
  ret
}

