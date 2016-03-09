
# reduce types of parameter set to simple types to avoid mlrMBO bugs
mboSaveMode = TRUE  

amaddprior.ammbo = function(env, prior) {
  NULL
}

amgetprior.ammbo = function(env) {
  NULL
}

amsetup.ammbo = function(env, prior, learner, task, measure) {
  requirePackages("mlrMBO", why = "optMBO", default.method = "load")
  requirePackages("smoof", why = "optMBO", default.method = "load")
  # FIXME things that could be variable:
  #  resampling: holdout, cv, or something adaptive?
  #  infill control: focussearch, something else? how many points?
  env$runtimeEnv = environment()
  
  zeroWalltime = 0
  zeroModeltime = 0
  zeroEvals = 0
  
  isOutOfBudget = function(opt.state) {
    stopcondition(budget, spentBudget(opt.state, parent.env(environment())))
  }
  
  objectiveFun = function(x) {
    x = removeMissingValues(x)
    if (mboSaveMode) {
      x = complicateParams(x, learner$searchspace)
    }
    l = setHyperPars(learner, par.vals = x)
    resample(l, task, resDesc, list(measure), show.info = FALSE)$aggr
  }

  usedParset = learner$searchspace
  if (mboSaveMode) {
    usedParset = simplifyParams(mboRequirements(usedParset))
  }

  resDesc = makeResampleDesc("Holdout")
  objective = smoof::makeSingleObjectiveFunction(
      name = "automlr learner optimization",
      id = "automlr.objective",
      has.simple.signature = FALSE,
      vectorized = FALSE,
      noisy = TRUE,
      minimize = measure$minimize,
      par.set = usedParset,
      fn = objectiveFun)
  
  imputeval = generateRealisticImputeVal(measure, learner, task)
  imputefun = function(x, y, opt.path) imputeval

  control = mlrMBO::makeMBOControl(impute.y.fun = imputefun)
  control = mlrMBO::setMBOControlInfill(control, opt = "focussearch",
      opt.focussearch.points = 1000)
  control = mlrMBO::setMBOControlTermination(control, iters = NULL,
      more.stop.conds = list(function(opt.state) {
            if (isOutOfBudget(opt.state)) {
              list(term = TRUE, message = "automlr term", code = "iter")
            } else {
              list(term = FALSE, message = NA_character_, code = "iter")
            }
          }))
  

  
  mboLearner = mlrMBO:::checkLearner(NULL, usedParset, control)
  mboLearner$config = list(on.learner.error = "stop",
      on.learner.warning = "warn", show.learner.output = TRUE)
  mboLearner = makePreprocWrapperAm(mboLearner, ppa.impute.factor = "distinct",
      ppa.impute.numeric = "median")
  
  myMBO = mlrMBO::mbo
  environment(myMBO) = new.env(parent = asNamespace("mlrMBO"))
  environment(myMBO)$mboFinalize2 = identity
  env$opt.state = myMBO(objective, learner = mboLearner, control = control,
      show.info = TRUE)
  # clean up environment, it is used in objectiveFun().
  rm(myMBO, prior, env)
}


amresult.ammbo = function(env) {
  mboResult = mlrMBO:::mboFinalize2(env$opt.state)
  list(opt.point = removeMissingValues(mboResult$x),
      opt.val = mboResult$y,
      opt.path = mboResult$opt.path,
      result = mboResult)
}

amoptimize.ammbo = function(env, stepbudget) {
  # initialize for spent budget computation
  zero = env$runtimeEnv
  zero$numcpus = parallelGetOptions()$settings$cpus
  zero$numcpus[is.na(zero$numcpus)] = 1
  zero$budget = stepbudget

  env$opt.state = mlrMBO:::mboTemplate.OptState(env$opt.state)

  spent = spentBudget(env$opt.state, zero)
  zero$zeroWalltime %+=% spent["walltime"]
  zero$zeroModeltime %+=% spent["modeltime"]
  zero$zeroEvals %+=% spent["evals"]

  spent
}

spentBudget = function(opt.state, zero) {
  spent = numeric(0)
  totalWallTime = as.numeric(opt.state$time.used, units = "secs")
  totalModelTime = sum(getOptPathExecTimes(opt.state$opt.path))
  spent["walltime"] = totalWallTime - zero$zeroWalltime
  spent["cputime"] = spent["walltime"] * zero$numcpus
  spent["modeltime"] = totalModelTime - zero$zeroModeltime
  spent["evals"] = getOptPathLength(opt.state$opt.path) - zero$zeroEvals
  spent
}

# convert discrete to discrete-string parameters
# also convert logical to discrete parameters
simplifyParams = function(parset) {
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

# undo the 'simplifyParams' operation
complicateParams = function(params, origparset) {
  ret = lapply(names(params), function(parname) {
        par = params[[parname]]
        if (is.null(vals)) {
          # don't change anything
          return(par)
        }
        vals = origparset$pars[[parname]]$values
        type = origparset$pars[[parname]]$type
        switch(type,
            logicalvector = unlist(vals[unlist(par)]),
            logical = vals[[par]],
            discretevector = lapply(par, function(x) vals[[x]]),
            discrete = vals[[par]])
      })
  names(ret) = names(params)
  ret
}

# adapt requirements to parameter simplification we are doing above.
mboRequirements = function(searchspace) {
  replacements = list()
  for (param in searchspace$pars) {
    if (!isDiscrete(param)) {
      next
    }
    fullObject = capture.output(dput(param$values))
    fullObject = asQuoted(collapse(fullObject, sep = ""))
    insert =  list(fullObject = fullObject, index = asQuoted(param$id))
    template = switch(param$type,
        # index is a list -- we turn it into a vector and index into fullObject
        # to get a list
        discretevector = quote(fullObject[unlist(index)]),
        # index is also a list because to mlrMBO, logicalvector looks like
        # discretevector. however unlike for 'discretevector', we want to get a
        # logical vector out of this.
        logicalvector =  quote(unlist(fullObject[unlist(index)])),
        # index is a single element, and we want to get a single element.
        quote(fullObject[[index]]))
    replacements[[param$id]] = do.call(substitute, list(template, insert))
  }
  for (param in names(searchspace$pars)) {
    req = searchspace$pars[[param]]$requires
    if (is.null(req)) {
      next
    }
    newreq = replaceRequires(req, replacements)
    searchspace$pars[[param]]$requires = deExpression(newreq)
  }
  searchspace
}

