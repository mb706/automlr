

amcombinepriors.ammbo = function(prior, newprior) {
  NULL
}

amgetprior.ammbo = function(env) {
  NULL
}

amsetup.ammbo = function(env, prior, learner, task, measure) {
  env$learner = learner
  env$task = task
  env$measure = measure
  
  control = makeMBOControl(
      number.of.targets=1,  # single crit optimization
      propose.points=1,  # number of proposed points per iter
      final.method="best.true.y",  # final point to return
      y.name="y",
      impute.y.fun=NULL,
      trafo.y.fun=NULL,
      suppres.eval.erros=TRUE,
      save.on.disk.at=integer(0),  # TODO: the saving mechanism of mbo seems clumsy, work around it.
      save.on.disk.at.time=Inf,  # TODO: ditto
      save.file.path=file.path(getwd(), "mlr_run.RData"),  # TODO ditto
      store.model.at=NULL,  # TODO: ditto
      resample.at=integer(0),
      resample.desc=makeResampleDesc("Holdout"),  # how does this work?
      resample.measures=list(measure),
      output.num.format="%.3g")
}

# TODO: return whatever the user might be interested in
amresult.ammbo = function(env) {
  list(resultstring="The returning result stuff is under construction.") # TODO
}

# now this is where the fun happens
amoptimize.ammbo = function(env, stepbudget) {
  NULL # TODO
}