
# 'random' has no prior, so do nothing here
combinepriors.random = function(prior, newprior) {
  NULL
}

# 'random' has no prior, so do nothing here
extractprior.random = function(env) {
  NULL
}

# no bells and whistles here either
setup.random = function(env, prior, learner) {
  env$learner = learner
  env$tc = makeTuneControlRandom()  # TODO: how do we set a time constraint?
  invisible()
}

# TODO: return whatever the user might be interested in
result.dummy = function(env) {
  list(resultstring="The returning result stuff is under construction.")
}

# now this is where the fun happens
optimize.dummy = function(env, stepbudget) {

  c(walltime=10, cputime=10, modeltime=10, evals=1)
}

# filter out the tunable params, transform to finite interval
compactParamSet = function(pset) {
  pset = filterParams(pset, tunable=TRUE)
  pset = dropParams(pset, getParamIds(pset)[is.na(getParamLengths(pset))])  # idk what to do with <NA> length vectors
  
}