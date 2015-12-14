
# 'random' has no prior, so do nothing here
amcombinepriors.amrandom = function(prior, newprior) {
  NULL
}

# 'random' has no prior, so do nothing here
amgetprior.amrandom = function(env) {
  NULL
}

# no bells and whistles here either
amsetup.amrandom = function(env, prior, learner) {
  env$learner = learner
  env$tc = makeTuneControlRandom()  # TODO: how do we set a time constraint?
  invisible()
}

# TODO: return whatever the user might be interested in
amresult.amrandom = function(env) {
  list(resultstring="The returning result stuff is under construction.")
}

# now this is where the fun happens
amoptimize.amrandom = function(env, stepbudget) {
  NULL  # TODO
}

# filter out the tunable params, transform to finite interval
compactParamSet = function(pset) {
  pset = filterParams(pset, tunable=TRUE)
  pset = dropParams(pset, getParamIds(pset)[is.na(getParamLengths(pset))])  # idk what to do with <NA> length vectors
  
}