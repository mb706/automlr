

# 'random' has no prior, so do nothing here
amcombinepriors.ammbo = function(prior, newprior) {
  NULL
}

# 'random' has no prior, so do nothing here
amgetprior.ammbo = function(env) {
  NULL
}

# no bells and whistles here either
amsetup.ammbo = function(env, prior, learner, task, measure) {
  env$learner = learner
  env$tc = NULL  # TODO makeTuneControlRandom()  # TODO: how do we set a time constraint?
  invisible()
}

# TODO: return whatever the user might be interested in
amresult.ammbo = function(env) {
  list(resultstring="The returning result stuff is under construction.") # TODO
}

# now this is where the fun happens
amoptimize.ammbo = function(env, stepbudget) {
  NULL # TODO
}