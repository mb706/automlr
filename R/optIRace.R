

# 'random' has no prior, so do nothing here
amcombinepriors.amirace = function(prior, newprior) {
  NULL
}

# 'random' has no prior, so do nothing here
amgetprior.amirace = function(env) {
  NULL
}

# no bells and whistles here either
amsetup.amirace = function(env, prior, learner, task) {
  env$learner = learner
  env$tc = NULL  # TODO makeTuneControlRandom()  # TODO: how do we set a time constraint?
  invisible()
}

# TODO: return whatever the user might be interested in
amresult.amirace = function(env) {
  list(resultstring="The returning result stuff is under construction.")  # TODO
}

# now this is where the fun happens
amoptimize.amirace = function(env, stepbudget) {
  NULL # TODO
}