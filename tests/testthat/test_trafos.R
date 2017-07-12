
lrn = makeRLearnerClassif("testtrafo", character(0),
  paramSetSugar(int1: integer[0, 1000], real1: numeric[0, 1000]), list(),
  c("twoclass", "multiclass", "numerics"))
trainLearner.testtrafo = function(.learner, .task, .subset, .weights = NULL, ...) {
  list(...)
}
registerS3method("trainLearner", "testtrafo", trainLearner.testtrafo)

getargs = function(learner, params, task = pid.task) {
  m = train(setHyperPars(learner, par.vals = params), task)
  m$learner.model$learner.model$next.model$learner.model$next.model$learner.model
}

test_that("exp and invexp trafo do not change learner paramset", {
  expect_equal(train(lrn, pid.task)$learner.model, list())
  expect_equal(train(setHyperPars(lrn, int1 = 2, real1 = 3), pid.task)$learner.model, list(int1 = 2, real1 = 3))

  al = list(autolearner(lrn,
    list(sp("int1", "int", c(1, 100)),
      sp("real1", "int", c(1, 100)))))

  expect_warning(blrn <- buildLearners(al, pid.task, 6), "real1' for learner 'testtrafo' is of type 'numeric'", all = TRUE)

  expect_equal(getParamSet(blrn)$pars[[1]]$lower, 1)
  expect_equal(getParamSet(blrn)$pars[[1]]$upper, 100)
  expect_equal(getParamSet(blrn)$pars[[2]]$lower, 1)
  expect_equal(getParamSet(blrn)$pars[[2]]$upper, 100)

  expect_equal(getargs(blrn, list(testtrafo.int1 = 50, testtrafo.real1 = 40)),
    list(int1 = 50, real1 = 40))


  al = list(autolearner(lrn,
    list(sp("int1", "int", c(1, 100)),
      sp("real1", "real", c(1, 100)))))

  blrn = buildLearners(al, pid.task, 6)

  expect_equal(getParamSet(blrn)$pars[[1]]$lower, 1)
  expect_equal(getParamSet(blrn)$pars[[1]]$upper, 100)
  expect_equal(getParamSet(blrn)$pars[[2]]$lower, 1)
  expect_equal(getParamSet(blrn)$pars[[2]]$upper, 100)

  expect_equal(getargs(blrn, list(testtrafo.int1 = 50, testtrafo.real1 = 40.5)),
    list(int1 = 50, real1 = 40.5))


  al = list(autolearner(lrn,
    list(sp("int1", "int", c(1, 100), "exp"),
      sp("real1", "real", c(1, 100), "exp"))))

  blrn = buildLearners(al, pid.task, 6)

  expect_equal(getParamSet(blrn)$pars[[1]]$lower, 1)
  expect_equal(getParamSet(blrn)$pars[[1]]$upper, 100)
  expect_equal(getParamSet(blrn)$pars[[2]]$lower, 1)
  expect_equal(getParamSet(blrn)$pars[[2]]$upper, 100)

  expect_equal(getargs(blrn, list(testtrafo.int1 = 50, testtrafo.real1 = 40.5)),
    list(int1 = 50, real1 = 40.5))

  al = list(autolearner(lrn,
    list(sp("int1", "int", c(1, 100)),
      sp("real1", "real", c(0, .99), "invexp"))))

  blrn = buildLearners(al, pid.task, 6)

  expect_equal(getParamSet(blrn)$pars[[1]]$lower, 1)
  expect_equal(getParamSet(blrn)$pars[[1]]$upper, 100)
  expect_equal(getParamSet(blrn)$pars[[2]]$lower, 0)
  expect_equal(getParamSet(blrn)$pars[[2]]$upper, .99)

  expect_equal(getargs(blrn, list(testtrafo.int1 = 50, testtrafo.real1 = .3)),
    list(int1 = 50, real1 = .3))
})

test_that("exp and invexp trafo add searchspace trafo", {

  al = list(autolearner(lrn,
    list(sp("int1", "int", c(1, 100), "exp"),
      sp("real1", "int", c(1, 100), "exp"))))

  expect_warning(blrn <- buildLearners(al, pid.task, 6), "real1' for learner 'testtrafo' is of type 'numeric'", all = TRUE)
  expect_equal(blrn$searchspace$pars[[1]]$lower, 1)
  expect_equal(blrn$searchspace$pars[[1]]$upper, 14)
  expect_equal(blrn$searchspace$pars[[2]]$lower, 1)
  expect_equal(blrn$searchspace$pars[[2]]$upper, 14)

  trafod = trafoValue(blrn$searchspace, list(testtrafo.int1 = 1:14, testtrafo.real1 = 1:14))
  expect_equal(trafod$testtrafo.int1[14], 100)
  expect_equal(trafod$testtrafo.int1[1], 1)
  expect_true(trafod$testtrafo.int1[6] < 50)
  expect_equal(trafod$testtrafo.real1[14], 100)
  expect_equal(trafod$testtrafo.real1[1], 1)
  expect_true(trafod$testtrafo.real1[6] < 50)


  al = list(autolearner(lrn,
    list(sp("int1", "int", c(1, 100), "exp"),
      sp("real1", "real", c(1, 100), "exp"))))

  blrn = buildLearners(al, pid.task, 6)
  expect_equal(blrn$searchspace$pars[[1]]$lower, 1)
  expect_equal(blrn$searchspace$pars[[1]]$upper, 14)
  expect_equal(blrn$searchspace$pars[[2]]$lower, 0)
  expect_equal(blrn$searchspace$pars[[2]]$upper, 1)

  trafod = trafoValue(blrn$searchspace, list(testtrafo.int1 = 1:14, testtrafo.real1 = c(0, .5, 1)))
  expect_equal(trafod$testtrafo.int1[14], 100)
  expect_equal(trafod$testtrafo.int1[1], 1)
  expect_true(trafod$testtrafo.int1[6] < 50)
  expect_equal(trafod$testtrafo.real1[1], 1)
  expect_equal(trafod$testtrafo.real1[3], 100)
  expect_equal(trafod$testtrafo.real1[2], 10)

  al = list(autolearner(lrn,
    list(sp("int1", "int", c(1, 100)),
      sp("real1", "real", c(0, .99), "invexp"))))

  blrn = buildLearners(al, pid.task, 6)
  expect_equal(blrn$searchspace$pars[[2]]$lower, 0)
  expect_equal(blrn$searchspace$pars[[2]]$upper, 1)

  trafod = trafoValue(blrn$searchspace, list(testtrafo.int1 = 1, testtrafo.real1 = c(0, .5, 1)))
  expect_equal(trafod$testtrafo.real1[1], 0)
  expect_equal(trafod$testtrafo.real1[3], .99)
  expect_equal(trafod$testtrafo.real1[2], 0.9)

})


test_that("expression bounds with and without trafo", {

  al = list(autolearner(lrn,
    list(sp("int1", "int", c(1, quote(n))),
      sp("real1", "real", c(1, quote(p))))))

  blrn = buildLearners(al, pid.task, 6)

  expect_equal(getargs(blrn, list(testtrafo.int1 = 0))$int1, 1)
  expect_equal(getargs(blrn, list(testtrafo.int1 = 1))$int1, getTaskSize(pid.task))
  expect_equal(getargs(blrn, list(testtrafo.int1 = 1), subsetTask(pid.task, 1:10))$int1, 10)
  expect_equal(getargs(blrn, list(testtrafo.int1 = 0.5), subsetTask(pid.task, 1:9))$int1, 5)

  expect_equal(getargs(blrn, list(testtrafo.real1 = 0))$real1, 1)
  expect_equal(getargs(blrn, list(testtrafo.real1 = 1))$real1, length(getTaskFeatureNames(pid.task)))
  expect_equal(getargs(blrn, list(testtrafo.real1 = 1), subsetTask(pid.task, features = 1:2))$real1, 2)
  expect_equal(getargs(blrn, list(testtrafo.real1 = 0.5), subsetTask(pid.task, features = 1:2))$real1, 1.5)


  al = list(autolearner(lrn,
    list(sp("int1", "int", c(1, quote(round(n * PARAM.real1)))),
      sp("real1", "real", c(0, 100)))))

  blrn = buildLearners(al, pid.task, 6)

  expect_equal(getargs(blrn, list(testtrafo.int1 = 0, testtrafo.real1 = 1))$int1, 1)
  expect_equal(getargs(blrn, list(testtrafo.int1 = 1, testtrafo.real1 = 1))$int1, getTaskSize(pid.task))
  expect_equal(getargs(blrn, list(testtrafo.int1 = 1, testtrafo.real1 = 0.5))$int1, getTaskSize(pid.task) / 2)
  expect_equal(getargs(blrn, list(testtrafo.int1 = 1, testtrafo.real1 = 2), subsetTask(pid.task, 1:10))$int1, 20)
  expect_equal(getargs(blrn, list(testtrafo.int1 = 0.5, testtrafo.real1 = 3), subsetTask(pid.task, 1:3))$int1, 5)


  al = list(autolearner(lrn,
    list(sp("int1", "int", c(1, quote(n)), "exp"),
      sp("real1", "real", c(1, quote(p)), "exp"))))

  blrn = buildLearners(al, pid.task, 6)

  expect_equal(getargs(blrn, list(testtrafo.int1 = 0))$int1, 1)
  expect_equal(getargs(blrn, list(testtrafo.int1 = 1))$int1, getTaskSize(pid.task))
  expect_equal(getargs(blrn, list(testtrafo.int1 = 1), subsetTask(pid.task, 1:10))$int1, 10)
  expect_true(getargs(blrn, list(testtrafo.int1 = 0.5))$int1 < getTaskSize(pid.task))

  xx = sapply((1:30)/100, function(x) getargs(blrn, list(testtrafo.int1 = x))$int1)
  zz = sapply((1:30)/100, function(x)
    getargs(blrn, list(testtrafo.int1 = x), subsetTask(pid.task, 1:10))$int1)

  expect_equal(getargs(blrn, list(testtrafo.real1 = 0))$real1, 1)
  expect_equal(getargs(blrn, list(testtrafo.real1 = 1))$real1, length(getTaskFeatureNames(pid.task)))
  expect_equal(getargs(blrn, list(testtrafo.real1 = 1), subsetTask(pid.task, features = 1:2))$real1, 2)
  expect_true(getargs(blrn, list(testtrafo.real1 = 0.5), subsetTask(pid.task, features = 1:2))$real1 < 1.5)

  al = list(autolearner(lrn,
    list(sp("int1", "int", c(0, quote(n)), "exp"),
      sp("real1", "real", c(1, quote(p)), "exp"))))
  blrn = buildLearners(al, pid.task, 6)
  expect_equal(getargs(blrn, list(testtrafo.int1 = 0))$int1, 0)
  expect_equal(getargs(blrn, list(testtrafo.int1 = 1))$int1, getTaskSize(pid.task))
  expect_equal(getargs(blrn, list(testtrafo.int1 = 1), subsetTask(pid.task, 1:10))$int1, 10)
  expect_true(getargs(blrn, list(testtrafo.int1 = 0.5))$int1 < getTaskSize(pid.task))

  yy = sapply((1:30)/100, function(x) getargs(blrn, list(testtrafo.int1 = x))$int1)

  zz = sapply((1:30)/100, function(x)
    getargs(blrn, list(testtrafo.int1 = x), subsetTask(pid.task, 1:10))$int1)

})
