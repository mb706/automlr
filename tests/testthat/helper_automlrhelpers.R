
# a basic predictor that classifies everything within a radius around a certain point as positive
circleClassif = makeRLearnerClassif("circleClassif", character(0),
    makeParamSet(makeNumericLearnerParam("radius", lower = 0, default = 0, when = "predict"),
                 makeNumericVectorLearnerParam("coordinates", len = 2, default = c(0, 0), when = "train")),
    properties = c("twoclass", "numerics", "missings"))
circleClassif$fix.factors.prediction = TRUE
trainLearner.circleClassif = function(.learner, .task, .subset, .weights = NULL, coordinates, ...) {
  list(coords = coordinates)
}
predictLearner.circleClassif = function(.learner, .model, .newdata, radius, ...) {
  factor(.model$factor.levels[[1]][1 + apply(.newdata, 1, function(x) sum((x - .model$learner.model$coords)^2) > radius^2)])
}
ccAL = autolearner(circleClassif, list(
    sp("radius", "real", c(0.1, 10), "exp"),
    sp("coordinates", "real", c(-10, 10), dim = 2)))
    

# like circleClassif, but the function to use for classification is a DiscreteParam
estimfunctions = list(
    inner = function(x, coords, radius) sum((x - coords)^2) > radius^2,
    outer = function(x, coords, radius) sum((x - coords)^2) < radius^2)
circleInoutClassif = makeRLearnerClassif("circleInoutClassif", character(0),
    makeParamSet(makeNumericLearnerParam("radius", lower = 0, default = 0, when = "predict"),
                 makeNumericVectorLearnerParam("coordinates", len = 2, default = c(0, 0), when = "train"),
                 makeDiscreteLearnerParam("estimfunction", estimfunctions, when = "predict")),
    properties = c("twoclass", "numerics", "missings"))
circleInoutClassif$fix.factors.prediction = TRUE
trainLearner.circleInoutClassif = function(.learner, .task, .subset, .weights = NULL, coordinates, ...) {
  list(coords = coordinates)
}
predictLearner.circleInoutClassif = function(.learner, .model, .newdata, radius, estimfunction, ...) {
  factor(.model$factor.levels[[1]][1 + apply(.newdata, 1, function(x) estimfunction(x, .model$learner.model$coords, radius))])
}
cicAL = autolearner(circleInoutClassif, list(
    sp("radius", "real", c(0.1, 10), "exp"),
    sp("coordinates", "real", c(-10, 10), dim = 2),
    sp("estimfunction", "cat", c('inner', 'outer'))))


# like circleClassif, but instead of a circle, a 4x4 grid is used.
meshClassif = makeRLearnerClassif("meshClassif", character(0),
    makeParamSet(makeNumericLearnerParam("radius", lower = 0, default = 0, when = "predict"),
                 makeNumericVectorLearnerParam("coordinates", len = 2, default = c(0, 0), when = "train"),
                 makeDiscreteVectorLearnerParam("mesh", len = 16, list(T = list(TRUE), F = list(FALSE)), when = "predict")),
    properties = c("twoclass", "numerics", "missings"))
meshClassif$fix.factors.prediction = TRUE
trainLearner.meshClassif = function(.learner, .task, .subset, .weights = NULL, coordinates, ...) {
  list(coords = coordinates)
}
predictLearner.meshClassif = function(.learner, .model, .newdata, radius, mesh, ...) {
  pointsscaled = t(t(as.matrix(.newdata)) - .model$learner.model$coords) / radius
  mesh[[17]] = list(FALSE)
  xindex = floor(pointsscaled[, 1] * 2 + 2)
  yindex = floor(pointsscaled[, 2] * 2 + 2)
  totalIndex = yindex * 4 + xindex + 1
  totalIndex[xindex < 0 | xindex > 3 | yindex < 0 | yindex > 3] = 17
  hit = sapply(totalIndex, function(x) mesh[[x]][[1]])
  factor(.model$factor.levels[[1]][1 + !hit])
}
mcAL = autolearner(meshClassif, list(
    sp("radius", "real", c(0.1, 10), "exp"),
    sp("coordinates", "real", c(-10, 10), dim = 2),
    sp("mesh", "cat", c('T', 'F'), dim = 16)))

# like circleClassif, but fails predictably if the origin is not part of the circle
detfailClassif = makeRLearnerClassif("detfailClassif", character(0),
    makeParamSet(makeNumericLearnerParam("radius", lower = 0, default = 0, when = "predict"),
                 makeNumericVectorLearnerParam("coordinates", len = 2, default = c(0, 0), when = "train")),
    properties = c("twoclass", "numerics", "missings"))
detfailClassif$fix.factors.prediction = TRUE
trainLearner.detfailClassif = function(.learner, .task, .subset, .weights = NULL, coordinates, ...) {
  list(coords = coordinates)
}
predictLearner.detfailClassif = function(.learner, .model, .newdata, radius, ...) {
  if (sum(.model$learner.model$coords^2) > radius^2) {
    stop("origin not part of circle")
  }
  factor(.model$factor.levels[[1]][1 + apply(.newdata, 1, function(x) sum((x - .model$learner.model$coords)^2) > radius^2)])
}
dcAL = autolearner(detfailClassif, list(
    sp("radius", "real", c(0.1, 10), "exp"),
    sp("coordinates", "real", c(-10, 10), dim = 2)))

# like circleClassif, but fails predictably if the origin is not part of the circle
randfailClassif = makeRLearnerClassif("randfailClassif", character(0),
    makeParamSet(makeNumericLearnerParam("radius", lower = 0, default = 0, when = "predict"),
                 makeNumericVectorLearnerParam("coordinates", len = 2, default = c(0, 0), when = "train")),
    properties = c("twoclass", "numerics", "missings"))
randfailClassif$fix.factors.prediction = TRUE
trainLearner.randfailClassif = function(.learner, .task, .subset, .weights = NULL, coordinates, ...) {
  list(coords = coordinates)
}
predictLearner.randfailClassif = function(.learner, .model, .newdata, radius, ...) {
  if (sum((rnorm(2) - .model$learner.model$coords)^2) > radius^2) {
    stop("random point near the origin not part of circle")
  }
  factor(.model$factor.levels[[1]][1 + apply(.newdata, 1, function(x) sum((x - .model$learner.model$coords)^2) > radius^2)])
}
rcAL = autolearner(randfailClassif, list(
    sp("radius", "real", c(0.1, 10), "exp"),
    sp("coordinates", "real", c(-10, 10), dim = 2)))

preprocAL = autolearner(learner = autoWrapper(name = "ampreproc", constructor = makePreprocWrapperAm, conversion = identity), stacktype = "requiredwrapper",
    searchspace = list(
        sp("ppa.univariate.trafo", "cat", c("off", "center", "scale", "centerscale", "range"), req = quote(automlr.has.numerics == TRUE)),
        sp("ppa.multivariate.trafo", "cat", c("off", "pca", "ica"), req = quote(automlr.has.numerics == TRUE))))


tf = c(TRUE, FALSE)
names(tf) = !tf  # maybe this will cause some confusion..
# a predictor that predicts TRUE or FALSE with probability depending on its hyperparameters
noiseClassif = makeRLearnerClassif("noiseClassif", character(0),
    makeParamSet(makeIntegerLearnerParam("int", when = "predict"),
                 makeIntegerVectorLearnerParam("intv", 3, when = "both"),
                 makeNumericLearnerParam("num", when = "both"),
                 makeNumericVectorLearnerParam("numv", 3, when = "predict"),
                 makeLogicalLearnerParam("log", when = "predict"),
                 makeLogicalVectorLearnerParam("logv", 3, when = "both"),
                 makeDiscreteLearnerParam("disc1", c("a", "b", "c"), when = "both"),  # easy: character discrete params
                 makeDiscreteVectorLearnerParam("disc1v", 3, c("a", "b", "c"), when = "predict"),
                 makeDiscreteLearnerParam("disc2", tf, when = "predict"),  # harder: booleans
                 makeDiscreteVectorLearnerParam("disc2v", 3, tf, when = "both"),
                 makeDiscreteLearnerParam("disc3", c(3, 10), when = "both"),  # also harder: numeric
                 makeDiscreteVectorLearnerParam("disc3v", 3, c(3, 10), when = "predict"),
                 # challenge: mixed types
                 makeDiscreteLearnerParam("disc4", list(`3` = 3, `TRUE` = "TRUE", `FALSE` = TRUE, li = list(TRUE, FALSE), fun = function() TRUE), when = "both"),  
                 makeDiscreteVectorLearnerParam("disc4v", 3, list(`3` = 3, `TRUE` = "TRUE", `FALSE` = TRUE, li = list(TRUE, FALSE)), when = "predict"),
                 makeLogicalLearnerParam("often", default = FALSE, when = "predict",
                                         requires = quote(int > -10 && mean(intv) > -10 &&
                                                          num - abs(numv[1]) < numv[2] + numv[3] &&
                                                            (log || logv[1] || logv[2] || logv[3]) &&
                                                              disc1 %in% unlist(disc1v) &&
                                                                (disc2 || disc2v[[1]] || disc2v[[2]] || disc2v[[3]]) &&
                                                                  disc3 * min(unlist(disc3v)) < 100 &&
                                                                    if(is.function(disc4)) TRUE else as.character(unlist(disc4)[1])==TRUE &&
                                                                      sum(unlist(disc4v) == "TRUE") > 2)),
    # remember that functions in requirements do not work and probably will never work
                 makeLogicalLearnerParam("seldom", default = FALSE, when = "predict",
                                         requires = quote(int > 0 && mean(intv) > 0 &&
                                                          num - numv[1] < numv[2] + numv[3] &&
                                                            (log || logv[1] == logv[2] || logv[3]) &&
                                                              disc1 %nin% unlist(disc1v) &&
                                                                (disc2 || disc2v[[1]] || disc2v[[2]] || disc2v[[3]]) &&
                                                                  disc3 * min(unlist(disc3v)) < 100 &&
                                                                    if(is.function(disc4)) TRUE else as.character(unlist(disc4)[1])==TRUE &&
                                                                      sum(unlist(disc4v) == "TRUE") > 2)),
                 makeLogicalLearnerParam("testReqs", default = FALSE, when = "predict", tunable = FALSE)),
    properties = c("twoclass", "numerics", "missings"))
noiseClassif$fix.factors.prediction = TRUE

trainLearner.noiseClassif = function(.learner, .task, .subset, .weights = NULL, ...) {
  list()
}
predictLearner.noiseClassif = function(.learner, .model, .newdata, testReqs = FALSE,
    int, intv, num, numv, log, logv, disc1, disc1v, disc2, disc2v, disc3, disc3v, disc4, disc4v, ...) {
  expect_numeric(int, len = 1, any.missing = FALSE)
  expect_numeric(intv, len = 3, any.missing = FALSE)
  expect_numeric(num, len = 1, any.missing = FALSE)
  expect_numeric(numv, len = 3, any.missing = FALSE)
  expect_logical(log, len = 1, any.missing = FALSE)
  expect_logical(logv, len = 3, any.missing = FALSE)
  expect_character(disc1, len = 1, any.missing = FALSE)
  expect_list(disc1v, types = "character", any.missing = FALSE, len = 3)
  expect_logical(disc2, len = 1, any.missing = FALSE)
  expect_list(disc2v, types = "logical", any.missing = FALSE, len = 3)
  expect_numeric(disc3, len = 1, any.missing = FALSE)
  expect_list(disc3v, types = "numeric", any.missing = FALSE, len = 3)
  assert(!identical(disc4, FALSE))
  bar = mean(c(int > 0, intv[1] + intv[2] + intv[3] > 0,
      num > 0, mean(numv), log == TRUE, logv[1] && logv[2] || logv[3],
      disc1 == "a", disc1v[[1]] == disc1v[[2]], disc2 == TRUE,
      disc2v[[1]] && disc2v[[2]], disc3 == 3, disc3v[[1]] * disc3v[[2]] > 10))
  if (testReqs) {
    moreArgs = list(...)
    oftenEval = eval(noiseClassif$par.set$pars$often$requires)
    seldomEval = eval(noiseClassif$par.set$pars$seldom$requires)
    expect_identical(oftenEval, "often" %in% names(moreArgs))
    expect_identical(seldomEval, "seldom" %in% names(moreArgs))
    if ("seldom" %in% names(moreArgs)) {
      cat("seldom\n")
    }
  }
  factor(.model$factor.levels[[1]][1 + (runif(nrow(.newdata)) > bar)])
}

trivialParams = list(
    sp("int", "int", c(-5, 5)),
    sp("intv", "int", c(-5, 5), dim = 3),
    sp("num", "real", c(-5, 5)),
    sp("numv", "real", c(-5, 5), dim = 3),
    sp("log", "bool"),
    sp("logv", "bool", dim = 3),
    sp("disc1", "cat", c("a", "b", "c")),
    sp("disc1v", "cat", c("a", "b", "c"), dim = 3),
    sp("disc2", "cat", c("TRUE", "FALSE")),
    sp("disc2v", "cat", c(TRUE, FALSE), dim = 3),
    sp("disc3", "cat", c(3, 10)),
    sp("disc3v", "cat", c(3, 10), dim = 3),
    sp("disc4", "cat", c(3, "TRUE", "FALSE", "li", "fun")),
    sp("disc4v", "cat", c(3, "TRUE", "FALSE", "li"), dim = 3))



noiseCL = autolearner(noiseClassif, c(trivialParams, list(
    sp("often", "def", FALSE, req = quote(1==1)),
    sp("seldom", "def", FALSE, req = quote(1==1)),
    sp("testReqs", "def", FALSE))))

reqsCL = autolearner(noiseClassif, c(trivialParams, list(
    sp("often", "bool", req = getParamSet(noiseClassif)$pars$often$requires),
    sp("seldom", "bool", req = getParamSet(noiseClassif)$pars$seldom$requires),
    sp("testReqs", "fix", TRUE))))

#### to visualise what is happening:
plotres = function(res) {
  data = res$data$response
  plot(getTaskData(predicttask), pch = ifelse(data == "yes", 'x', '.'))
}
traintask = makeClassifTask(id = 'dummy', data = data.frame(x = c(1, 2), y = c(1, 2), incirc = factor(c('yes', 'no'))), target = 'incirc')
predicttask = makeClusterTask(id = 'lotsapoints', data = data.frame(x = runif(1000, -1, 1), y = runif(1000, -1, 1)))
function()  plotres(predict(train(setHyperPars(circleClassif, radius = 0.9, coordinates = c(.1, .2)), traintask), predicttask))
function()  plotres(predict(train(setHyperPars(circleInoutClassif, radius = 0.3, coordinates = c(.1, .2), estimfunction = estimfunctions[[2]]), traintask), predicttask))
function()  plotres(predict(train(setHyperPars(meshClassif, radius = 0.5, coordinates = c(-.1, .2), mesh = list(
                                                                list(F), list(T), list(T), list(F),
                                                                list(T), list(T), list(T), list(T),
                                                                list(T), list(T), list(T), list(T),
                                                                list(F), list(T), list(T), list(F))), traintask), predicttask))
#### end visualization





generateCircleTask = function(id, n, x, y, radius, coordinates = c(x, y)) {
  data = matrix(runif(n * 2, -10, 10), ncol = 2)
  isCircle = apply(data, 1, function(p) sum((p-coordinates)^2) < radius^2)
  makeClassifTask(id, data.frame(x = data[, 1], y = data[, 2], c = factor(c("no", "yes")[1 + isCircle], levels = c("yes", "no"))), target = 'c')
}

checkLegitAMResult = function(amobject, budget, budgettest) {
  expect_class(amresult <- amfinish(amobject), c("AMObject", "AMResult"))
  expect_subset(c("opt.path", "opt.point", "opt.val", "result"), names(amresult))
  if (budgettest == 'evals') {
    expect_lte(budget, getOptPathLength(amresult$opt.path))
  }
  
}

checkSpentVsBudget = function(amobject, budget, budgettest, runtime) {
  if (budgettest %in% c("walltime", "modeltime")) {
    expect_lte(budget, runtime + 1)
  }
  expect_lte(budget, amobject$spent[budgettest])    
  expect_lte(amobject$spent['modeltime'], amobject$spent['walltime'])
  expect_lte(amobject$spent['walltime'], amobject$spent['cputime'])
  expect_lte(amobject$spent['walltime'], runtime + 1)
}

checkBackend = function(searchSpaceToTest, backendToTest) {
  typicalBudget = list(walltime = 30, cputime = 30, modeltime = 2, evals = 10)
  amfile = tempfile()
  for (methodOfContinuation in c("file", "object")) {
    for (budgettest in c("walltime", "cputime", "modeltime", "evals")) {
      budget = typicalBudget[[budgettest]]
      names(budget) = budgettest

      starttime = Sys.time()
      amobject = automlr(theTask, searchspace = searchSpaceToTest, backend = backendToTest, budget = budget, savefile = amfile)
      runtime = as.numeric(difftime(Sys.time(), starttime, units = "secs"))
      expect_gt(as.numeric(difftime(file.mtime(amobject$savefile), starttime, units = "secs")) + 1, 0) # see if the file was modified, prevent rounding errors

      checkLegitAMResult(amobject, budget, budgettest)
      checkSpentVsBudget(amobject, budget, budgettest, runtime)

      continueObject = switch(methodOfContinuation,
          file = amfile, object = amobject)

      spentBefore = amobject$spent
      budgetBefore = budget
      budget = budget + spentBefore[budgettest]

      starttime = Sys.time()
      amobject = automlr(continueObject, budget = budget)
      runtime = runtime + as.numeric(difftime(Sys.time(), starttime, units = "secs"))
      expect_gt(as.numeric(difftime(file.mtime(amobject$savefile), starttime, units = "secs")) + 1, 0)  # see again if the file was modified

      expect_equal(amobject$backend, backendToTest)

      checkLegitAMResult(amobject, budget, budgettest)    
      checkSpentVsBudget(amobject, budget, budgettest, runtime)

      for (property in c('modeltime', 'walltime', 'cputime', 'evals')) {
        expect_lte(spentBefore[property], amobject$spent[property])
      }
    }
  }
  try(file.remove(amobject$savefile), silent = TRUE)
}

nofailSearchSpace = list(ccAL, cicAL, mcAL)
withFailSearchSpace = list(ccAL, cicAL, mcAL, dcAL, rcAL)
withPPSearchSpace = list(ccAL, cicAL, mcAL, dcAL, rcAL, preprocAL)
paramtestSearchSpace = list(noiseCL)
reqstestSearchSpace = list(mcAL, reqsCL)

theTask = generateCircleTask('circle', 200, 1, 2, 3)
