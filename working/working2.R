


options(width=150)
devtools::load_all("../../ParamHelpers")
devtools::load_all("../../mlr")
devtools::load_all("../../smoof")
library('testthat')

library(roxygen2)
roxygenise('..')

devtools::load_all("..")
options(error=dump.frames)

##### Testing automlr


library('testthat')
devtools::test(pkg="..", filter="paramhandling")



##### mlrMBO

devtools::load_all("../../mlrMBO")

control = makeMBOControl(
    number.of.targets=1,  # single crit optimization
    propose.points=1,  # number of proposed points per iter
    final.method="best.true.y",  # final point to return
    final.evals = 0,  # evaluate at the end to get a more exact result
    y.name="y",
    impute.y.fun=NULL,  # if evaluation fails, this value is used instead
    trafo.y.fun=NULL,   # trafo y value before modeling, e.g. log transform time
    suppres.eval.erros=TRUE,  # ignored
    save.on.disk.at=integer(0),  # TODO: the saving mechanism of mbo seems clumsy, work around it.
    save.on.disk.at.time=Inf,  # TODO: ditto
    save.file.path=file.path(getwd(), "mlr_run.RData"),  # TODO ditto
    store.model.at=NULL,  # TODO: ditto
    resample.at=integer(0),  # resample the model, we don't need that
    resample.desc=makeResampleDesc("CV", iter=10),  # ignored
    resample.measures=list(mse),  #ignored
    output.num.format="%.3g")

# so we can use the defaults, what we might want to change is
#  - impute.y.fun give some maximally bad value? Look at the snoek paper how he does it with restricted regions

# setMBOControlInfill - only for multipoint
# setMBOControlTermination time.budget, exec.time.budget, max.evals, set iters to NULL

control = makeMBOControl()
control$noisy = FALSE
fn = makeBraninFunction()

ex = exampleRun(fn, control=control)

ex
plotExampleRun(ex)

ctrl = makeMBOControl()
ctrl$noisy = TRUE
ctrl = setMBOControlTermination(ctrl, iters=1)
mborun = mbo(fn, control=ctrl)

##### start here

learner = makeLearner("classif.rFerns")
task = pid.task
measure = getDefaultMeasure(task)
learner$searchspace = makeParamSet(makeIntegerParam("depth", 1, 13), makeIntegerParam("ferns", 100, 4000))

# learner$config = insert(learner$config, list(show.learner.output=FALSE, on.learner.warning="quiet"))
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

objFun = function(x) {
  resample(setHyperPars(learner, par.vals=complicateParams(x, learner$searchspace)), pid.task, resDesc, list(measure), show.info=FALSE)$aggr
}

resDesc = makeResampleDesc("CV")
simpleParset = simplifyParams(learner$searchspace)


objective = makeSingleObjectiveFunction(
    name="automlr learner optimization",
    id="automlr.objective",
    has.simple.signature=FALSE,
    vectorized=FALSE,
    noisy=TRUE,
    minimize=measure$minimize,
    par.set=simpleParset,
    fn=objFun)

control = makeMBOControl()
control = setMBOControlInfill(control, opt="focussearch", opt.focussearch.points = 1000)

# We want to have termination criteria for:
# walltime, cputime, modeltime, evals
# mbo gives us:
# as.numeric(getOptStateTimeUsed(opt.state), units="secs")  # 'time budget'
# sum(getOptPathExecTimes(getOptStateOptPath(opt.state)))  # 'execution time'
# getOptPathLength(getOptStateOptPath(opt.state)) # 'evals'



control = setMBOControlTermination(control, iters=NULL, more.stop.conds=list(function(opt.state) list(term=TRUE, message="automlr term", code="iter")))
names(control)

mboLearner = checkLearner(NULL, simpleParset, control)
mboLearner$config = list(on.learner.error="stop", on.learner.warning="quiet", show.learner.output=FALSE)

noFinalize = new.env(parent=asNamespace("mlrMBO"))
noFinalize$mboFinalize2 = identity
myMBO = mlrMBO::mbo
environment(myMBO) = noFinalize
mresult = myMBO(objective, learner=mboLearner, control=control, show.info=FALSE)
rm(myMBO)

mresult = mlrMBO:::mboTemplate(mresult)

mlrMBO:::mboFinalize2(mresult)

getOptPathLength(mresult$opt.path)
sum(getOptPathExecTimes(mresult$opt.path))
mresult$time.used

mresult$state

debugonce(warningf)
mresult

resample(learner, pid.task, ho, list(measure))$aggr

debug(generateDesign)

trace(generateDesign)
untrace(generateDesign)

mlrMBO:::checkLearner(NULL, learner$searchspace, control)


hasDiscrete



fun2 = function(y) {
  r = y + 2
  r = r / 2
  r
}

trace(fun2)
untrace(fun2)

fun1 = function(x) {
  r = fun2(x)
  r + 1
}

debugonce(fun2)

fun1(10)

##### testing optMBO

options(warn=2)  # stop on warnings


devtools::load_all("..")


aobj = automlr(pid.task, searchspace=list(ccAL), backend="irace")
aobj2 = automlr(aobj, budget=c(evals=1))
debugonce(mlr:::tuneIrace)


aobj = automlr(pid.task, searchspace=automlr:::autolearners[c('classif.rFerns', 'classif.knn')], backend="irace")
aobj
aobj2 = automlr(aobj, budget=c(evals=17))

res = amfinish(aobj2)

print(res, longform=TRUE)


debugonce(amsetup.ammbo)
debugonce(isOutOfBudget)

debugonce(aobj$backendprivatedata$runtimeEnv$isOutOfBudget)

names(aobj)
ls(aobj$backendprivatedata)


debug(mlr:::trainLearner.regr.randomForest)

aobj3 = automlr(aobj2, budget=c(evals=20))




aobj3$spent

debug(mlrMBO:::trainModels)

untrace(randomForest::randomForest)
debug(randomForest::randomForest)

mlrMBO:::opt
aobj3$backendprivatedata$opt.state$opt.path

as.data.frame(aobj2$backendprivatedata$opt.state$opt.path)
as.data.frame(aobj3$backendprivatedata$opt.state$opt.path)

aobj3
names(aobj2$backendprivatedata)
as.data.frame(aobj2$backendprivatedata$opt.path)
as.data.frame(aobj3$backendprivatedata$opt.path)

res = mlrMBO::mboFinalize2(aobj2$backendprivatedata$opt.state)
names(res)
res$y

aobj2$previous.version



nf = makeRegrTask("nofeat", data=data.frame(x=c(100, 101, 102, 103, 109)), target='x')
mean(getTaskData(nf)[['x']])
td = train(makeLearner("regr.randomForest"), nf)
predict(td, nf)


tf = c(TRUE, FALSE)
names(tf) = !tf  # maybe this will cause some confusion..
# a predictor that predicts TRUE or FALSE with probability depending on its hyperparameters
noiseClassif = makeRLearnerClassif("noiseClassif", character(0),
    makeParamSet(makeIntegerLearnerParam("int", when="predict"),
                 makeIntegerVectorLearnerParam("intv", 3, when="both"),
                 makeNumericLearnerParam("num", when="both"),
                 makeNumericVectorLearnerParam("numv", 3, when="predict"),
                 makeLogicalLearnerParam("log", when="predict"),
                 makeLogicalVectorLearnerParam("logv", 3, when="both"),
                 makeDiscreteLearnerParam("disc1", c("a", "b", "c"), when="both"),  # easy: character discrete params
                 makeDiscreteVectorLearnerParam("disc1v", 3, c("a", "b", "c"), when="predict"),
                 makeDiscreteLearnerParam("disc2", tf, when="predict"),  # harder: booleans
                 makeDiscreteVectorLearnerParam("disc2v", 3, tf, when="both"),
                 makeDiscreteLearnerParam("disc3", c(3, 10), when="both"),  # also harder: numeric
                 makeDiscreteVectorLearnerParam("disc3v", 3, c(3, 10), when="predict"),
                 # challenge: mixed types
                 makeDiscreteLearnerParam("disc4", list(`3`=3, `TRUE`="TRUE", `FALSE`=TRUE, li=list(TRUE, FALSE), fun=function() TRUE), when="both"),  
                 makeDiscreteVectorLearnerParam("disc4v", 3, list(`3`=3, `TRUE`="TRUE", `FALSE`=TRUE, li=list(TRUE, FALSE)), when="predict"),
                 makeLogicalLearnerParam("often", default=FALSE, when="predict",
                                         requires=quote(int > -10 && mean(intv) > -10 &&
                                                          num - abs(numv[1]) < numv[2] + numv[3] &&
                                                            (log || logv[1] || logv[2] || logv[3]) &&
                                                              disc1 %in% unlist(disc1v) &&
                                                                (disc2 || disc2v[[1]] || disc2v[[2]] || disc2v[[3]]) &&
                                                                  disc3 * min(unlist(disc3v)) < 100 &&
                                                                    if(is.function(disc4)) TRUE else as.character(unlist(disc4)[1])==TRUE &&
                                                                      sum(unlist(disc4v) == "TRUE") > 2)),
    # remember that functions in requirements do not work and probably will never work
                 makeLogicalLearnerParam("seldom", default=FALSE, when="predict",
                                         requires=quote(int > 0 && mean(intv) > 0 &&
                                                          num - numv[1] < numv[2] + numv[3] &&
                                                            (log || logv[1] == logv[2] || logv[3]) &&
                                                              disc1 %nin% unlist(disc1v) &&
                                                                (disc2 || disc2v[[1]] || disc2v[[2]] || disc2v[[3]]) &&
                                                                  disc3 * min(unlist(disc3v)) < 100 &&
                                                                    if(is.function(disc4)) TRUE else as.character(unlist(disc4)[1])==TRUE &&
                                                                      sum(unlist(disc4v) == "TRUE") > 2)),
                 makeLogicalLearnerParam("testReqs", default=FALSE, when="predict", tunable=FALSE)),
    properties=c("twoclass", "numerics", "missings"))
noiseClassif$fix.factors.prediction = TRUE

trainLearner.noiseClassif = function(.learner, .task, .subset, .weights=NULL, ...) {
  list()
}
predictLearner.noiseClassif = function(.learner, .model, .newdata, testReqs=FALSE,
    int, intv, num, numv, log, logv, disc1, disc1v, disc2, disc2v, disc3, disc3v, disc4, disc4v, ...) {
  expect_numeric(int, len=1, any.missing=FALSE)
  expect_numeric(intv, len=3, any.missing=FALSE)
  expect_numeric(num, len=1, any.missing=FALSE)
  expect_numeric(numv, len=3, any.missing=FALSE)
  expect_logical(log, len=1, any.missing=FALSE)
  expect_logical(logv, len=3, any.missing=FALSE)
  expect_character(disc1, len=1, any.missing=FALSE)
  expect_list(disc1v, types="character", any.missing=FALSE, len=3)
  expect_logical(disc2, len=1, any.missing=FALSE)
  expect_list(disc2v, types="logical", any.missing=FALSE, len=3)
  expect_numeric(disc3, len=1, any.missing=FALSE)
  expect_list(disc3v, types="numeric", any.missing=FALSE, len=3)
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
    sp("intv", "int", c(-5, 5), dim=3),
    sp("num", "real", c(-5, 5)),
    sp("numv", "real", c(-5, 5), dim=3),
    sp("log", "bool"),
    sp("logv", "bool", dim=3),
    sp("disc1", "cat", c("a", "b", "c")),
    sp("disc1v", "cat", c("a", "b", "c"), dim=3),
    sp("disc2", "cat", c("TRUE", "FALSE")),
    sp("disc2v", "cat", c(TRUE, FALSE), dim=3),
    sp("disc3", "cat", c(3, 10)),
    sp("disc3v", "cat", c(3, 10), dim=3),
    sp("disc4", "cat", c(3, "TRUE", "FALSE", "li", "fun")),
    sp("disc4v", "cat", c(3, "TRUE", "FALSE", "li"), dim=3))



noiseCL = autolearner(noiseClassif, c(trivialParams, list(
    sp("often", "def", FALSE, req=quote(1==1)),
    sp("seldom", "def", FALSE, req=quote(1==1)),
    sp("testReqs", "def", FALSE))))

reqsCL = autolearner(noiseClassif, c(trivialParams, list(
    sp("often", "bool", req=getParamSet(noiseClassif)$pars$often$requires),
    sp("seldom", "bool", req=getParamSet(noiseClassif)$pars$seldom$requires),
    sp("testReqs", "fix", TRUE))))


l <- buildLearners(list(noiseCL), pid.task)

l <- buildLearners(list(reqsCL), pid.task)
class(l$searchspace$pars$noiseClassif.often$requires)


ps = removeMissingValues(sampleValues(l$searchspace, 1)[[1]])
tl = train(setHyperPars(l, par.vals=ps), pid.task)
predict(tl, pid.task)

aobj = automlr(pid.task, searchspace=list(noiseCL), backend="random")

aobj = automlr(pid.task, searchspace=list(reqsCL), backend="irace")

aobj = automlr(pid.task, searchspace=list(reqsCL), backend="mbo")

aobj2 = automlr(aobj, budget=c(evals=100))
aobj2$spent
sum(getOptPathCol(amfinish(aobj2)$opt.path, 'exec.time'))
sum(getOptPathExecTimes(amfinish(aobj2)$opt.path))
sum(getOptPathExecTimes(makeOp))
aobj3 = automlr(aobj, budget=c(evals=1000))
aobj3$spent
mean(getTaskData(pid.task)$diabetes == 'pos')

ps = amfinish(aobj2)$opt.point
op = amfinish(aobj2)$opt.path
aobj2$spent

print(amfinish(aobj2), longform=TRUE)

ps
oop = order(as.data.frame(op)$mmce.test.mean)
plot(as.data.frame(op)[oop, 'dob'])

convertParamSetToIrace(makeParamSet(makeIntegerParam("C1", lower=0, upper=3), makeIntegerVectorParam("C", len=3, lower=0, upper=3)))

devtools::load_all("..")
options(error=dump.frames)


ss = automlr:::iraceRequirements(l$searchspace)



ss$pars[[13]]$requires
ss$pars[[14]]$requires

l$searchspace$pars$noiseClassif.intv$type

debugonce(automlr:::iraceRequirements)

