


options(width=150)
library("smoof")
# devtools::load_all("../../ParamHelpers")
devtools::load_all("../../mlr")
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

devtools::load_all("..")

aobj = automlr(pid.task, searchspace=automlr:::autolearners[c('classif.rFerns', 'classif.knn')], backend="irace")
aobj
aobj2 = automlr(aobj, budget=c(evals=17))

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

