


options(width=150)
library("smoof")
# devtools::load_all("../../ParamHelpers")
devtools::load_all("../../mlr")
library(roxygen2)
roxygenise('..')
devtools::load_all("..")
options(error=dump.frames)


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


learner = makeLearner("classif.J48")
ho = makeResampleDesc("Holdout")
task = pid.task
measure = getDefaultMeasure(task)
tfclass = c(TRUE, FALSE)
names(tfclass) = tfclass
learner$searchspace = makeParamSet(makeNumericParam("C", 0.1, 0.9), makeDiscreteParam("O", tfclass))

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
                         par
                       })
  parset
}

objFun = function(x) {
  resample(setHyperPars(learner, par.vals=complicateParam(x, learner$searchspace)), pid.task, ho, list(measure))$aggr
}

objective = makeSingleObjectiveFunction(
    name="automlr learner optimization",
    id="automlr.objective",
    has.simple.signature=FALSE,
    vectorized=FALSE,
    noisy=TRUE,
    minimize=measure$minimize,
    par.set=simplifyParams(learner$searchspace),
    fn=objFun)

control = makeMBOControl()
control$noisy = TRUE
control$infill.opt.focussearch.points = 100
mresult = mbo(objective, control=control)

mresult

resample(learner, pid.task, ho, list(measure))$aggr


mlrMBO:::checkLearner(NULL, learner$searchspace, control)


hasDiscrete
