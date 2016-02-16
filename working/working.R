##### init

options(width=150)
devtools::load_all("../../ParamHelpers")
devtools::load_all("../../mlr")
library(roxygen2)
roxygenise('..')
devtools::load_all("..")  # this veritable package itself
options(error=dump.frames)

##### preProcess

ret <- preProcess(getTaskData(iris.task), univariate.trafo="centerscale", nzv.cutoff.numeric=0.5, multivariate.trafo="ica")
all.equal(predict(ret, getTaskData(iris.task)), ret$debugdata)

ret$scale
ret$center
dat <- getTaskData(iris.task)
dat[ret$cols.numeric] <- as.matrix(getTaskData(iris.task)[ret$cols.numeric]) %*% ret$rotation
ret$rotation
dat

##### autolearners
names(automlr::autolearners)
automlr::autolearners[[1]]
automlr::autolearners$classif.logreg$learner

l = buildLearners(automlr::autolearners, iris.task)
l = setHyperPars(l, selected.learner="classif.plr", classif.plr.lambda=0, classif.plr.cp.type="bic")

l
debugger()
0
m = train(l, iris.task)
debugonce(mlr:::trainLearner.classif.plr)
debugonce(stepPlr::plr)

base::set.seed(10)
s = ParamHelpers::generateRandomDesign(10, l$searchspace, FALSE)
base::set.seed(10)
s = ParamHelpers::generateRandomDesign(100, l$searchspace, TRUE)

l$searchspace
l$searchspace$pars$classif.sparseLDA.lambda$trafo(0.1)

environment(l$searchspace$pars$classif.sparseLDA.lambda$trafo)$min

##### printAllGiven

printAllGiven = function(s) {
for (line in seq_len(nrow(s))) {
  print(removeMissingValues(s[line, ]))
}
}

##### Trafn

t1 = automlr:::createTrafo(1e-10, 1, FALSE)$trafo
t2 = automlr:::createTrafo(0, 1, FALSE)$trafo
  environment(t1)$min

removeMissingValues(s[1, ])

##### all AutoLearners

lbig = buildLearners(automlr:::autolearners, iris.task)

lbig$searchspace$pars$classif.lqa.lambda$requires

automlr:::allfeasible(getParamSet(makeLearner("classif.lqa")), c(1, 100), "gamma", 1)
isFeasible(getParamSet(makeLearner("classif.lqa"))$pars[['gamma']], 1)

getParamSet(makeLearner("classif.lqa"))$pars[['gamma']]$lower - 1

s = ParamHelpers::generateRandomDesign(1000, lbig$searchspace, TRUE)

s
printAllGiven(s)

quote({a; b})

lbig$searchspace$pars$classif.lqa.lambda1$requires

##### expressions, quotes

funcallmatchReverse = "(?:\\.AUTOMLR_TEMP_((?:[[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|(`)\\.AUTOMLR_TEMP_((?:[^`\\\\]|\\\\.)+`))([[:blank:]]*\\()"
parse(text=gsub(funcallmatchReverse, "\\2\\1\\3\\4", text), keep.source=FALSE)

({x = 1 ; y = 2 ; x + y})
x0 = expression({a + b; b+c}, a+c)
ex = parse(text=deparse(x0))
do.call(deparse, list(x0))
deparse(x0)
ex
do.call(substitute, list(ex, list(a=asQuoted("A"), b=asQuoted("B"))))

lapply(ex, function(x) do.call(substitute, list(x, list(a=quote(A)))))

A=quote(1)

parse(text=deparse(parse(text=deparse(expression(a=1, b=2, a+b)))))

parse(text=deparse(parse(text=deparse(expression(a=1, b=2, a+b), control=c("keepInteger", "keepNA"))), control=c("keepInteger", "keepNA")))

toQuote = function(exp) {
  exp = as.expression(exp)
}

B=eval(eval(parse(text=c("expression(quote({", "a+a", "}))"))))
B

substitute((a) && (b), list(a=A, b=B))

as.expression(B)

lbig$searchspace$pars$classif.glmboost.mstop$requires

##### ModelMultiplexer Requirements

mod = makeLearner("classif.svm")

mmod = makeModelMultiplexer(list(mod))

ps = makeParamSet(makeDiscreteParam("type", values=c("nu-classification",  "C-classification")),
  makeNumericParam("nu", .001, 1, 1, requires=quote(type == "nu-classification")))
generateRandomDesign(10, ps)

debugonce(automlr:::makeModelMultiplexerParamSetEx)

psx = automlr:::makeModelMultiplexerParamSetEx(mmod, list(classif.svm=ps))

psx$pars$classif.svm.nu$requires
lbig$searchspace$pars$classif.svm.nu$requires


automlr:::allfeasible(getParamSet(makeLearner("classif.bdk")), c(0, 1), "alpha", 2)

devtools::load_all("..")  # this veritable package itself
lbig = buildLearners(automlr:::autolearners, iris.task)

##### small buildLearners

lsmall = buildLearners(list(automlr::autolearners$classif.glmboost), iris.task)

generateRandomDesign(10, lsmall$searchspace)

ps = BBmisc::extractSubList(lbig$base.learners, "properties")
Filter(function(x) ("ordered" %in% x), ps)
length(allfacs)
Filter(function(x) "ordered" %in% x, allfacs)

##### listWrapperCombinations

listWrapperCombinations = function(ids, required) {
  requiredIDs = ids[required]
  unlist(sapply(seq_along(ids), function(l) apply(expand.grid(rep(list(ids), l)), 1, function(x)
        if (all(requiredIDs %in% x) && all(!duplicated(x))) paste(x, collapse="$"))))
}

listWrapperCombinations(c("a", "b", "c"), c(F, T, T))
duplicated(ids)
ids = c('a', 'b', 'c')
rep(list(ids), 1)


conva = function(x) x
convb = function(x) c(x, "")
wrappers = list(
  a=list(required=TRUE, id="a", conversion=convb, searchspace=makeParamSet(makeLogicalParam("a1", req=quote(TRUE == automlr.remove.factors)))))
taskdesc = getTaskDescription(iris.task)
idRef = list()
canHandleX = list(
  missings=c("a", "b"),
  factors=c("a", "c", "d"),
  ordered=c("a", "c"))


##### makeAMExoWrapper

devtools::load_all("..")  # this veritable package itself
res = automlr:::makeAMExoWrapper(NULL, wrappers, taskdesc, idRef, c("missings", "factors", "ordered", "numerics"), canHandleX)
names(res)

res$wrappers$a$searchspace$pars$a1$requires
res$newparams

debugger()
0

##### Small wrapped list loading and executing
smallAL = makeNamedAlList(
  automlr::autolearners$ampreproc,
  autolearner("classif.probit"),
  autolearner("classif.rotationForest",
              list(
                sp("K", "real", c(2, 40), trafo=function(x) max(1, round(sum(info$n.feat) / x))),
                sp("L", "int", c(25, 100)))),
  autolearner("classif.glmboost",
              list(
                sp("family", "cat", c("AdaExp", "Binomial")),
                sp("mstop", "int", c(25, 400), "exp", req=quote(m == "mstop")),
                sp("mstop.AMLRFIX1", "cat", c(400), dummy=TRUE, req=quote(m != "mstop")),
                sp("nu", "real", c(.001, .3), "exp"),
                sp("risk", "cat", c("inbag", "oobag", "none")),
                sp("stopintern", "bool"),
                sp("m", "cat", c("mstop", "cv")),
                sp("center", "def", FALSE),
                sp("trace", "def", FALSE))))

devtools::load_all("..")  # this veritable package itself
vse = buildLearners(smallAL, pid.task)
as.list(environment(vse$searchspace$pars$classif.glmboost.nu$trafo))
vse$fixedParams
vse$shadowparams

sapply(vse$searchspace$pars, function(x) x$requires)

getParamSet(vse)
vse$staticParams
getLearnerProperties(makeLearner("classif.probit"))

getParamSet(vse)

t = train(vse, bc.task)
names(t)
predict(t$learner.model$learner.model$next.model, bc.task)
predict(t, pid.task)
predict(t$learner.model, bc.task)
names(x)
t$learner$fix.factors.prediction

t$learner.model$learner$fix.factors.prediction


getParamSet(vse)$pars$classif.rotationForest.K$trafo(5)

vsx = setHyperPars(vse,
  selected.learner="classif.glmboost",
  classif.glmboost.family=getParamSet(vse)$pars$classif.glmboost.family$values[[1]],
#  classif.glmboost.mstop=1,
  classif.glmboost.nu=0.1,
  classif.glmboost.risk="inbag",
  classif.glmboost.stopintern=FALSE,
  classif.glmboost.m="cv")

vsx
t = train(vsx, pid.task)

getHyperPars(t$learner.model$learner)


isFeasible(getParamSet(vse)$pars$classif.glmboost.family, getParamSet(vse)$pars$classif.glmboost.family$values[[1]])

getParamSet(vse)$pars$classif.glmboost.family$values

filterX = function(x, ps) {
  r = lapply(names(x), function(n) if (is.factor(x[[n]])) ps$pars[[n]]$values[[as.character(x[[n]])]] else x[[n]])
  names(r) = names(x)
  r
}

tbl = generateRandomDesign(500, vse$searchspace, trafo=TRUE)

removeMissingValues(tbl[1, ])
removeMissingValues(tbl[2, ])

vse$searchspace$pars$automlr.remove.numerics

tbl[c(12, 13), ]
as.list(environment(vse$searchspace$pars$classif.glmboost.mstop$trafo))

round(25 * sqrt(26/25) ^ seq(from=0, to=floor(log(400/25, base=sqrt(26/25)))))

res = lapply(seq(nrow(tbl)), function(x) {
  vsx = setHyperPars(vse, par.vals=filterX(removeMissingValues(as.list(tbl[x, ])), vse$searchspace)); print(x) ; print(getHyperPars(vsx)) ; holdout(vsx, pid.task)  })

removeMissingValues(tbl[1, ])
vsx = setHyperPars(vse, par.vals=filterX(removeMissingValues(as.list(tbl[1, ])), vse$searchspace))

debugonce(automlr:::preProcess)

ho = makeResampleDesc("Holdout")
resample(vsx, pid.task, ho)

holdout(vsx, pid.task)
pv = filterX(removeMissingValues(as.list(tbl[1, ])), vse$searchspace)
vse$fixedParams

train(vsx, pid.task)

cbind(tbl, BBmisc::extractSubList(res, "aggr"))[order(BBmisc::extractSubList(res, "aggr"))[1:10], ]
stars = order(BBmisc::extractSubList(res, "aggr"))[1:10]
res[[9]]$aggr
res[[47]]$aggr
plot(sort(BBmisc::extractSubList(res, "aggr"))[1:10])
pv = filterX(removeMissingValues(as.list(tbl[4, ])), vse$searchspace)

holdout(setHyperPars(vse, par.vals=pv), pid.task)

res[[174]]
res[[386]]


xx = removeMissingValues(as.list(generateRandomDesign(1, vse$searchspace, trafo=TRUE)[1, ]))
xx
setHyperPars(vse, par.vals=filterX(xx))

getLearnerOptions(vse$learner, c("on.learner.error"))

##### small learner list II


smallAL = makeNamedAlList(
  automlr::autolearners$ampreproc,
  autolearner("classif.probit"),
  autolearner("classif.rotationForest",
              list(
                sp("K", "real", c(2, 40), trafo=function(x) max(1, round(sum(info$n.feat) / x))),
                sp("L", "int", c(25, 100)))),
  autolearner("classif.glmboost",
              list(
                sp("family", "cat", c("AdaExp", "Binomial")),
                sp("mstop", "int", c(25, 400), "exp", req=quote(m == "mstop")),
                sp("mstop.AMLRFIX1", "cat", c(400), dummy=TRUE, req=quote(m != "mstop")),
                sp("nu", "real", c(.001, .3), "exp"),
                sp("risk", "cat", c("inbag", "oobag", "none")),
                sp("stopintern", "bool"),
                sp("m", "cat", c("mstop", "cv")),
                sp("center", "def", FALSE),
                sp("trace", "def", FALSE))))

devtools::load_all("..")  # this veritable package itself
vse2 = buildLearners(automlr::autolearners, pid.task)
vse = buildLearners(automlr::autolearners, pid.task)
tbl = sampleValues(vse$searchspace, 1000, trafo=TRUE)
vse$learner$base.learners$classif.rotationForest$config$on.learner.error="warn"
configureMlr(on.learner.error="warn")

# ho = makeResampleDesc("CV", iters=5)
ho = makeResampleDesc("Holdout")

res3 = lapply(seq(length(tbl)), function(x) {
  vsx = setHyperPars(vse, par.vals=removeMissingValues(as.list(tbl[[x]])))
  print(x)
  print(getHyperPars(vsx))
  resample(vsx, pid.task, ho)
})

res3

vsx = setHyperPars(vse, par.vals=removeMissingValues(as.list(tbl[[1]])))
getHyperPars(vsx)
resample(vsx, pid.task, ho)

debugonce(automlr::preProcess)

vsx
holdout(vsx, pid.task)
getHyperPars(vsx)

head(tbl)
getHyperPars(vsx)
debugonce(automlr:::trainLearner.AMExoWrapper)

debugonce(mlr:::trainLearner.ModelMultiplexer)

names(res3[[1]])
res3[[1]]

times <- extractSubList(res3, "runtime")
learners <- as.factor(extractSubList(tbl,"selected.learner"))
names(tbl[[1]])
slev <- names(sort(sapply(levels(learners), function(x) max(times[learners==x], na.rm=TRUE))))
learnersSorted = factor(extractSubList(tbl, "selected.learner"), slev)
plot(times~learnersSorted)
plot(sort(sapply(levels(learners), function(x) min(extractSubList(res3, "aggr")[learners==x], na.rm=TRUE))))

runlengthorder <- order(extractSubList(res3, "runtime"))
extractSubList(tbl[runlengthorder],"selected.learner")
names(tbl[[1]])

plotDist <- function(tbl, res3, xitem, aggregate, aggfn=min) {
  #  times <- extractSubList(res3, xitem)
  if (is.character(unlist(extractSubList(tbl, aggregate)))) {
    learners <- as.factor(extractSubList(tbl, aggregate))
    items <- sort(sapply(levels(learners), function(x) aggfn(extractSubList(res3, xitem)[learners==x], na.rm=TRUE)))
  #  plot(sort(sapply(levels(learners), function(x) aggfn(extractSubList(res3, xitem)[learners==x], na.rm=TRUE))))
    #plot(items ~ names(items))
    plot(items ~ factor(names(items), names(items)), las=2)
  } else {
    xval <- aggfn(extractSubList(tbl, aggregate))
    yval <- extractSubList(res3, xitem)
    plot(yval ~ xval)
  }
}
# selected.learner
# ppa.nzv.cutoff.numeric, ppa.nzv.cutoff.factor
# ppa.univariate.trafo ppa.multivariate.trafo
# ppa.impute.numeric, ppa.impute.factor
# ppa.feature.filter, ppa.feature.filter.thresh

plotDist(tbl, res3, "aggr", "selected.learner", function(x, ...) mean(is.na(x), ...))
abline(c(1, 0))

sum(extractSubList(res3, "runtime"))
which(extractSubList(res3, "runtime") > 10)

res3[[36]]
removeMissingValues(tbl[[36]])

vsx = setHyperPars(vse, par.vals=removeMissingValues(as.list(tbl[[36]])))
resample(vsx, pid.task, ho)


##### Failed Learners

# learners that sometimes fail:
#  - rotationForest, if n(covariates) == 1
#  - classif.dcSVM: need to make "kmeans" method to mean kmeans(x, y, algorithm=c("MacQueen"))!
#  - classif.ranger, if n(covariates) < mtry
fails <- which(is.na(extractSubList(res3, "aggr")))
failedResult <- res3[fails]
failedTbl <- tbl[fails]

plotDist(failedTbl, failedResult, "runtime", "selected.learner", mean)

configureMlr(on.learner.error="stop")

i = 6
vsx = setHyperPars(vse, par.vals=removeMissingValues(failedTbl[[i]]))
removeMissingValues(failedTbl[[i]])
failedResult[[i]]

resample(vsx, pid.task, ho)


buildLearners(list(automlr::autolearners$classif.dbnDNN), pid.task)

a <- matrix(1:100, 20)

a

dcSVM, gaterSVM: both have problems with .model$factor.levels$Class?

##### fast learning
devtools::load_all("..")  # this veritable package itself

slowLearners <- c('classif.xyf', 'classif.lda', 'classif.mda', 'classif.rrlda', 'classif.dcSVM',
                  'classif.rda', 'classif.bartMachine', 'classif.boosting', 'classif.nodeHarvest')

sum(extractSubList(res3[extractSubList(tbl, "selected.learner") %nin% slowLearners], "runtime"))
sum(extractSubList(res3[extractSubList(tbl, "selected.learner") %in% slowLearners], "runtime"))

fastvse <- buildLearners(automlr::autolearners[names(automlr::autolearners) %nin% slowLearners], pid.task)
fasttbl = sampleValues(fastvse$searchspace, 1000, trafo=TRUE)
configureMlr(on.learner.error="warn")

# ho = makeResampleDesc("CV", iters=5)
# ho = makeResampleDesc("Holdout")

res4 = lapply(seq(length(tbl)), function(x) {
  vsx = setHyperPars(fastvse, par.vals=removeMissingValues(as.list(fasttbl[[x]])))
  print(x)
  print(getHyperPars(vsx))
  resample(vsx, pid.task, ho)
})



  

##### empty task & ModelMultiplexer
emptyTask = makeClassifTask("emptyTask", data.frame(x=factor(c("a", "b"))), target='x')
makeModelMultiplexer
ml = makeModelMultiplexer(list(makeLearner("classif.probit")))
ml = removeHyperPars(ml, "selected.learner")  # for example: one way to make trainLearner.ModelMultiplexer fail
tml = train(ml, pid.task)
isFailureModel(tml)


class(tml)

configureMlr(on.learner.error="stop")

tml = train(ml, emptyTask)
predict(tml, emptyTask)



class(tml)
class(tml)
names(tml)
class(tml$learner.model)
debugonce(mlr:::trainLearner.ModelMultiplexer)

class(ml)
tml$learner.model
mlr::predictLearner
getLearnerModel(tml)

filterFeatures(iris.task, method="rf.importance", abs=3)

debugonce(randomForestSRC::rfsrc)

##### plotting results
cbind(tbl, BBmisc::extractSubList(res, "aggr"))[order(BBmisc::extractSubList(res, "aggr"))[1:10], ]
stars = order(BBmisc::extractSubList(res, "aggr"))[1:10]
plot(sort(BBmisc::extractSubList(res, "aggr"))[1:10])

res2 = lapply(stars, function(x) {
  vsx = setHyperPars(vse, par.vals=filterX(removeMissingValues(as.list(tbl[x, ])), vse$searchspace))
  print(x)
  print(getHyperPars(vsx))
  ho = makeResampleDesc("Holdout")
  resample(vsx, pid.task, ho)
})

cbind(tbl, BBmisc::extractSubList(res2, "aggr"))[order(BBmisc::extractSubList(res2, "aggr"))[1:10], ]
plot(BBmisc::extractSubList(res2, "aggr"), sort(BBmisc::extractSubList(res, "aggr"))[1:10])
plot(BBmisc::extractSubList(res, "aggr"), BBmisc::extractSubList(res3, "aggr"))
plot(sort(BBmisc::extractSubList(res3, "aggr")))

dtc <- cbind(tbl, y=BBmisc::extractSubList(res3, "aggr"))[order(BBmisc::extractSubList(res3, "aggr")), ]

plot(dtc$y, pch=as.numeric(dtc$ppa.feature.filter)+1)


##### Testing
library('testthat')
devtools::test(pkg="../../mlr", filter="classif_gaterSVM")
devtools::test(pkg="../../mlr", filter="classif_dcSVM")


  base.learners = list(
    makeLearner("regr.glmnet"),
    makeLearner("regr.rpart")
  )
learner = makeModelMultiplexer(base.learners)

task = subsetTask(bh.task, features = character(0))

m = train(learner, task)
m$learner.model
predict(m, task)

m2 = train(makeLearner("regr.glmnet"), task)
p = predict(m2, task)
class(p$data)

gb = makeLearner("regr.glmboost", stopintern=FALSE)
x = train(gb, pid.task)
predict(x, pid.task)
getParamSet(gb)


gb = makeLearner("surv.glmboost")
x = train(gb, .task)
predict(x, pid.task)
getParamSet(gb)


##### wrapper debugging

lrn = makeLearner("classif.probit", config=list(on.learner.error="warn"))
lrn2 = makeLearner("classif.probit")
trainfun = function(data, target, args) stop("Hammer Time")
predictfun = function(data, target, args, control) NULL
badPreprocLrn = makePreprocWrapper(lrn2, train = trainfun, predict = predictfun)
x = train(badPreprocLrn, pid.task)
str(x)

##### classif.gaterSVM, classif.dcSVM debug
devtools::load_all("../../mlr")

holdout(makeLearner("classif.gaterSVM"), pid.task)

holdout(makeLearner("classif.dcSVM"), pid.task)

traintask = makeClassifTask('train', data.frame(a=c(1, 2), b=c("a", "b")), 'b')
testtask = makeClusterTask('test', data.frame(a=c(1, 1)))

tt2 = makeClassifTask('train', data.frame(a=c(rnorm(100), (1+rnorm(100))), b=rep(c("a", "b"), each=100), c=rnorm(200)), 'b')

x = train(makeLearner("classif.dcSVM", m=100,k=10,max.levels=1,early=0, seed=0), tt2)

x = train(makeLearner("classif.dcSVM", m=100,k=10,max.levels=1,early=1, seed=0), traintask)

is.factor(predict(x$learner.model, newdata=data.frame(a=c(1, 1), c=c(0, 0))))


predict(x, testtask)

traintask = makeClassifTask('train', data.frame(a=c(1, 2, 1, 2), b=c(1, 1, 2, 2), c=c("a", "b", "a", "b")), 'c')
testtask = makeClusterTask('test', data.frame(a=c(1, 1), b=c(1, 1)))
x = train(makeLearner("classif.gaterSVM", m=2), traintask)
predict(x, testtask)

predict(x$learner.model, newdata=data.frame(a=c(1, 1), b=c(1, 1)))

debugonce(predictLearner.classif.gaterSVM)


  data = data.frame(a=c(1, 2, 1, 2), b=c(1, 1, 2, 2), c=c("a", "b", "a", "b"))
  traintask = makeClassifTask("train", data, "c")
  testtask = makeClusterTask("test", data.frame(a=c(1, 1), b=c(1, 1)))
  x = train(makeLearner("classif.dcSVM"), traintask)
predict(x, testtask)$data$response

  data = data.frame(a = c(1, 2, 1, 2), b = c(1, 1, 2, 2), c = c("a", "b", "a", "b"))
  traintask = makeClassifTask("train", data, "c")
  testtask = makeClusterTask("test", data.frame(a = c(1, 1), b = c(1, 1)))
  x = train(makeLearner("classif.gaterSVM", seed = 0), traintask)
  predict(x, testtask)
