##### Install
install.packages("devtools")
update.packages()
install.packages(c("mlr", "ParamHelpers"), dependencies=c("Depends", "Imports", "LinkingTo", "Suggests"))

##### init



options(width=150)
devtools::load_all("../../ParamHelpers")
devtools::load_all("../../mlr")
library(roxygen2)
roxygenise('..')
devtools::load_all("..")
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

devtools::load_all("..")
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

devtools::load_all("..")
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

devtools::load_all("..")
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

devtools::load_all("..")
vse2 = buildLearners(automlr::autolearners, pid.task)

vse = buildLearners(automlr::autolearners, pid.task)
tbl = sampleValues(vse$searchspace, 1000, trafo=TRUE)
vse$learner$base.learners$classif.rotationForest$config$on.learner.error="warn"
configureMlr(on.learner.error="warn")

# ho = makeResampleDesc("CV", iters=5)
ho = makeResampleDesc("Holdout")

#res3 = lapply(seq(length(tbl)), function(x) {
#  vsx = setHyperPars(vse, par.vals=removeMissingValues(as.list(tbl[[x]])))
#  print(x)
#  print(getHyperPars(vsx))
#  resample(vsx, pid.task, ho)
#})

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

nas = sapply(res3, function(x) is.na(x$aggr))

plotDist(tbl[!nas], res3[!nas], "runtime", "selected.learner", mean)
abline(c(.5, 0))

sum(extractSubList(res3, "runtime"))
which(extractSubList(res3, "runtime") > 10)

res3[[36]]
removeMissingValues(tbl[[36]])

vsx = setHyperPars(vse, par.vals=removeMissingValues(as.list(tbl[[36]])))
resample(vsx, pid.task, ho)


##### Failed Learners
options(error=dump.frames)
# learners that sometimes fail:
#  - Things that break when ncol = 1
#    - rotationForest, if n(covariates) == 1.
#    - classif.hdrda fails when ncol == 1
#    - classif.qda: fails when ncol == 1
#    - classif.glmnet: fails if n(covariates) == 1
#    - classif.quaDA: doesn't work with n(covariates) == 1
#  - Things that break when n(covariates) determines the range of a param
#    - classif.ranger, if n(covariates) < mtry
#    - rotationForest:  when round(ncol(x) / K) == 1
#    - classif.plsdaCaret breaks if ncomp is > min(ncol, nrow-1)
#    - classif.rknn: fails if mtry > ncol(data)
#  - Things that break when number of data too small:
#    - classif.xyf fails when gridsize is larger than number of data point (listed again here)
#    - classif.plsdaCaret breaks if ncomp is > min(ncol, nrow-1)
#  - Things that seem generally unstable
#    - classif.dcSVM: kernkmeans is unstable and fails randomly.
#    - classif.dcSVM: length(ind) < min.cluster: need to set min.cluster to max(min.cluster, k)!! In SwarmSVM::dcSVM
#    - classif.lssvm sometimes has singular matrix
#    - classif.randomForestSRC crashes sometimes, not others?
#    - classif.gaterSVM fails just like that, depending on "groups with zero length" or parameter c
#    - classif.rknn: seems to be a bit unstable generally
#    - classif.mda failed once in "df.inv" function (tbl[[325]])
#    - classif.gaterSVM has its times, see below

#    - classif.dcSVM: need to make "kmeans" method to mean kmeans(x, y, algorithm=c("MacQueen"))!
#  [x] classif.nnTrain: 'numlayers' breaks it. Need new type 'HIDDEN'.
#  [x] classif.extraTrees: subsetSizeIsNull must be HIDDEN
#  [x] classif.saeDNN: numlayers must be HIDDEN

#  [x] classif.glmboost: m == "aic" --> classif.glmboost.risk = "inbag"
#  [x] classif.PART: errors thrown if: -R && C, C not between 0 and 1, N && NOT-R
#  [x] classif.J48: errors thrown if: -U && -S, -U && -R, -C && -U, -C && -R; C not between 0 and 1. -N && NOT-R
#  [x] classif.geoDA:  validation == "learntest" is broken, but from geoDAs side
#  [x] classif.quaDA: "learntest" also broken...
#  [x] classif.neuralnet: linear.output does not work.
#  [x] classif.ksvm: kernel == "stringdot" not actually supported; instead use "matrix".
#  [x] classif.rda: fails when crossval==TRUE, fold == 1


# gaterSVM fails with:
vsx = setHyperPars(vse, par.vals=list(selected.learner = "classif.gaterSVM", classif.gaterSVM.m = 25,      classif.gaterSVM.max.iter = 71, classif.gaterSVM.hidden = 3,      classif.gaterSVM.learningrate = 0.423925815119678, classif.gaterSVM.threshold = 0.000000497414795165264,      classif.gaterSVM.stepmax = 74, classif.gaterSVM.c = 100,      ppa.nzv.cutoff.numeric = 0.159084632935392, ppa.univariate.trafo = "centerscale",      ppa.multivariate.trafo = "ica", ppa.feature.filter = "off"))

# interesting fails:
# 325
# 347 -- binomial -- i guess completely random
Q
i
length(f2)
     
fails <- which(is.na(extractSubList(res3, "aggr")))
f2 <- fails[intersect(55:length(fails), which(extractSubList(tbl[fails], "selected.learner") %nin% c("classif.neuralnet", "classif.geoDA", "classif.saeDNN", "classif.extraTrees", "classif.nnTrain" , "classif.gbm", "classif.bdk", "classif.dbnDNN", "classif.ada", "classif.ctree", "classif.blackboost")))]
f2
fails

failedResult <- res3[f2]
failedTbl <- tbl[f2]

plotDist(failedTbl, failedResult, "runtime", "selected.learner", mean)

configureMlr(on.learner.error="stop")
i-6
f2[i]
f2[49]
removeMissingValues(failedTbl[[i-1]])
f2
i = which(f2 == 325)
i

(i = i + 1)

vsx = setHyperPars(vse, par.vals=removeMissingValues(failedTbl[[i]]))
removeMissingValues(failedTbl[[i]])
# failedResult[[i]]
resample(vsx, pid.task, ho)
print(failedTbl[[i]]$selected.learner)

removeMissingValues(failedTbl[[i]])

debugonce(mlr:::trainLearner.classif.saeDNN)

getHyperPars(vsx)$
print()
cat(deparse(removeMissingValues(failedTbl[[i]]), control=list()))

buildLearners(list(automlr::autolearners$classif.dbnDNN), pid.task)

a <- matrix(1:100, 20)

a

dcSVM, gaterSVM: both have problems with .model$factor.levels$Class?

##### fast learning
devtools::load_all("..")

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


x = matrix(c(
  -0.0579049224306274, 1.00516616975178,  0.418984059437141, 0.804358348244999, 1.22299378460262, -0.0681603440346371,  0.676985389705721,
  1.56008796296189, 1.02436587954592, 1.81105582121434,  0.627961070792271, 0.977704132291529, 0.719160397917624, 0.975428687276605,
  1.17156255214052, 0.473509846619958, 1.25558128690508, 1.88003466789258,  -0.0214571970730996, -0.231525725027871, -0.506551479512185,
  0.182355440425921, -0.432214282809464, -0.438108498310288, -0.310841316754162,  -0.0653533854965769, -0.624947955596895, -0.484539358723184,
  -0.182586118381738, -0.722164175184012, 0.0800340678458996, -0.0321743672609238,  -0.405937312016915, -0.343843459458522, -0.137867070894641,
  -0.566686259554594,  -0.11371837606192, -0.558140234390195, 2.25504301037202, 1.88742772396292,  1.67044559546668, 1.58943745553616,
  1.23785589841201, 2.14827236914216,  1.94488972734457, 0.99335453521417, 1.19882226022849, 1.12006172306959,  1.95014378343019,
  1.53653888509545, 1.52268138869386, 1.60751591235831,  1.25876181616502, 1.69361849057563, 1.47858817475208, 1.05185104851319,
  2.37091109724582, 0.0237604601797854, -0.053886299562887, 0.0604365901259318,  -0.826475021104859, -0.194966572933609, -0.814129148853346,
  0.261093918305815,  -0.181622846849615, -0.779321290085687, 0.819110569455364, -0.515414062712161,  1.07487047641155, 0.123152057908705,
  0.286392630864587, -0.0423487167533624,  0.302752112505672, -0.184781920271067, 1.08447165496384, -0.538007211629324,  0.500543762952515,
  1.73853372325441, -0.791089702388143, 1.26374142602086,  -1.74607587081893, -0.509424814917579, 0.277030808683005, 0.124972622597836,
  -1.78148854895652, -0.909603187072895, -0.875679611333982, -0.935051899674395,  0.567851804064621, 0.135950465089575, 1.28462503580773,
  0.472479005919984,  0.663306948219522, -0.0284579592907923, -0.295667842684, 0.30613172133055,  0.295315344162428, 0.390008907128904,
  0.291186403449642, 0.380653920729877,  0.359954425046099, 0.375325308823369, 0.277160822633114, 0.372317709549438,  0.391980575110528,
  0.354600602019738, 0.379496754784887, 0.311928115455958,  0.310550301877922, 0.258102041747073, 0.285720733675813, 0.303401096577156,
  0.350386696724582, 0.340694450780792, -0.054311582247365, -0.158151471049343,  -0.808683313203137, -0.0542069129023475, -0.190481602144457,
  -0.815710324225835, -0.953574205193643, 1.00910291201536, -0.199061837855707,  -0.33024331445543, -0.402390318594414, -0.215355811516023,
  0.121997536554817,  0.338596359733725, 0.849870268287867, 0.589097957217166, 0.19583609400354,  0.164437143610763, -0.549557209895514), ncol=7)
kernlab::kkmeans(x = x, centers=6)



#fails because
#https://github.com/cran/kernlab/blob/master/R/kkmeans.R
#line 168
#lower[compin,u] <- dismat[compin,u] <- - 2 * affinMult(kernel,x[compin,],x[vgr[[u]],,drop=FALSE], w[vgr[[u]]], p , D, D1)/sum(w[vgr[[u]]]) + secsum[u] + kdiag[compin]
#is missing a drop=FALSE at x[compin, ]?


##### quick tests for dependencies



slowLearners <- c('classif.xyf', 'classif.glmboost', 'classif.rotationForest', 'classif.lda', 'classif.mda', 'classif.rrlda', 'classif.dcSVM',
                  'classif.rda', 'classif.bartMachine', 'classif.boosting', 'classif.nodeHarvest', 'classif.randomForest', 'classif.nnet', 'classif.svm',
                  'classif.lssvm', 'classif.clusterSVM', 'classif.ksvm', 'classif.geoDA', 'classif.nnTrain', 'classif.IBk')



devtools::load_all("..")
vse = buildLearners(c(Filter(function(x) x$learner %nin% slowLearners, automlr::autolearners), automlr:::autowrappers), pid.task)
vse = buildLearners(c(automlr::autolearners, automlr:::autowrappers), pid.task)

vse2 = buildLearners(c(list(automlr::autolearners$classif.rFerns), automlr:::autowrappers), pid.task)
tbl2 = sampleValues(vse2$searchspace, 2000, trafo=TRUE)



tbl2 = sampleValues(vse$searchspace, 2000, trafo=TRUE)

configureMlr(on.learner.error="warn")

# ho = makeResampleDesc("CV", iters=5)
ho2 = makeResampleDesc("Holdout")
x <- 1
removeMissingValues(tbl2[[1]])
sink()
sink(type="message")
x
z <- file("output2.msg", open="wt")
sink(z, type="message")
sink("output2.run")
1 + 1

res4 = lapply(seq(length(tbl2)), function(x) {
  vsx = setHyperPars(vse2, par.vals=removeMissingValues(as.list(tbl2[[x]])))
  print(x)
  print(removeMissingValues(tbl2[[x]]))
  print(resample(vsx, pid.task, ho2))
})

Filter(function(x) is.na(x$aggr), res4)

res2

repeat holdout(setHyperPars(vse, ppa.feature.filter="off", ppa.multivariate.trafo="ica", ppa.univariate.trafo="scale", ppa.nzv.cutoff.numeric=0.2396230602,
                     classif.rFerns.ferns=128, classif.rFerns.depth=16, selected.learner="classif.rFerns"), pid.task)



##### intensively testing parameter space stuff

configureMlr(show.learner.output=TRUE)

emptypredict = function(.learner, .model, .newdata, ...) {}

l1 = makeRLearnerClassif("numhandler", character(0),
  makeParamSet(makeDiscreteLearnerParam("a", values=c("x", "y", "z")),
               makeDiscreteLearnerParam("b", values=c("1", "2", "3"), requires=quote(a == "x"))),
  properties=c("twoclass", "multiclass", "numerics"))

trainLearner.numhandler = function(.learner, .task, .subset, .weights=NULL, ...) {
  pars = list(...)
  cat("numhandler", pars$a, pars$b, "\n")
  NULL
}

predictLearner.numhandler = emptypredict

l2 = makeRLearnerClassif("facthandler", character(0),
    makeParamSet(makeDiscreteLearnerParam("a", values=c("x", "y", "z")),
                 makeDiscreteLearnerParam("b", values=c("1", "2", "3"), requires=quote(a == "x"))),
    properties=c("twoclass", "multiclass", "factors", "numerics"))

predictLearner.facthandler = emptypredict
trainLearner.facthandler = function(.learner, .task, .subset, .weights=NULL, ...) {
  pars = list(...)
  cat(colnames(getTaskData(.task)), "\n")
  cat("facthandler", pars$a, pars$b, "\n")
  NULL
}

invisible(train(setHyperPars(l1, a="x"), pid.task))

removeFactorsWrapper = function (learner, ...) {
  par.set = makeParamSet(
      makeLogicalLearnerParam("remove.factors", default=FALSE)
  )
  par.vals = getDefaults(par.set)
  par.vals = insert(par.vals, list(...))
  
  trainfun = function(data, target, args) {
    if (args$remove.factors) {
      data = data[sapply(data, is.numeric) | colnames(data) == target]
    }
    list(data=data, control=list(args$remove.factors))
  }
  
  predictfun = function(data, target, args, control) {
    if (control[[1]]) {
      data = data[sapply(data, is.numeric) | colnames(data) == target]
    }
    data
  }
  
  x = makePreprocWrapper(learner, trainfun, predictfun, par.set, par.vals)
  addClasses(x, "PreprocWrapperAm")
}


invisible(train(l2, pid.task))



demodata = data.frame(
    n1 = rnorm(10),
    n2 = rnorm(10),
    f1 = factor(sample(c("a", "b"), 10, TRUE)),
    f2 = factor(sample(c("y", "z"), 10, TRUE)),
    target=factor(sample(c("1", "2"), 10, TRUE)))
dd = makeClassifTask("demo", demodata, "target")

lx = removeFactorsWrapper(l2)
invisible(train(setHyperPars(lx, remove.factors=TRUE), dd))


wspace = automlr::makeNamedAlList(
    autolearner(stacktype="requiredwrapper",
                searchspace=list(
                    sp("remove.factors", "cat", TRUE, req=quote(automlr.remove.factors == TRUE)),
                    sp("remove.factors.AMLRFIX1", "cat", FALSE, req=quote(automlr.remove.factors == FALSE))),
                learner=autoWrapper(
                    name="removefactwrapper",
                    constructor=removeFactorsWrapper,
                    conversion=function(x) switch(x, factors=c("factors", ""), x))),
    autolearner(l1,
                list(sp("a", "cat", c("x", "y", "z")),
                     sp("b", "cat", c("1", "2", "3"), req=quote(a == "x")))),
    autolearner(l2,
                list(sp("a", "cat", c("x", "y", "z")),
                     sp("b", "cat", c("1", "2", "3"), req=quote(a == "x")))))


bldemo = buildLearners(wspace, pid.task)

rd = sampleValues(bldemo$searchspace, 10, TRUE)
for (i in 1:10) {
  train(setHyperPars(bldemo, par.vals=removeMissingValues(rd[[i]])), dd)
}


# Ok, what needs to be tested?
# searchspace with regr, twoclass, multiclass learners --
# twoclass classif: - all learners that can handle twoclass are present, no others
# multiclass classif: all learners that can handle multiclass are present, no others
#
# 





