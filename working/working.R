options(width=150)
devtools::load_all("../../ParamHelpers")
devtools::load_all("../../mlr")

library(roxygen2)
roxygenise('..')

devtools::load_all("..")  # this veritable package itself

options(error=dump.frames)

ret <- preProcess(getTaskData(iris.task), univariate.trafo="centerscale", nzv.cutoff.numeric=0.5, multivariate.trafo="ica")
all.equal(predict(ret, getTaskData(iris.task)), ret$debugdata)

ret$scale
ret$center
dat <- getTaskData(iris.task)
dat[ret$cols.numeric] <- as.matrix(getTaskData(iris.task)[ret$cols.numeric]) %*% ret$rotation
ret$rotation
dat


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

printAllGiven = function(s) {
for (line in seq_len(nrow(s))) {
  print(removeMissingValues(s[line, ]))
}
}

s
l$searchspace
l$searchspace$pars$classif.sparseLDA.lambda$trafo(0.1)

environment(l$searchspace$pars$classif.sparseLDA.lambda$trafo)$min

t1 = automlr:::createTrafo(1e-10, 1, FALSE)$trafo
t2 = automlr:::createTrafo(0, 1, FALSE)$trafo
  environment(t1)$min

removeMissingValues(s[1, ])

devtools::load_all("..")  # this veritable package itself
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

lsmall = buildLearners(list(automlr::autolearners$classif.glmboost), iris.task)

generateRandomDesign(10, lsmall$searchspace)

ps = BBmisc::extractSubList(lbig$base.learners, "properties")
Filter(function(x) ("ordered" %in% x), ps)
length(allfacs)
Filter(function(x) "ordered" %in% x, allfacs)


listWrapperCombinations = function(ids, required) {
  requiredIDs = ids[required]
  unlist(sapply(seq_along(ids), function(l) apply(expand.grid(rep(list(ids), l)), 1, function(x)
        if (all(requiredIDs %in% x) && all(!duplicated(x))) paste(x, collapse="$"))))
}

listWrapperCombinations(c("a", "b", "c"), c(F, T, T))
duplicated(ids)
ids = c('a', 'b', 'c')
rep(list(ids), 1)

generateRandomDesign(10, lsmall$searchspace)


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

devtools::load_all("..")  # this veritable package itself
res = automlr:::makeAMExoWrapper(NULL, wrappers, taskdesc, idRef, c("missings", "factors", "ordered", "numerics"), canHandleX)
res

res$wrappers$a$searchspace$pars$a1$requires

debugger()


