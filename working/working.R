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

s = ParamHelpers::generateRandomDesign(10, lbig$searchspace, TRUE)

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

mod = makeLearner("classif.glmboost")
mmod = makeModelMultiplexer(list(mod))
ps = makeParamSet(makeDiscreteParam("m", values=c("cv",  "aic", "mstop")), makeIntegerParam("mstop", 25, 500, 25, requires=quote(m == "mstop")))
generateRandomDesign(10, psx)

debugonce(automlr:::makeModelMultiplexerParamSetEx)
psx = automlr:::makeModelMultiplexerParamSetEx(mmod, list(classif.glmboost=ps))
psx$pars$classif.glmboost.mstop$requires
