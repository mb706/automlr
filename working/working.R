
devtools::load_all("../../ParamHelpers")
devtools::load_all("../../mlr")
devtools::load_all("..")  # this veritable package itself


ret <- preProcess(getTaskData(iris.task), univariate.trafo="centerscale", nzv.cutoff.numeric=0.5, multivariate.trafo="ica")
all.equal(predict(ret, getTaskData(iris.task)), ret$debugdata)

ret$scale
ret$center
dat <- getTaskData(iris.task)
dat[ret$cols.numeric] <- as.matrix(getTaskData(iris.task)[ret$cols.numeric]) %*% ret$rotation
ret$rotation
dat
