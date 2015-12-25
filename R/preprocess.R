
preProcess = function(data, nzv.cutoff.numeric=0, nzv.cutoff.factor, univariate.trafo="off",
    impute.numeric="remove.na", impute.factor="remove.na", multivariate.trafo="off", feature.filter="off",
    feature.filter.thresh = 0) {
  # first: check arguments
  assertClass(task, classes = "Task")
  assertNumber(nzv.cutoff.numeric, lower=0)
  assertPercentage(nzv.cutoff.factor)
  assertChoice(univariate.trafo, c("off", "center", "scale", "centerscale", "range"))
  assertChoice(impute.numeric, c("remove.na", "mean", "median", "hist"))
  assertChoice(impute.factor, c("remove.na", "distinct", "mode", "hist"))
  assertChoice(multivariate.trafo, c("off", "pca", "ica"))
  assertChoice(feature.filter, c("none", "info.gain", "chi.squared", "rf.importance"))
  if (feature.filter != "off") {
    assertPercentage(feature.filter.thresh)
  }
  
  ppobject = addClasses(as.list(environment()), "ampreproc")  # collect all arguments given
  
  # determine the numeric & factor cols
  cols.numeric = sapply(data, is.numeric)
  cols.numeric = names(data)[cols.numeric]
  cols.factor = sapply(data, class) == "factor"
  cols.factor = names(data)[cols.factor]
  
  naToZero = function(x) ifelse(is.na(x), 0, x)
  
  # drop cols that have low variance
  nzv.drop.numeric = naToZero(sapply(data[cols.numeric], var, na.rm=TRUE)) <= nzv.cutoff.numeric  # TODO: is this kosher with NAs?
  ndsplit = split(cols.numeric, nzv.drop.numeric)
  nzv.drop.numeric = ndsplit$`TRUE`
  cols.numeric = ndsplit$`FALSE`
  
  highestFactorFrequency = function(x) sort(table(x, useNA="ifany"), TRUE)[1] / length(x)
  nzv.drop.factor = sapply(data[cols.factor], highestFactorFrequency) >= 1 - nzv.cutoff.factor
  ndsplit = split(cols.factor, nzv.drop.factor)
  nzv.drop.factor = ndsplit$`TRUE`
  cols.factor = ndsplit$`FALSE`
  
  data = data[names(data) %nin% c(nzv.drop.factor, nzv.drop.numeric)]

  # univariate transformation (only on numerics)
  if (univariate.trafo != "off") {
    ndata = as.matrix(data[cols.numeric])
    if (univariate.trafo %in% c("center", "scale", "centerscale")) {
      ndata = scale(ndata, center=center, scale=scale)

      center = univariate.trafo %in% c("center", "centerscale")
      scale = univariate.trafo %in% c("scale", "centerscale")

      ppobject$center = attr(ndata, "scaled:center")
      ppobject$scale = attr(ndata, "scaled:scale")


    } else {  # univariate.trafo == "range"
      rng = apply(ndata, 2, range, na.rm=TRUE)

      ppobject$scale = apply(rng, 2, diff)
      ppobject$center = apply(rng, 2, mean) - ppobject$scale / 2

      ndata = scale(ndata, center=ppobject$center, scale=ppobject$scale)
    }
    data[cols.numeric] = ndata
  }
  
  
  removeNa = function(data, cols) {
    delendum = Reduce(`&`, lapply(data[cols.numeric], is.na))
    data[!delendum, ]
  }
  
  imputeFun = function(x, fun, ...) {
      x[is.na(x)] = fun(x[!is.na(x)], size=sum(is.na(x)), replace=TRUE)
      x
  }

  switch(impute.numeric,
      remove.na={
        data = removeNa(data, cols.numeric)
      },
      mean={
        data[cols.numeric] = lapply(data[cols.numeric], imputeFun, fun=mean)
      },
      median={
        data[cols.numeric] = lapply(data[cols.numeric], imputeFun, fun=median)
      },
      hist={
        data[cols.numeric] = lapply(data[cols.numeric], imputeFun, fun=sample)
      })

  switch(impute.factor,
      remove.na={
        data = removeNa(data, cols.factor)
      },
      distinct={
        data[cols.factor] = lapply(data[cols.factor], addNA, ifany=TRUE)
      },
      mode={
        data[cols.factor] = lapply(data[cols.factor], imputeFun, fun=function(x, ...) names(sort(table(x), TRUE))[1])
      },
      hist={
        data[cols.factor] = lapply(data[cols.factor], imputeFun, fun=sample)
      })

  if (multivariate.trafo == "pca") {
    pcr = prcomp(data[cols.numeric])
    data[cols.numeric] = pcr$rotation
  } else if (multivariate.trafo == "ica") {
    requirePackages("fastICA", why="preProcess", default.method="load")
    data[cols.numeric] = fastICA::fastICA(data[cols.numeric], length(cols.numeric),
        method="C")$S
  }
  
}



predict.ampreproc = function(object, newdata, ...) {
  
}