
preProcess = function(task, nzv.cutoff.numeric=0, nzv.cutoff.factor=0, univariate.trafo="off",
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
  assertChoice(feature.filter, c("off", "info.gain", "chi.squared", "rf.importance"))
  if (feature.filter != "off") {
    assertPercentage(feature.filter.thresh)
  }
  
  ppobject = addClasses(as.list(environment()), "ampreproc")  # collect all arguments given. TODO: don't save task
  
  data = getTaskData(task)
  weights = coalesce(task$weights, rep(1, nrow(data)))
  # determine the numeric & factor cols
  cols.numeric = sapply(data, is.numeric)
  cols.numeric = names(data)[cols.numeric]
  cols.factor = sapply(data, class) == "factor"
  cols.factor = names(data)[cols.factor]
  
  naToZero = function(x) ifelse(is.na(x), 0, x)
  
  # drop cols that have low variance
  nzv.drop.numeric = naToZero(sapply(data[cols.numeric], weighted.var, w=weights, na.rm=TRUE)) <= nzv.cutoff.numeric  # TODO: is this kosher with NAs?
  ndsplit = split(cols.numeric, nzv.drop.numeric)
  nzv.drop.numeric = ndsplit$`TRUE`
  cols.numeric = ndsplit$`FALSE`
  
  highestFactorFrequency = function(x, w) sort(weighted.table(x, w, useNA="ifany"), TRUE)[1] / length(x)
  nzv.drop.factor = sapply(data[cols.factor], highestFactorFrequency, w=weights) >= 1 - nzv.cutoff.factor
  ndsplit = split(cols.factor, nzv.drop.factor)
  nzv.drop.factor = ndsplit$`TRUE`
  cols.factor = ndsplit$`FALSE`
  
  ppobject$dropcols = c(nzv.drop.factor, nzv.drop.numeric)
  ppobject$cols.factor = cols.factor
  ppobject$cols.numeric = cols.numeric
  
  data = data[names(data) %nin% ppobject$dropcols]
  
  # univariate transformation (only on numerics)
  if (length(cols.numeric) > 0 && univariate.trafo != "off") {
    ndata = as.matrix(data[cols.numeric])
    if (univariate.trafo %in% c("center", "scale", "centerscale")) {
      
      center = univariate.trafo %in% c("center", "centerscale")
      scale = univariate.trafo %in% c("scale", "centerscale")
      
      if (center) {
        center = sapply(ndata, weighted.mean, w=weights, na.rm=TRUE)
      }
      
      if (scale) {
        weighted.rms = function(x, w, c) sqrt(sum(w * (x - c)^2, na.rm=TRUE)/(sum(w[!is.na(x)])-1))
        scale = mapply(ndata, weighted.rms, w=weights, c=center)
      }
      
      ndata = scale(ndata, center=center, scale=scale)
      
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
  
  if (length(cols.numeric) > 0) {
    if (impute.numeric == "remove.na") {
      delendum = naRows(data, cols.numeric)
      data = data[!delendum, ]
      target = target[!delendum]
    } else {
      ppobject$pop.numeric = lapply(data[cols.numeric], function(x) {
            switch(impute.numeric,
                            mean=mean(x),
                            median=median(x),
                            hist=x[!is.na(x)])
          })  # TODO: maybe do rounding for mean / median and integers
      data[cols.numeric] = mapply(imputeRandom, data[cols.numeric], ppobject$pop.numeric, SIMPLIFY=FALSE)
    }
  }
  
  if (length(cols.factor) > 0) {
    if (impute.factor == "remove.na") {
      delendum = naRows(data, cols.factor)
      data = data[!delendum, ]
      target = target[!delendum]
    } else if (impute.factor == "distinct") {
      data[cols.factor] = lapply(data[cols.factor], addNA, ifany=FALSE)
    } else {
      ppobject$pop.factor = lapply(data[cols.factor], function(x) {
            switch(impute.factor, 
                            mode={
                                ftab = sort(table(x), TRUE)
                                names(ftab)[ftab==ftab[1]]  # if there is a tie, get all tieing levels
                            },
                            hist=x[!is.na(x)])
          })
      data[cols.factor] = mapply(imputeRandom, data[cols.factor], ppobject$pop.factor, SIMPLIFY=FALSE)
    }
  }
  
  if (length(cols.numeric) > 0) {
    if (multivariate.trafo == "pca") {
      pcr = prcomp(data[cols.numeric], center=FALSE)
      data[cols.numeric] = pcr$x
      ppobject$rotation = pcr$rotation
    } else if (multivariate.trafo == "ica") {
      requirePackages("fastICA", why="preProcess", default.method="load")
      ica = fastICA::fastICA(data[cols.numeric], length(cols.numeric), method="C")
      data[cols.numeric] = ica$S
      ppobject$rotation = ica$K %*% ica$W
    }
  }
  
  ppobject$debugdata = data
  
  ppobject
}

predict.ampreproc = function(object, newdata, ...) {
  # all right lets go
  # drop low variance cols
  newdata = newdata[names(newdata) %nin% object$dropcols]
  
  # univariate trafo
  if (length(object$cols.numeric) > 0) {
    if (object$univariate.trafo != "off") {
      ndata = as.matrix(newdata[object$cols.numeric])
      ndata = scale(ndata, center=coalesce(object$center, FALSE), scale=coalesce(object$scale, FALSE))
      newdata[object$cols.numeric] = ndata
    }
    
    if (object$impute.numeric == "remove.na") {
      newdata = removeNa(newdata, object$cols.numeric)
    } else {
      newdata[object$cols.numeric] = mapply(imputeRandom, newdata[object$cols.numeric], object$pop.numeric, SIMPLIFY=FALSE)
    }
    
    if (object$multivariate.trafo != "off") {
      newdata[object$cols.numeric] = as.matrix(newdata[object$cols.numeric]) %*% object$rotation
    }
  }
  
  if (length(object$cols.factor) > 0) {
    if (object$impute.factor == "remove.na") {
      newdata = removeNa(newdata, object$cols.factor)
    } else if (impute.factor == "distinct") {
      newdata[object$cols.factor] = lapply(newdata[object$cols.factor], addNA, ifany=FALSE)
    } else {
      newdata[object$cols.factor] = mapply(imputeRandom, newdata[object$cols.factor], object$pop.factor, SIMPLIFY=FALSE)
    }
  }
  
  newdata
}

weighted.var = function(x, w, na.rm=FALSE) {
  xbar = weighted.mean(x, w, na.rm=na.rm)
  sum(w * (x - xbar)^2, na.rm=na.rm) / (sum(w[!is.na(x)]) - 1)
}

weighted.table = function(x, w, useNA="no", exclude = if(useNA == "no") c(NA, NaN)) {
  if (useNA == "always") {
    # add a weight-zero element if we always want an <NA> entry
    x = c(x, NA)
    w = c(w, 0)
  }
  xtabs(w~x, na.action=ifelse(useNA == "no", na.exclude, na.pass), exclude=exclude)
}

naRows = function(data, cols) {
  Reduce(`&`, lapply(data[cols], is.na))
}

imputeRandom = function(x, pop) {
  x[is.na(x)] = sample(pop, size=sum(is.na(x)), replace=TRUE)
  x
}