
#' @title preprocess the given data set
#' 
#' @description
#' Do a set of transformation on the dataset depending on given parameters.
#' 
#' @param data [\code{data.frame}]\cr
#'   The dataset
#' @param target [\code{character}]\cr
#'   The name(s) of the target column(s) of the dataset; may be NULL if there is
#'   no data column.
#' @param nzv.cutoff.numeric [\code{numeric}]\cr
#'   Exclude numeric columns if their variance goes below this threshold.
#' @param nzv.cutoff.factor [\code{numeric}]\cr
#'   Exclude factorial columns if the frequency of its most frequent level is
#'   above 1-\code{nzv.cutoff.factor}.
#' @param univariate.trafo [\code{character(1)}]\cr
#'   The transformation to perform on numeric columns. Must be one of
#'   \code{"off"} (no trafo), \code{"center"}, \code{"scale"},
#'   \code{"centerscale"}, \code{"range"} (scaling so that all values lie
#'   between \code{0} and \code{1}.
#' @param impute.numeric [\code{character(1)}]\cr
#'   If and how to impute numeric missing values. Must be one of \code{"off"},
#'   \code{"remove.na"} (deleting rows with NAs), \code{"mean"},
#'   \code{"median"}, \code{"hist"}.
#' @param impute.factor [\code{character(1)}]\cr
#'   If and how to impute factorial missing values. Must be one of \code{"off"},
#'   \code{"remove.na"}, \code{"distinct"} (introduce new factor level),
#'   \code{"mode"}, \code{"hist"}.
#' @param multivariate.trafo [\code{character(1)}]\cr
#'   The multivariate transformation of numeric parameters to perform. Must be
#'   one of \code{"off"}, \code{"pca"}, \code{"ica"}.
#' @param feature.filter [\code{character(1)}]\cr
#'   How to do feature filtering. Must be one of \code{"off"},
#'   \code{"information.gain"}, \code{"chi.squared"}, \code{"rf.importance"}.
#' @param feature.filter.thresh [\code{numeric}]\cr
#'   The threshold at which to exclude features when performing feature
#'   filtering. Has no effect if \code{feature.filter} is set to \code{"off"}.
#' @param keep.data [\code{logical(1)}]\cr
#'   Whether to include the transformed data in the returned object.
#' 
#' @return [\code{ampreproc}]
#' An object that can be used with \code{predict} to transform new data. If
#' \code{keep.data} is \code{TRUE}, the slot \code{$data} will contain the
#' transformed data.
preProcess = function(data, target = NULL, nzv.cutoff.numeric = 0,
    nzv.cutoff.factor = 0, univariate.trafo = "off", impute.numeric = "off",
    impute.factor = "off", multivariate.trafo = "off", feature.filter = "off",
    feature.filter.thresh = 0, keep.data = FALSE) {
  # FIXME: handle ordered factors

  # first: check arguments
  assertClass(data, classes = "data.frame")
  
  assertNumber(nzv.cutoff.numeric, lower = 0)
  assertNumber(nzv.cutoff.factor, lower = 0, upper = 1)
  assertChoice(univariate.trafo,
      c("off", "center", "scale", "centerscale", "range"))
  assertChoice(impute.numeric,
      c("off", "remove.na", "mean", "median", "hist"))
  assertChoice(impute.factor,
      c("off", "remove.na", "distinct", "mode", "hist"))
  assertChoice(multivariate.trafo,
      c("off", "pca", "ica"))
  assertChoice(feature.filter,
      c("off", "information.gain", "chi.squared", "rf.importance"))
  if (feature.filter != "off") {
    assertNumber(feature.filter.thresh)
  }
  
  # collect all arguments given
  ppobject = addClasses(as.list(environment()), "ampreproc")
  ppobject$data = NULL
  
  hasTarget = !is.null(target) && length(target) > 0
  if (hasTarget) {
    targetData = data[target]
    data[target] = NULL
  }
  # determine the numeric & factor cols
  cols.numeric = sapply(data, is.numeric)
  cols.numeric = names(data)[cols.numeric]
  cols.factor = sapply(data, class) == "factor"
  cols.factor = names(data)[cols.factor]
  
  naToZero = function(x) ifelse(is.na(x), 0, x)
  
  # drop cols that have low variance
  # FIXME: does this work with NAs?
  var.numeric = naToZero(sapply(data[cols.numeric], var, na.rm = TRUE))
  nzv.drop.numeric = var.numeric < nzv.cutoff.numeric
  ndsplit = split(cols.numeric, nzv.drop.numeric)
  nzv.drop.numeric = ndsplit$`TRUE`
  cols.numeric = ndsplit$`FALSE`
  
  highestFactorFrequency = function(x){
    sort(table(x, useNA = "ifany"), TRUE)[1] / length(x)
  }
  var.factor = sapply(data[cols.factor], highestFactorFrequency)
  nzv.drop.factor = var.factor > 1 - nzv.cutoff.factor
  ndsplit = split(cols.factor, nzv.drop.factor)
  nzv.drop.factor = ndsplit$`TRUE`
  cols.factor = ndsplit$`FALSE`
  
  # need to prevent this from being NULL!
  ppobject$dropcols = c(character(0), nzv.drop.factor, nzv.drop.numeric)
  ppobject$cols.factor = cols.factor
  ppobject$cols.numeric = cols.numeric
  
  data = data[names(data) %nin% ppobject$dropcols]
  
  # univariate transformation (only on numerics)
  if (length(cols.numeric) > 0 && univariate.trafo != "off") {
    ndata = as.matrix(data[cols.numeric])
    if (univariate.trafo %in% c("center", "scale", "centerscale")) {
      
      center = univariate.trafo %in% c("center", "centerscale")
      scale = univariate.trafo %in% c("scale", "centerscale")
      
      ndata = scale(ndata, center = center, scale = scale)
      
      ppobject$center = attr(ndata, "scaled:center")
      ppobject$scale = attr(ndata, "scaled:scale")
      
    } else {  # univariate.trafo == "range"
      rng = apply(ndata, 2, range, na.rm = TRUE)
      
      ppobject$scale = apply(rng, 2, diff)
      ppobject$center = apply(rng, 2, mean) - ppobject$scale / 2
      
      ndata = scale(ndata, center = ppobject$center, scale = ppobject$scale)
    }
    data[cols.numeric] = ndata
  }
  
  if (length(cols.numeric) > 0 && impute.numeric != "off") {
    if (impute.numeric == "remove.na") {
      delendum = naRows(data, cols.numeric)
      data = data[!delendum, ]
      if (hasTarget) {
        targetData = targetData[!delendum, ]
      }
    } else {
      ppobject$pop.numeric = lapply(data[cols.numeric], function(x) {
            res = switch(impute.numeric,
                mean = mean(x, na.rm = TRUE),
                median = median(x, na.rm = TRUE),
                hist = x[!is.na(x)])
            if (length(res) == 0 || identical(res, NaN) ||
                (length(res) == 1 && is.na(res))) {
              res = 0
            }
            res
          })  # FIXME: maybe do rounding for mean / median and integers
      if (length(ppobject$pop.numeric) == 0) {
        # this /shouldn't/ happen, but just in case
        ppobject$pop.numeric = 0
      }
      data[cols.numeric] = mapply(imputeRandom, data[cols.numeric],
          ppobject$pop.numeric, SIMPLIFY = FALSE)
    }
  }
  
  if (length(cols.factor) > 0 && impute.factor != "off") {
    if (impute.factor == "remove.na") {
      delendum = naRows(data, cols.factor)
      data = data[!delendum, ]
      if (hasTarget) {
        targetData = targetData[!delendum, ]
      }
    } else if (impute.factor == "distinct") {
      data[cols.factor] = lapply(data[cols.factor], function(f) {
            ret = addNA(f, ifany = FALSE)
            # if the level name is `NA`, it will removed when copying which
            # crashes some learners. Therefore we give it a 'unique' name here.
            # If preprocess is called like this twice in a row, this adds an
            # empty new NA level first (since all NAs were removed already) and
            # then unified with the existing automlr.auxlevel.NA, so everything
            # works as it should.
            levels(ret) = ifelse(is.na(levels(ret)), "automlr.auxlevel.NA",
                levels(ret))
            ret
          })
    } else {
      ppobject$pop.factor = lapply(data[cols.factor], function(x) {
            res = switch(impute.factor,
                mode = {
                  ftab = sort(table(x), TRUE)
                  # if there is a tie, get all tieing levels
                  names(ftab)[ftab == ftab[1]]
                },
                hist = x[!is.na(x)])
            if (length(res) == 0) {
              res = levels(x)[1]
            }
            res
          })
      if (length(ppobject$pop.factor) == 0) {
        ppobject$pop.factor = levels
      }
      data[cols.factor] = mapply(imputeRandom, data[cols.factor],
          ppobject$pop.factor, SIMPLIFY = FALSE)
    }
  }
  
  if (length(cols.numeric) > 0) {
    if (multivariate.trafo == "pca") {
      pcr = prcomp(data[cols.numeric], center = FALSE)
      data[cols.numeric] = pcr$x
      ppobject$rotation = pcr$rotation
    } else if (multivariate.trafo == "ica") { 
      requirePackages("fastICA", why = "preProcess", default.method = "load")
      ica = fastICA::fastICA(data[cols.numeric],
          length(cols.numeric), method = "C")
      data[cols.numeric] = ica$S
      ppobject$rotation = ica$K %*% ica$W
    }
  }
  
  if (hasTarget) {
    data[target] = targetData
  }
  
  if (hasTarget && feature.filter != "off") {
    ## If feature.filter ever works with empty target, dummy.task would need to
    # be "cluster".
    #if (length(target) == 0) {  # target is empty --> cluster task.
    #  dummyTask = makeClusterTask("dummy", data)
    #} else
    if (length(target) == 1) {
      # target is numeric --> regression task; otherwise classification
      if (is.numeric(targetData[[1]])) {
        dummyTask = makeRegrTask("dummy", data, target)
      } else {
        # if data is not numeric, logical nor factorial an error is thrown.
        dummyTask = makeClassifTask("dummy", data, target)
      }
    } else {
      # two target columns, first numeric, second numeric or
      # logical -> survival task
      if (length(target) == 2 && is.numeric(targetData[[1]]) &&
          (is.numeric(targetData[[2]]) || is.logical(targetData[[2]]))) {
        dummyTask = makeSurvTask("dummy", data, target,
            ifelse(is.numeric(targetData[[2]]), "icens", "rcens"))
      } else {
        # last default: multilabel. If target is not logical, this will throw an
        # error.
        dummyTask = makeMultilabelTask("dummy", data, target)
      }
    }
    # missing: costsens. The preprocessWrapper interface does not allow us
    # to distinguish cost sensitive tasks from clustering tasks.
    
    filteredTask = filterFeatures(dummyTask, method = feature.filter,
        threshold = feature.filter.thresh)
    
    oldcols = colnames(data)
    data = getTaskData(filteredTask)
    # now we populate 'dropcols2': the columns that get dropped *after*
    # rotation etc.
    ppobject$dropcols2 = c(ppobject$dropcols, setdiff(oldcols, colnames(data)))
  }
  
  if (keep.data) {
    ppobject$data = data
  }
  
  ppobject
}

predict.ampreproc = function(object, newdata, ...) {
  # all right lets go
  # drop low variance cols
  newdata = newdata[names(newdata) %nin% object$dropcols]
  badRows = NULL
  # univariate trafo
  if (length(object$cols.numeric) > 0) {
    if (object$univariate.trafo != "off") {
      ndata = as.matrix(newdata[object$cols.numeric])
      ndata = scale(ndata, center = coalesce(object$center, FALSE),
          scale = coalesce(object$scale, FALSE))
      newdata[object$cols.numeric] = ndata
    }
    
    if (object$impute.numeric == "remove.na") {
      badRows = naRows(newdata, object$cols.numeric)
      if (length(object$cols.factor) > 0 &&
          object$impute.factor == "remove.na") {
        # badRows should represent all the missing rows, so we throw out
        # numerics and factors in one go
        badRows = badRows | naRows(newdata, object$cols.factor)
      }
      newdata = newdata[!badRows, ]
    } else if (object$impute.numeric != "off") {
      newdata[object$cols.numeric] = mapply(imputeRandom,
          newdata[object$cols.numeric], object$pop.numeric, SIMPLIFY = FALSE)
    }
    
    if (object$multivariate.trafo != "off") {
      newdata[object$cols.numeric] =
          as.matrix(newdata[object$cols.numeric]) %*% object$rotation
    }
  }
  
  if (length(object$cols.factor) > 0) {
    if (object$impute.factor == "remove.na") {
      if (is.null(badRows)) {
        # only do this if we haven't done it further above.
        badRows = naRows(newdata, object$cols.factor)
        newdata = newdata[!badRows, ]
      }
    } else if (object$impute.factor == "distinct") {
      newdata[object$cols.factor] = lapply(newdata[object$cols.factor],
          function(f) {
            ret = addNA(f, ifany = FALSE)
            levels(ret) = ifelse(is.na(levels(ret)), "automlr.auxlevel.NA",
                levels(ret))
            ret
          })
      
    } else if (object$impute.factor != "off"){
      newdata[object$cols.factor] = mapply(imputeRandom,
          newdata[object$cols.factor], object$pop.factor, SIMPLIFY = FALSE)
    }
  }
  
  newdata = newdata[names(newdata) %nin% object$dropcols2]
  attr(newdata, "badRows") = badRows
  newdata
}

naRows = function(data, cols) {
  Reduce(`|`, lapply(data[cols], is.na))
}

imputeRandom = function(x, pop) {
  assert(all(!is.na(pop)))
  assert(length(pop) > 0)
  if (length(pop) == 1) {
    # this is why I hate R
    replacement = pop
  } else {
    replacement = sample(pop, size = sum(is.na(x)), replace = TRUE)
  }
  x[is.na(x)] = replacement
  x
}

#' @title Wrap learner with preProcess function
#' 
#' @description
#' Fuse the learner with the automlr preProcess function.
#' 
#' NOTE 1:\cr
#' some of these arguments are useless dependent on the format of the data: If
#' there are no numeric columns, the \code{*numeric-arguments} have no effect etc.
#' 
#' Note 2:\cr
#' setting \code{ppa.nzv.cutoff.*} to their maximum values would effectively
#' remove all numeric / factor columns, therefore allowing conversion for
#' learners that don't support some types.
#' 
#' @param learner [\code{Learner}]\cr
#'   The mlr learner object
#' @param ... [any]\cr
#'   Additional parameters passed to \code{\link{preProcess}}.
#' 
#' @export
makePreprocWrapperAm = function (learner, ...) {
  par.set = makeParamSet(
      makeNumericLearnerParam("ppa.nzv.cutoff.numeric", lower = 0,
          default = 0),
      makeNumericLearnerParam("ppa.nzv.cutoff.factor", lower = 0, upper = 1,
          default = 0),
      makeDiscreteLearnerParam("ppa.univariate.trafo",
          c("off", "center", "scale", "centerscale", "range"), default = "off"),
      makeDiscreteLearnerParam("ppa.impute.numeric",
          c("remove.na", "mean", "median", "hist", "off"), default = "off"),
      makeDiscreteLearnerParam("ppa.impute.factor",
          c("remove.na", "distinct", "mode", "hist", "off"), default = "off"),
      makeDiscreteLearnerParam("ppa.multivariate.trafo",
          c("off", "pca", "ica"), default = "off"),
      makeDiscreteLearnerParam("ppa.feature.filter",
          c("off", "information.gain", "chi.squared", "rf.importance"),
          default = "off"),
      makeNumericLearnerParam("ppa.feature.filter.thresh",
          lower = 0, default = 0,
          requires = quote(ppa.feature.filter != "off")))

  par.vals = getDefaults(par.set)
  par.vals = insert(par.vals, list(...))

  wrapper = wrapLearner("PreprocWrapperAm", "pwa", "PreprocWrapperAm",
      learner = learner,
      par.set = c(makeAllTrainPars(getParamSet(learner)), par.set),
      par.vals = c(getHyperPars(learner), par.vals))
}

trainLearner.PreprocWrapperAm = function(.learner, .task, .subset,
    .weights = NULL, ...) {
  
  
  
  tdesc = getTaskDescription(.task)
  ttype = getTaskType(.task)
  args = .learner$par.vals
  inargs = grepl("^ppa\\.", names(args))

  .learner$learner = setHyperPars(.learner$learner, par.vals = args[!inargs])

  args = args[inargs]
  names(args) = sub("ppa.", "", names(args), fixed = TRUE)
  
  
  
  
  trivialTask = dropFeatures(.task, getTaskFeatureNames(.task))
  trivialModel = train(.learner, task = trivialTask, subset = .subset,
      weights = .weights)

  data = getTaskData(.task, .subset)
  target = getTaskTargetNames(.task)
  if (ttype == "costsens") {
    costs = getTaskCosts(.task)[.subset, , drop = FALSE]
    colnames(costs) = paste0("AMLR.COST.", colnames(costs))
    target = colnames(costs)
    data = cbind(data, costs)
  }

  ppobject = do.call(preProcess,
      c(list(data = data, target = target, keep.data = TRUE), args))
  data = ppobject$data
  ppobject$data = NULL
  
  constructor = switch(ttype,
      classif = makeClassifTask,
      cluster = makeClusterTask,
      costsens = makeCostSensTask,
      multilabel = makeMultilabelTask,
      regr = makeRegrTask,
      surv = makeSurvTask)
  
  if (ttype == 'costsens') {
    cost = data[, target, drop = FALSE]
    data = data[, !(colnames(data) %in% target), drop = FALSE]
  }
  
  constructorArgs = list(
      id = getTaskId(.task),
      data = data,
#      weights = we drop weights and blocks, since preproc doesnt transform them
#      blocking = 
      fixup.data = "no")

  if (!(ttype %in% c('cluster', 'costsens'))) {
    constructorArgs$target = target
  }
  if (ttype == 'classif' && length(tdesc$class.levels) == 2) {
    constructorArgs$positive = tdesc$positive
  }
  if (ttype == 'costsens') {
    constructorArgs$costs = cost
  }
  if (ttype == 'surv') {
    constructorArgs$censoring = tdesc$censoring
  }

  .task = do.call(constructor, constructorArgs)
  .subset = seq_len(getTaskSize(.task))
  .weights = NULL  # not handling weights, so we have to reset these
  result = NextMethod("trainLearner")
  result$control = ppobject

  result$trivialModel = trivialModel

  result
}

predictLearner.PreprocWrapperAm = function(.learner, .model, .newdata,
    ...) {

  # first create a bogus result vector / table
  result = getPredictionResponse(predict(.model$learner.model$trivialModel,
          newdata = .newdata))


  .newdata = predict(.model$learner.model$control, .newdata)
  newResultIdx = !coalesce(attr(.newdata, "badRows"), FALSE)

  newResult = NextMethod("predictLearner")

  if (is.null(nrow(result))) {
    result[newResultIdx] = newResult
  } else {
    result[newResultIdx, , drop=FALSE] = newResult
  }
  result
}

#' @export
getLearnerProperties.PreprocWrapperAm = function(learner) {
  
  props = NextMethod("getLearnerProperties")
  # this is only half a lie; unfortunately we cannot dynamically change this
  # according to hyperparameters, since mlr doesn't expect this to change.
  props = union(props, c("missings", "factors", "ordered", "numerics"))
  props
}

