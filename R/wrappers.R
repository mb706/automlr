
#' @include autolearner.R mlrLearners.R

getSupportedFilterMethods = function(suggestions) {
  allFilters = lapply(listFilterMethods(FALSE), as.character)
  allPkgs = rownames(installed.packages())
  presentFilters = allFilters$id[allFilters$package %in% c("", allPkgs)]
  
  absentFilters = setdiff(suggestions, presentFilters)
  if (length(absentFilters) > 0) {
    absentPkgs = unique(allFilters$package[allFilters$id %in% absentFilters])
    warningf(paste("Note that feature filter options '%s' will not be",
            "available since packages '%s' are not installed."),
        collapse(absentFilters, "', "), collapse(absentPkgs, "', '"))
  }
  intersect(suggestions, presentFilters)
}

#' @title
#' A list of wrappers with corresponding \code{par.set}s that can be searched
#' over.
#' 
#' @description
#' This is a list of wrappers that can be used as part of a searchspace.
#' Currently this only includes a preprocessing wrapper, but may in future also
#' include some meta-methods.
#' 
#' @name mlrWrappers
#' @family searchspace
#' @docType data
#' @export
mlrWrappers = list()

mlrWrappers.gen = function() makeNamedAlList()
#
#    autolearner(
#        stacktype = "requiredwrapper",
#        searchspace = list(
#            sp("ppa.nzv.cutoff.numeric", "real", c(.Machine$double.eps, 1),
#                trafo = function(x) -log(x),
#                req = quote(automlr.has.numerics == TRUE)),
#            sp("ppa.nzv.cutoff.factor.AMLRFIX1", "real", c(1, 1),
#                req = quote(automlr.remove.factors == TRUE)),
#            sp("ppa.nzv.cutoff.factor", "real", c(0, 1),
#                req = quote(automlr.has.factors && !automlr.remove.factors)),
#            sp("ppa.univariate.trafo", "cat",
#                c("off", "center", "scale", "centerscale", "range"),
#                req = quote(automlr.has.numerics == TRUE)),
#            sp("ppa.impute.numeric", "cat",
#                c("remove.na", "mean", "median", "hist"),
#                req = quote(automlr.has.numerics && automlr.remove.missings)),
#            sp("ppa.impute.factor", "cat",
#                c("remove.na", "distinct", "mode", "hist"),
#                req = quote(automlr.has.factors && (!automlr.remove.factors) &&
#                        automlr.remove.missings)),
#            sp("ppa.multivariate.trafo", "cat", c("off", "pca", "ica"),
#                req = quote(automlr.has.numerics == TRUE)),
#            sp("ppa.feature.filter", "cat",
#                c("off", getSupportedFilterMethods(c("information.gain",
#                            "chi.squared", "randomForest.importance")))),
#            sp("ppa.feature.filter.thresh", "real", c(.Machine$double.eps, 1),
#                trafo = function(x) -log(x),
#                req = quote(ppa.feature.filter != "off"))),
#        learner = autoWrapper(
#            name = "ampreproc",
#            constructor = makePreprocWrapperAm,
#            conversion = list(missings="factors",
#                factors=c("factors", "numerics"),
#                ordered=c("ordered", "numerics"),
#                numerics = "numerics"))))


#' @title
#' A list of learners with corresponding \code{par.set}s that can be searched
#' over.
#' 
#' @description
#' This is the complete search space as suggested by the automlr package. It
#' includes learners and wrappers.
#' 
#' @name mlrLearners
#' @family searchspace
#' @docType data
#' @export
mlrLearners = list()

mlrLearners.gen = function() c(mlrLearnersNoWrap, mlrWrappers)

# the following is a bit handwavy. I ran these learners in randomsearch and
# looked which learners took, on average, "kinda long".
slowLrn = c('classif.lda', 'classif.mda', 'classif.rrlda',
    'classif.dcSVM', 'classif.rda', 'classif.bartMachine', 'classif.boosting',
    'classif.nodeHarvest', 'classif.randomForestSRC')
if (!all(slowLrn %in% names(mlrLearnersNoWrap))) {
  stop("slowLrn references unknown learner(s) %s",
      setdiff(slowLrn, names(mlrLearnersNoWrap)))
}

# classif.ctree takes a long time also when testtype == MonteCarlo
# classif.multinom also (?)

# classif.randomForestSRC seems to be a memory hog

#' @title
#' A list of fast learners with corresponding \code{par.set}s that can be 
#' earched over.
#' 
#' @description
#' This is a list similar to \code{\link{mlrLearnersNoWrap}}, only it excludes
#' the slowest learners. This decreases average evaluation time significantly
#' and also makes the search space small enough for the \code{mbo} backend.
#' 
#' @name mlrLightweightNoWrap
#' @family searchspace
#' @docType data
#' @export
mlrLightweightNoWrap = mlrLearnersNoWrap[names(mlrLearnersNoWrap) %nin% slowLrn]

#' @title
#' A list of fast learners with corresponding \code{par.set}s that can be 
#' earched over.
#' 
#' @description
#' This is a list similar to \code{\link{mlrLearners}}, only it excludes
#' the slowest learners. This decreases average evaluation time significantly
#' and also makes the search space small enough for the \code{mbo} backend.
#' 
#' @name mlrLightweight
#' @family searchspace
#' @docType data
#' @export
mlrLightweight = list()

mlrLightweight.gen = function() c(mlrLightweightNoWrap, mlrWrappers)

