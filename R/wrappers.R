
#' @include preprocess.R autolearner.R mlrLearners.R

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
#' @export
# FIXME: the dirtiest of hacks
mlrWrappers = function() makeNamedAlList(
    autolearner(
        stacktype = "requiredwrapper",
        searchspace = list(
            sp("ppa.nzv.cutoff.numeric", "real", c(.Machine$double.eps, 1),
                trafo = function(x) -log(x),
                req = quote(automlr.has.numerics == TRUE)),
            sp("ppa.nzv.cutoff.factor.AMLRFIX1", "real", c(1, 1),
                req = quote(automlr.remove.factors == TRUE)),
            sp("ppa.nzv.cutoff.factor", "real", c(0, 1),
                req = quote(automlr.has.factors && !automlr.remove.factors)),
            sp("ppa.univariate.trafo", "cat",
                c("off", "center", "scale", "centerscale", "range"),
                req = quote(automlr.has.numerics == TRUE)),
            sp("ppa.impute.numeric", "cat",
                c("remove.na", "mean", "median", "hist"),
                req = quote(automlr.has.numerics && automlr.remove.missings)),
            sp("ppa.impute.factor", "cat",
                c("remove.na", "distinct", "mode", "hist"),
                req = quote(automlr.has.factors && (!automlr.remove.factors) &&
                        automlr.remove.missings)),
            sp("ppa.multivariate.trafo", "cat", c("off", "pca", "ica"),
                req = quote(automlr.has.numerics == TRUE)),
            sp("ppa.feature.filter", "cat",
                c("off", getSupportedFilterMethods(c("information.gain",
                            "chi.squared", "rf.importance")))),
            sp("ppa.feature.filter.thresh", "real", c(.Machine$double.eps, 1),
                trafo = function(x) -log(x),
                req = quote(ppa.feature.filter != "off"))),
        learner = autoWrapper(
            name = "ampreproc",
            constructor = makePreprocWrapperAm,
            conversion = function(x) switch(x,
                  missings = c("missings", ""),
                  factors = c("factors", ""),
                  ordered = c("ordered", ""),
                  x))))


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
#' @export
# FIXME: the dirtiest of hacks
mlrLearners = function() c(mlrLearnersNoWrap, mlrWrappers)