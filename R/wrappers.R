
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

mlrWrappers.gen = function() makeNamedAlList(
# **** feature filters
  autolearner(autoWrapper("numfilter", automlrNumFilterCPO(id = "numfilter"), "numerics"),
    list(
        sp("numfilter.filter", "cat", amlr.numfilters),
        sp("numfilter.cutoff", "real", c(0, 1)),
        sp("numfilter.cutofftype", "cat", c("perc.of.features", "perc.of.range"))), "wrapper"),
  autolearner(autoWrapper("facfilter", automlrFacFilterCPO(id = "facfilter"), "factors"),
    list(
        sp("facfilter.filter", "cat", amlr.facfilters),
        sp("facfilter.cutoff", "real", c(0, 1)),
        sp("facfilter.cutofftype", "cat", c("perc.of.features", "perc.of.range"))), "wrapper"),
  autolearner(autoWrapper("ordfilter", automlrOrdFilterCPO(id = "ordfilter"), "ordered"),
    list(
        sp("ordfilter.filter", "cat", amlr.ordfilters),
        sp("ordfilter.cutoff", "real", c(0, 1)),
        sp("ordfilter.cutofftype", "cat", c("perc.of.features", "perc.of.range"))), "wrapper"),
# **** various
  autolearner(autoWrapper("collapse.fact", cpoCollapseFact(id = "collapse.fact"), "factors"),
    list(sp("collapse.fact.max.collapsed.class.prevalence", "real", c(0.1, 0.999), "exp")),
    "wrapper"),
  autolearner(autoWrapper("collapse.ord", cpoCollapseFact(id = "collapse.ord"), "ordered"),
    list(sp("collapse.ord.max.collapsed.class.prevalence", "real", c(0.1, 0.999), "exp")),
    "wrapper"),
  autolearner(autoWrapper("scale", cpoScale(id = "scale.nums"), "numerics"),
    list(sp("scale.nums.scale", "bool"), sp("scale.nums.center", "bool")), "wrapper"),
  autolearner(autoWrapper("pca", cpoImputeMedian(id = "pcaimp") %>>%
      fremover(id = "pcaimp.fremove") %>>%
      cpoPca(center = FALSE, scale = FALSE, id = "pca.num"), "numerics"),
    list(), "wrapper"),
# **** imputers
  autolearner(autoWrapper("impute.mean", cpoImputeMean(make.dummy.cols = FALSE) %>>% fremover(id = "nfremover"),
    "numerics", "missings"), stacktype = "wrapper"),
  autolearner(autoWrapper("impute.median", cpoImputeMedian(make.dummy.cols = FALSE) %>>% fremover(id = "nfremover"),
    "numerics", "missings"), stacktype = "wrapper"),
  autolearner(autoWrapper("impute.min", cpoImputeMin(id = "impute.min", make.dummy.cols = FALSE) %>>% fremover(id = "nfremover"),
    "numerics", "missings"), list(sp("impute.min.multiplier", "real", c(0, 2))), stacktype = "wrapper"),
  autolearner(autoWrapper("num.impute.hist", cpoImputeHist(id = "numimpute.hist", make.dummy.cols = FALSE) %>>% fremover(id = "nfremover"),
    "numerics", "missings"), stacktype = "wrapper"),
  autolearner(autoWrapper("fac.impute.const", cpoImputeConstFact(),
    "factors", "missings"), stacktype = "wrapper"),
  autolearner(autoWrapper("fac.impute.hist", cpoImputeHist(make.dummy.cols = FALSE, id = "factimpute.hist"),
    "factors", "missings"), stacktype = "wrapper"),
  autolearner(autoWrapper("ord.impute.const", cpoImputeConstFact(),
    "ordered", "missings"), stacktype = "wrapper"),
  autolearner(autoWrapper("ord.impute.hist", cpoImputeHist(make.dummy.cols = FALSE, id = "ordimpute.hist") %>>% fremover(id = "ofremover"),
    "ordered", "missings"), stacktype = "wrapper"),
# **** converters
  autolearner(autoWrapper("dummy.fact.num", cpoDummyEncode(id = "dummy.fact.num"),
      "numerics", "factors"), list(sp("dummy.fact.num.reference.cat", "bool")), "wrapper"),
  autolearner(autoWrapper("dummy.ord.num", cpoDummyEncode(id = "dummy.ord.num"),
      "numerics", "ordered"), list(sp("dummy.ord.num.reference.cat", "bool")), "wrapper"),
  autolearner(autoWrapper("asnum.ord.num", asnumcpo(), "numerics", "ordered"), stacktype = "wrapper"),
  autolearner(autoWrapper("splitnum.num.fac", splitnumcpo(id = "splitnum.num.fac"), "factors", "numerics"),
    list(sp("splitnum.num.fac.numsplits", "int", c(2, 5))), "wrapper"),
  autolearner(autoWrapper("asfac.ord.fac", ordasfaccpo(), "factors", "ordered"), stacktype = "wrapper"),
  autolearner(autoWrapper("splitfac.ord.fac", asnumcpo() %>>% splitnumcpo(id = "splitnum.ord.fac"),
      "factors", "ordered"), list(sp("splitnum.ord.fac.numsplits", "int", c(2, 5))),"wrapper"),
  autolearner(autoWrapper("splitnum.num.ord", splitnumcpo(id = "splitnum.num.ord") %>>% facasordcpo(), "ordered", "numerics"),
    list(sp("splitnum.num.ord.numsplits", "int", c(2, 5))), "wrapper"))


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
slowLrn = c(
    "classif.nodeHarvest",  # currently broken in new R versions
    "classif.neuralnet",
    "classif.bartMachine",  # JVM seems broken.
    "classif.gausspr",
    "classif.earth",  # DLL can not be loaded, too many of them.
    "classif.mda",  # deadlocks
    NULL)

if (!all(slowLrn %in% names(mlrLearnersNoWrap))) {
  stop("slowLrn references unknown learner(s) %s",
      setdiff(slowLrn, names(mlrLearnersNoWrap)))
}

javaLrn = c(
    "classif.extraTrees",
    "classif.IBk",
    "classif.J48",
    "classif.JRip",
    "classif.bartMachine",
    "classif.PART",
    "classif.OneR",
    NULL)

if (!all(javaLrn %in% names(mlrLearnersNoWrap))) {
  stop("slowLrn references unknown learner(s) %s",
      setdiff(javaLrn, names(mlrLearnersNoWrap)))
}

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
#' This is a list similar to \code{\link{mlrLightweightNoWrap}}, but it also
#' excludes java based learners. This makes it possible to use the
#' \dQuote{fork} backend for timeouts.
#'
#' @name mlrLightweightNoWrapNoJava
#' @family searchspace
#' @docType data
#' @export
mlrLightweightNoWrapNoJava = mlrLearnersNoWrap[names(mlrLearnersNoWrap) %nin%
  c(slowLrn, javaLrn)]

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

#' @title
#' A list of fast learners with corresponding \code{par.set}s that can be
#' earched over.
#'
#' @description
#' This is a list similar to \code{\link{mlrLightweight}}, but it also
#' excludes java based learners. This makes it possible to use the
#' \dQuote{fork} backend for timeouts.
#'
#' @name mlrLightweightNoJava
#' @family searchspace
#' @docType data
#' @export
mlrLightweightNoJava = list()

mlrLightweightNoJava.gen = function() {
  c(mlrLightweightNoWrapNoJava, mlrWrappers)
}
