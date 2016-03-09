
#' @include preprocess.R

autowrappers = makeNamedAlList(
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
# don't need this, it's the default:
#           sp("ppa.impute.numeric.AMLRFIX1", "cat", c("off"),
#               req = quote(!automlr.has.numerics || !automlr.remove.missings)),
            sp("ppa.impute.factor", "cat",
                c("remove.na", "distinct", "mode", "hist"),
                req = quote(automlr.has.factors && (!automlr.remove.factors) &&
                        automlr.remove.missings)),
#           sp("ppa.impute.factor.AMLRFIX1", "cat", c("off"),
#               req = quote(!automlr.has.factors || automlr.remove.factors ||
#                       !automlr.remove.missings)),
            sp("ppa.multivariate.trafo", "cat", c("off", "pca", "ica"),
                req = quote(automlr.has.numerics == TRUE)),
            sp("ppa.feature.filter", "cat",
                # FIXME: why does rf.importance crash in the following?
                c("off", "information.gain", "chi.squared")),
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