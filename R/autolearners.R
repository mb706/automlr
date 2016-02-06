
#' A list of learners with corresponding \code{par.set}s that can be searched over.
#' 
#' This is a list of learners that the infinitely wise developer of this package
#' collected \emph{himself} that work well with automlr.
#' @include autolearner.R
#' @export


#    autolearner(function(x) makePreprocWrapperCaret(x, ppc.na.remove=FALSE), "wrapper",
#        makeParamSet(
#              makeLogicalParam("ppc.BoxCox", requires=quote(!(ppc.YeoJohnson || ppc.expoTrans))),  # this doesn't work
#              makeLogicalParam("ppc.YeoJohnson"),
#              makeLogicalParam("ppc.expoTrans"),
#              ## pca / ica / spatialSign / knnImpute imply either (center & scale) or (range), DEPENDING ON WHETHER range IS GIVEN AT ALL. FUCK.
#              makeLogicalParam("ppc.center"),
#              makeLogicalParam("ppc.scale"),
#              makeLogicalParam("ppc.range", requires=quote(!ppc.center && !ppc.scale && !ppc.BoxCox)),
#              makeLogicalParam("ppc.knnImpute", requires=quote(!(ppc.bagImpute || ppc.medianImpute))),
#              makeLogicalParam("ppc.bagImpute", requires=quote(!(ppc.knnImpute || ppc.medianImpute))),
#              makeLogicalParam("ppc.medianImpute", requires=quote(!(ppc.knnImpute || ppc.bagImpute))),
#              makeLogicalParam("ppc.pca", requires=quote(!ppc.ica)),
#              makeLogicalParam("ppc.ica", requires=quote(!ppc.pca)),
#              makeLogicalParam("ppc.spatialSign"),
#              makeLogicalParam("ppc.pcaAbsoluteCompNo", requires=quote((ppc.pca))),
#              makeNumericParam("ppc.thresh", requires=quote(ppc.pca && !ppc.pcaAbsoluteCompNo)),
#              makeIntegerParam("ppc.pcaComp", requires=quote(ppc.pca && ppc.pcaAbsoluteCompNo)),
#              makeIntegerParam("ppc.k", requires=quote((ppc.knnImpute))),
#              makeNumericParam("ppc.fudge", requires=quote((ppc.BoxCox)))
#        ))

makeNamedAlList = function(l) {  # make a named list, more convenient to use
    n = sapply(l, function(item) ifelse(is.character(item$learner, item$learner, item$learner$id)))
    names(l) = n
    l
}

autolearners = makeNamedAlList(
    autolearner("classif.logreg"),
    autolearner("classif.probit"),
    autolearner("classif.plr",
        list(
# ** vp
            sp(lambda, "real", c(1e-5, 100), "exp"),
            sp(cp.type, "cat", c("bic", "aic")),
# ** cp
            sp(cp, "fix", NULL))),
    autolearner("classif.lda",
        list(
# ** vp
            sp(method, "cat", c("moment", "mle", "mve", "t"), id="da.method"),
            sp(nu, "int", c(2, 64), "exp", id="da.nu", req=quote(method=='t')),
            sp(predict.method, "cat", c("plug-in", "predictive", "debiased"), id="da.pm"),
# ** dp
            sp(tol, "def", .0001),
            sp(CV, "def", FALSE))),
    autolearner("classif.qda",
        list(
# ** vp
            sp(method, "cat", c("moment", "mle", "mve", "t"), id="da.method"),
            sp(nu, "int", c(2, 64), "exp", id="da.nu", req=quote(method=='t')),
            sp(predict.method, "cat", c("plug-in", "predictive", "debiased"), id="da.pm"))),
    autolearner("classif.linDA",
        list(
# ** dp
            sp(validation, "def", NULL))),
    autolearner("classif.sparseLDA",
        list(
# ** vp
            sp(lambda, "real", c(1e-10, 1), "exp"),
            sp(maxIte, "int", c(50, 400), "exp"),
# ** dp
            sp(trace, "def", FALSE),
            sp(tol, "def", 1e-6))),
    autolearner("classif.rrlda",
        list(
# ** vp
            sp(lambda, "real", c(0.01, 10)),
            sp(hp, "real", c(0.3, 1)),
            sp(nssamples, "int", c(10, 1000), "exp"),
            sp(maxit, "int", c(50, 400), "exp"),
            sp(penalty, "cat", c("L1", "L2")),
# ** dp
            sp(prior, "def", NULL))))