
#' A list of learners with corresponding \code{par.set}s that can be searched over.
#' 
#' This is a list of learners that the infinitely wise developer of this package
#' collected \emph{himself} that work well with automlr.
#' @include autolearner.R
#' @export
autolearners.small = list(
    autolearner(makeLearner("classif.ksvm"), "learner",
        makeParamSet(
              makeDiscreteParam("C", values = 2^(-2:2)),
              makeDiscreteParam("sigma", values = 2^(-2:2))
        )),
    autolearner(makeLearner("classif.randomForest"), "learner",
        makeParamSet(
              makeIntegerParam("ntree", lower=1, upper=500)
        ))
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
    )

autolearners = list(
    autolearner(makeLearner("classif.naiveBayes"), "learner",
        makeParamSet(
            makeNumericParam("laplace", lower=0, upper=1)
        )),
    autolearner(makeLearner("classif.binomial"), "learner",
        makeParamSet(
            makeDiscreteParam("link", c("logit", "probit", "cloglog", "cauchit", "log"))
        )),
    autolearner(makeLearner("classif.randomForest"), "learner",
        makeParamSet(
            makeIntegerParam("ntree", lower=1, upper=1000),
            makeIntegerParam("nodesize", lower=1, upper=5)
        )),
    autolearner(makeLearner("classif.ksvm"), "learner",
        makeParamSet(
            makeDiscreteParam("type", c("C-svc", "nu-svc", "C-bsvc")),
            makeDiscreteParam("kernel", c("rbfdot", "polydot")),
            makeNumericParam("C", lower=log(0.5), upper=log(4), trafo=exp),
            makeNumericParam("sigma", lower=log(0.5), upper=log(4), trafo=exp, reqires=quote(kernel == "rbfdot")),
            makeIntegerParam("degree", lower=1, upper=5, requires=quote(kernel == "polydot")),
            makeNumericParam("scale", lower=log(0.1), upper=log(10), trafo=exp, requires=quote(kernel == "polydot")),
            makeNumericParam("nu", lower=log(0.05), upper=log(0.5), trafo=exp, requires=quote(type == "nu-svc"))
        )),
    autolearner(makeLearner("classif.avNNet"), "learner",
        makeParamSet(
            makeNumericParam("decay", lower=log(.0001), upper=log(.2), trafo=exp)
        ))
    )