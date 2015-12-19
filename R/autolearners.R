
#' A list of learners with corresponding \code{par.set}s that can be searched over.
#' 
#' This is a list of learners that the infinitely wise developer of this package
#' collected \emph{himself} that work well with automlr.
#' @include autolearner.R
#' @export
autolearners = list(
    autolearner(makeLearner("classif.ksvm"), "learner",
        makeParamSet(
              makeDiscreteParam("C", values = 2^(-2:2)),
              makeDiscreteParam("sigma", values = 2^(-2:2))
        )),
    autolearner(makeLearner("classif.randomForest"), "learner",
        makeParamSet(
              makeIntegerParam("ntree", lower=1, upper=500)
        )),
    autolearner(function(x) makePreprocWrapperCaret(x, ppc.na.remove=FALSE), "wrapper",
        makeParamSet(
              makeLogicalParam()
              makeLogicalParam("ppc.BoxCox", requires=quote(!(ppc.YeoJohnson || ppc.expoTrans))),  # this doesn't work
              makeLogicalParam("ppc.YeoJohnson"),
              makeLogicalParam("ppc.expoTrans"),
              ## pca / ica / spatialSign / knnImpute imply either (center & scale) or (range), DEPENDING ON WHETHER range IS GIVEN AT ALL. FUCK.
              makeLogicalParam("ppc.center"),
              makeLogicalParam("ppc.scale"),
              makeLogicalParam("ppc.range", requires=quote(!ppc.center && !ppc.scale && !ppc.BoxCox)),
              makeLogicalParam("ppc.knnImpute", requires=quote(!(ppc.bagImpute || ppc.medianImpute))),
              makeLogicalParam("ppc.bagImpute", requires=quote(!(ppc.knnImpute || ppc.medianImpute))),
              makeLogicalParam("ppc.medianImpute", requires=quote(!(ppc.knnImpute || ppc.bagImpute))),
              makeLogicalParam("ppc.pca", requires=quote(!ppc.ica)),
              makeLogicalParam("ppc.ica", requires=quote(!ppc.pca)),
              makeLogicalParam("ppc.spatialSign"),
              makeLogicalParam("ppc.pcaAbsoluteCompNo", requires=quote((ppc.pca))),
              makeNumericParam("ppc.thresh", requires=quote(ppc.pca && !ppc.pcaAbsoluteCompNo)),
              makeIntegerParam("ppc.pcaComp", requires=quote(ppc.pca && ppc.pcaAbsoluteCompNo)),
              makeIntegerParam("ppc.k", requires=quote((ppc.knnImpute))),
              makeNumericParam("ppc.fudge", requires=quote((ppc.BoxCox)))
        ))
    )