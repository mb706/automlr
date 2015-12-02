
#' A list of learners with corresponding \code{par.set}s that can be searched over.
#' 
#' This is a list of learners that the infinitely wise developer of this package
#' collected \emph{himself} that work well with automlr.
#' @include autolearner.R
#' @export
autolearners = list(
    autolearner(makeLearner("classif.ksvm"), "learner"),
    autolearner(makeLearner("classif.randomForest"), "learner")
    )