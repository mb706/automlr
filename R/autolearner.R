#' Create \code{Autolearner} objects
#' 
#' \code{Autolearner} objects wrap mlr \code{Learner} objects as well as preprocessing functions
#' and provide additional
#' meta information. This is used to define the automlr searchspace.
#' @param learner An mlr Learner object or similar, depending on stacktype.
#' @param stacktype character(1) describing how this object can be connected with other learners.
#' Must be one of \code{preprocessing} (e.g. feature selection), \code{learner}, \code{meta}
#' (is combined with another learner, e.g. bagging), \code{multimeta} (e.g. is combined with
#' multiple different learners, e.g. stacking).
#' @export
autolearner = function(learner, stacktype, searchspace=NULL) {
  makeS3Obj("Autolearner",
            learner=learner,
            stacktype=stacktype,
            searchspace=coalesce(pspace, filterParams(getParamSet(learner), tunable=TRUE)))
}