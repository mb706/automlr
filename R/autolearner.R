#' Create \code{Autolearner} objects
#' 
#' \code{Autolearner} objects wrap mlr \code{Learner} objects as well as preprocessing functions
#' and provide additional
#' meta information. This is used to define the automlr searchspace.
#' @param learner An mlr Learner object or similar, depending on stacktype.
#' @param stacktype character(1) describing how this object can be connected with other learners.
#' Must be one of \code{preprocessing} (e.g. feature selection), \code{learner}, \code{wrapper}
#' (is combined with another learner, e.g. bagging), \code{multiwrapper} (e.g. is combined with
#' multiple different learners, e.g. stacking).
#' @param searchspace a \code{ParamSet} object reqpresenting the relevant parameter search space.
#' @export
autolearner.old = function(learner, searchspace=list(), stacktype="learner") {
  makeS3Obj("Autolearner",
            learner=learner,
            searchspace=searchspace,
            stacktype=stacktype)
}


autolearner = function(learner, searchspace, stacktype="learner") {
  
}

#' Define the searchspace parameter in a short form
#' 
#' @param name the name of the parameter, must match the id of the \code{Param} it refers to.
#' @param values if \code{type} is \code{real} or \code{int}, it gives the lower and upper bound.
#'        if \code{type} is \code{cat}, it is a character vector of possible values. If \code{type}
#'        is \code{fix}, only one value (the one to be fixed) must be given. If \code{type} is
#'        code{bool} or \code{def}, it is ignored.
#' @param type the type of the parameter: \code{real} (numeric), \code{int} (integer), \code{cat}
#'        (discrete), \code{bool} (logical), \code{fix} (fixed value of whatever type), \code{def}
#'        using default value / not setting the value.
#' @param trafo may be "exp" for exponential transformation. Transforms integer parameters in a 
#'        smart way. Only applies for \code{real} and \code{int} values.
#' @param id may be given to identify parameters of different learners having the same function.
#' @param dummy this is a dummy variable that has no equivalent in the learners searchspace.
#' @param req A requirement for the variable to have effect
#' @param dim the number of dimensions of this variable
#' @param 
sp = function(name, type="real", values=NULL, trafo=NULL, id=NULL, dummy=FALSE, req=NULL, dim=1) {
  makeS3Obj("searchparam", name=name, values=values, type=type, trafo=trafo,id=id, dummy=dummy,
      req=req, dim=dim)
}