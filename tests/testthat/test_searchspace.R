


context("searchspace")

test_that("each searchspace item works", {


  tsk = subsetTask(pid.task, 1:50)


  for (lrn in mlrLightweightNoWrap) {
    print(lrn$learner)
    if (!length(lrn$searchspace)) {
      # skipping items with no searchspace
      next
    }
    set.seed(123)
    try(automlr(tsk,
      budget = c(evals = 1), verbosity=6,
      searchspace = list(lrn), backend = "random")
      , FALSE)

  }


  # classif.binomial?

  # classif.lqa: 'gamma' infeasible bounds 1, 10????
  # linDA: 'no parameters were passed'
  # quaDA, geoDA: "no parameters were passed"
  # lvq1: no params passed
  # naiveBayes: no params passed


  # neuralnet fails

})
