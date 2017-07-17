


context("searchspace")


suppressSpecificWarning = function(expr, match) {
  withCallingHandlers(expr, warning=function(w) {
    if (length(grep(match, conditionMessage(w)))) {
      invokeRestart("muffleWarning")
    }
  })
}


test_that("each searchspace item works", {

  tsk = subsetTask(pid.task, 1:50)

  for (lrn in mlrLightweightNoWrap) {
    if (!length(lrn$searchspace)) {
      # skipping items with no searchspace
      next
    }
    set.seed(123)
    expect_error(suppressSpecificWarning(capture.output(automlr(tsk,
      budget = c(evals = 1), verbosity=6,
      searchspace = list(lrn), backend = "random")),
      "' and has different \\(but feasible\\) type '|was already set to a value; this value has been removed"), NA)
  }

  # TODO: test the ones with no searchspace
  # TODO: test the ones that have a searchspace that gets hidden for pid.task
})


test_that("each searchspace item works with different timeout backend.", {

  tsk = subsetTask(pid.task, 1:50)

  setDefaultRWTBackend("fork")

  for (lrn in mlrLightweightNoWrapNoJava) {
    if (!length(lrn$searchspace)) {
      # skipping items with no searchspace
      next
    }
    set.seed(123)
    expect_error(suppressSpecificWarning(capture.output(automlr(tsk,
      budget = c(evals = 1), verbosity=6,
      searchspace = list(lrn), backend = "random")),
      "' and has different \\(but feasible\\) type '|was already set to a value; this value has been removed"), NA)
  }

  setDefaultRWTBackend("native")

})
