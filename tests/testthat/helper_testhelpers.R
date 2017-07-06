# Auxiliary functions for checking automlr: Creating dummy learners and tasks,
# checking that the correct learners, are present and that they get the expected
# parameters.

# TODO: put 'all = TRUE' in all expect_warning and expect_error

library("checkmate")

configureMlr()  # set defaults
configureMlr(show.learner.output = TRUE, on.learner.error = "warn")

# Create a data frame with the given number of features. The factorial features
# have nClasses cases.
createTestData = function(nrow, nNumeric = 0, nFactor = 0, nOrdered = 0,
    nClasses = 2) {
  res = as.data.frame(c(
                replicate(nNumeric, rnorm(nrow), FALSE),
                replicate(nFactor,
                    factor(sample(letters[1:nClasses], nrow, TRUE),
                        ordered = FALSE),
                    FALSE),
                replicate(nOrdered,
                    factor(sample(letters[1:nClasses], nrow, TRUE),
                        ordered = TRUE),
                    FALSE)))
  names(res) = c(
      if (nNumeric) paste0("num.", seq_len(nNumeric)),
      if (nFactor) paste0("fac.", seq_len(nFactor)),
      if (nOrdered) paste0("ord.", seq_len(nOrdered)))
  res
}

# Create a classification task with the given number of features.
# Possibly add NAs.
createTestClassifTask = function(id, nrow, nNumeric = 0, nFactor = 0,
    nOrdered = 0, nClasses = 2, missings = FALSE, ...) {
  data = createTestData(nrow, nNumeric, nFactor + 1, nOrdered, nClasses)
  target = paste0("fac.", nFactor + 1)
  mrow = (seq_len(nrow) %% 3 == 0) & missings
  data[mrow, colnames(data) != target] = NA
  makeClassifTask(id, data, target, ...)
}

# Create a classification task with the given number of features.
# Possibly add NAs.
createTestRegrTask = function(id, nrow, nNumeric = 0, nFactor = 0, nOrdered = 0,
    nClasses = 2, missings = FALSE, ...) {
  data = createTestData(nrow, nNumeric + 1, nFactor, nOrdered, nClasses)
  target = paste0("num.", nNumeric + 1)
  mrow = (seq_len(nrow) %% 3 == 0) & missings
  data[mrow, colnames(data) != target] = NA
  makeRegrTask(id, data, target, ...)
}

# human readable output of list
debuglist = function(l, prefix = "") {
  res = ""
  if (checkmate::testNamed(l)) {
    for (n in sort(names(l))) {
      res = paste0(res, paste0(prefix, n, ": "))
      if (is.list(l[[n]])) {
        res = paste0(res, "\n", debuglist(l[[n]], paste0(prefix, "+")))
      } else {
        res = paste0(res, paste(l[[n]], collapse = ", "), "\n")
      }
    }
  } else {
    for (i in seq_along(l)) {
      showname = names(l)[i]
      if (is.null(showname) || is.na(showname) || showname == "") {
        showname = i
      }
      res = paste0(res, paste0(prefix, showname, ": "))
      if (is.list(l[[i]])) {
        res = paste0(res, "\n", debuglist(l[[i]], paste0(prefix, "+")))
      } else {
        res = paste0(res, paste(l[[i]], collapse = ", "), "\n")
      }
    }
  }
  res
}

# writing list, and corresponding expected output for testthat
debuglistout = function(l) cat("###\n", debuglist(l), "###\n")
expectout = function(l) paste("###\n", debuglist(l), "###")

# train and predict the learner on the task.
# test that the learner has params in 'trainps' during training
# and params in 'predps' during prediction
expect_learner_output = function(learner, task, name, trainps = list(),
    predps = list(), ...) {
  oldopts = getMlrOptions()
  configureMlr(show.learner.output = TRUE)
  wrapperflags = c(".remove.factors", ".remove.ordered", ".remove.NA",
      ".convert.fac2num", ".convert.ord2fac", ".convert.ord2num")
  wrapperArgs = list(...)
  # emulate wrapper output to get the expected string
  wrapperoutput = function(which) capture.output(
      for (w in names(wrapperArgs)) {
        catf("wrapper %s %s", w, which)
        dput(wrapperArgs[[w]][[paste0(w, ".spare1")]])
        dput(wrapperArgs[[w]][[paste0(w, ".spare2")]])
        if (which == "train") {
          for (pname in wrapperflags) {
            pcomplete = paste0(w, pname)
            if (isTRUE(wrapperArgs[[w]][[pcomplete]])) {
              dput(pcomplete)
            }
          }
        }
      })
  expectedlout = expectout(c(list(myname = name), trainps))
  expect_output(model <- train(learner, task),
      paste(c(wrapperoutput("train"), expectedlout), collapse = "\n"),
      fixed = TRUE)
  expectedpout = expectout(c(list(myname = name), predps))
  expect_output(predict(model, task),
      paste(c(wrapperoutput("predict"), expectedpout), collapse = "\n"),
      fixed = TRUE)
  do.call(configureMlr, oldopts)
}

# Create a new learner in the global namespace with given parset and properties
# The created learner makes output about the given parameters and predicts
# randomly.
# isClassif==FALSE -> regr
testLearner = function(name, parset, properties, isClassif = TRUE, ...) {
  constructor = if (isClassif) makeRLearnerClassif else makeRLearnerRegr
  ret = constructor(name, character(0), parset, properties = properties, ...)
  if (isClassif) {
    ret$fix.factors.prediction = TRUE
  }
  pf = globalenv()

  assign(paste0("trainLearner.", name), envir = pf,
      value = function (.learner, .task, .subset, .weights = NULL, ...) {
    debuglistout(list(myname = name, ...))
    list(data = getTaskData(.task, .subset), target = getTaskTargetNames(.task))
  })
  assign(paste0("predictLearner.", name), envir = pf,
      value = function (.learner, .model, .newdata, ...) {
    debuglistout(list(myname = name, ...))
    sample(.model$learner.model$data[[.model$learner.model$target]],
        nrow(.newdata), replace = TRUE)
  })
  ret
}


optionalProps = c("numerics", "factors", "ordered",
    "missings", "twoclass", "multiclass")

# combination of all possible properties
allOPs = do.call(c, lapply(seq_along(optionalProps),
        function(i) combn(optionalProps, i, simplify = FALSE)))

# all possible learners
names(allOPs) = as.character(seq_along(allOPs))
propertyLearners = mapply(testLearner, name = names(allOPs),
    properties = allOPs, MoreArgs = list(parset = makeParamSet()),
    SIMPLIFY = FALSE)
# these learners as automlr input objects
autolearnersBASIC = lapply(propertyLearners, autolearner)

# which learner to expect to be present:
# mustBeHandledList is a list of conditions, at lest one of which must be true.
# The condition is that x must be a subset of the listed properties
defaultExpFun2 = function(mustBeHandledList)
  function(x) {  # yes we curry
    possible = sapply(mustBeHandledList, function(mustBeHandled) {
          # all things that must be handled are present
          all(mustBeHandled %in% x) &&
              # at least one of numerics, factors, ordered is present
              any(intersect(mustBeHandled,
                      c("numerics", "factors", "ordered")) %in% x)
        })
  any(possible)  # at least one type of column can be processed
}

# check that the learners with properties that make `expfun` return TRUE are
# present in the automlr learner.
checkLearnersPresent = function(autolearnersPL, task, propertiesExpected,
    optionalProperties = character(0),
    expfun = defaultExpFun2(propertiesExpected), debugOut = FALSE) {
  expectedLearners = names(allOPs)[sapply(allOPs, expfun)]
  ps = getParamSet(buildLearners(autolearnersPL, task))
  presentLearners = unlist(ps$pars$selected.learner$values)
  if (debugOut) {
    catf("Expected Learners: %s", paste(expectedLearners, collapse = ", "))
    catf("Present Learners: %s", paste(presentLearners, collapse = ", "))
  }
  expect_set_equal(presentLearners, expectedLearners)
}

# create a classification task that requires the given properties to be present.
createTestWithProperties = function(properties) {
  assert(any(c("twoclass", "multiclass") %in% properties))
  nClasses = 2 + ("multiclass" %in% properties)
  createTestClassifTask("t", 200,
                        nNumeric = as.numeric("numerics" %in% properties),
                        nFactor = as.numeric("factors" %in% properties),
                        nOrdered = as.numeric("ordered" %in% properties),
                        nClasses = nClasses,
                        missings = as.numeric("missings" %in% properties))
}

# check that for any combination of numerics, factors, ordered, missings, and
# for either twoclass or multiclass, the learners included by automlr conform
# to the required learners for the tasks created by createTestWithProperties.
checkWrapperEffectEx = function(autolearnersPL, transformation = list,
    debugOut = FALSE, ...) {
  testprops = c("numerics", "factors", "ordered")
  for (classness in c("twoclass", "multiclass")) {
    for (numprops in seq_along(testprops)) {
      for (chosenprops in combn(testprops, numprops, simplify = FALSE)) {
        for (doMissings in c(FALSE, TRUE)) {
          totalprops = c(classness, if(doMissings) "missings", chosenprops)
          testTask = createTestWithProperties(totalprops)
          if (debugOut) {
            print(totalprops)
          }
          checkLearnersPresent(autolearnersPL, testTask,
              transformation(totalprops), debugOut = debugOut, ...)
        }
      }
    }
  }
}

# shorthand for building learners for pid.task
bl = function(...) {
  buildLearners(list(...), pid.task, verbosity = 5)
}

blt = function(learners, task) {
  buildLearners(learners, task, verbosity = 5)
}


# check that x is feasible in the param set, and that all feasible parameters
# are present.
isFeasibleNoneMissing = function(par, x) {
  nalist = rep(NA, length(par$pars))
  names(nalist) = getParamIds(par)
  isFeasible(par, insert(nalist, x))
}

isFeasibleMissingPossible = function(ps, x) {
  assertSubset(names(x), getParamIds(ps))
  for (n in names(x)) {
    par = ps$pars[[n]]
    val = x[[n]]
    if ((!requiresOk(par, x) && !isScalarNA(val)) ||
      !isFeasible(par, val)) {
      return(FALSE)
    }
  }
  TRUE
}

# shorthand for getting list of parameters
getpars = function(learner) getParamSet(learner)$pars

# check that all parameters are set, and that the learner does what is expected.
checkLearnerBehaviour = function(learner, task, params, ...) {
  expect_true(isFeasibleNoneMissing(getParamSet(learner), params))
#predict(train(setHyperPars(learner, par.vals = params), task), task)
  expect_learner_output(setHyperPars(learner, par.vals = params), task, ...)
}


