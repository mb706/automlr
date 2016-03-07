
# TODO: put 'all = TRUE' in all expect_warning and expect_error

configureMlr(show.learner.output = TRUE, on.learner.error = "warn")

createTestData = function(nrow, nNumeric = 0, nFactor = 0, nOrdered = 0, nClasses = 2) {
  res = as.data.frame(c(
                replicate(nNumeric, rnorm(nrow), FALSE),
                replicate(nFactor, factor(sample(letters[1:nClasses], nrow, TRUE), ordered = FALSE), FALSE),
                replicate(nOrdered, factor(sample(letters[1:nClasses], nrow, TRUE), ordered = TRUE), FALSE)))
  names(res) = c(
      if (nNumeric) paste0("num.", seq_len(nNumeric)),
      if (nFactor) paste0("fac.", seq_len(nFactor)),
      if (nOrdered) paste0("ord.", seq_len(nOrdered)))
  res
}

createTestClassifTask = function(id, nrow, nNumeric = 0, nFactor = 0, nOrdered = 0, nClasses = 2, missings = FALSE, ...) {
  data = createTestData(nrow, nNumeric, nFactor + 1, nOrdered, nClasses)
  target = paste0("fac.", nFactor + 1)
  mrow = (seq_len(nrow) %% 3 == 0) & missings
  data[mrow, colnames(data) != target] = NA
  makeClassifTask(id, data, target, ...)
}

createTestRegrTask = function(id, nrow, nNumeric = 0, nFactor = 0, nOrdered = 0, nClasses = 2, missings = FALSE, ...) {
  data = createTestData(nrow, nNumeric + 1, nFactor, nOrdered, nClasses)
  target = paste0("num.", nNumeric + 1)
  mrow = (seq_len(nrow) %% 3 == 0) & missings
  data[mrow, colnames(data) != target] = NA
  makeRegrTask(id, data, target, ...)
}

changeColsWrapper  = function (learner, prefix, ...) {
  par.set = makeParamSet(
      makeLogicalLearnerParam(paste0(prefix, ".remove.factors"), default = FALSE),
      makeLogicalLearnerParam(paste0(prefix, ".remove.ordered"), default = FALSE),
      makeLogicalLearnerParam(paste0(prefix, ".remove.NA"), default = FALSE),      
      makeLogicalLearnerParam(paste0(prefix, ".convert.fac2num"), default = FALSE),
      makeLogicalLearnerParam(paste0(prefix, ".convert.ord2fac"), default = FALSE),
      makeLogicalLearnerParam(paste0(prefix, ".convert.ord2num"), default = FALSE),
      makeIntegerLearnerParam(paste0(prefix, ".spare1"), default = 0),
      makeIntegerLearnerParam(paste0(prefix, ".spare2"), default = 0)
  )
  par.vals = getDefaults(par.set)
  par.vals = insert(par.vals, list(...))

  colman = function(args, data) {
    names(args) = sub(paste0("^", prefix, "\\."), "", names(args))
    ordereds = sapply(data, is.ordered)
    if (args$convert.ord2num) {
      data[ordereds] = lapply(data[ordereds], as.numeric)
    }
    factors = sapply(data, is.factor)
    if (args$convert.fac2num) {
      hotencoder = function(col, colname) {
        newcs = lapply(args$levels[[colname]], function(lvl) as.numeric(col == lvl))
        names(newcs) = args$levels[[colname]]
        newcs
      } 
      data = cbind(data, do.call(base::c, mapply(hotencoder, data[factors], names(data)[factors], SIMPLIFY = FALSE)))
      args$remove.factors = TRUE
      ordereds = sapply(data, is.ordered)
      factors = sapply(data, is.factor)
    }
    if (args$convert.ord2fac) {
      data[ordereds] = lapply(data[ordereds], factor, ordered = FALSE)
      ordereds = FALSE
    }
    data = data[!((args$remove.factors & factors) | (args$remove.ordered & ordereds))]
    data
  }

  trainfun = function(data, target, args) {
    tcol = data[target]
    data[target] = NULL
    args$levels = lapply(data, levels)
    data = colman(args, data)
    data = cbind(data, tcol)
    catf("wrapper %s train", prefix)
    dput(args[[paste0(prefix, ".spare1")]])
    dput(args[[paste0(prefix, ".spare2")]])
    for (pname in getParamIds(par.set)) {
      if (par.set$pars[[pname]]$type == "logical" && args[[pname]] == TRUE) {
        dput(pname)
      }
    }
    list(data = data, control = args)
  }

  predictfun = function(data, target, args, control) {
    catf("wrapper %s predict", prefix)
    dput(args[[paste0(prefix, ".spare1")]])
    dput(args[[paste0(prefix, ".spare2")]])
    colman(control, data)
  }
  x = makePreprocWrapper(learner, trainfun, predictfun, par.set, par.vals)
  addClasses(x, "PreprocWrapperAm")
}

debuglist = function(l, prefix = "") {
  res = ""
  if (testNamed(l)) {
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
      if (!is.null(names(l)[i]) && !is.na(names(l)[i]) && names(l)[i] != "") {
        res = paste0(res, paste0(prefix, names(l)[i], ": "))
      } else {
        res = paste0(res, paste0(prefix, i, ": "))
      }
      if (is.list(l[[i]])) {
        res = paste0(res, "\n", debuglist(l[[i]], paste0(prefix, "+")))
      } else {
        res = paste0(res, paste(l[[i]], collapse = ", "), "\n")
      }
    }
  }
  res
}

debuglistout = function(l) cat("###\n", debuglist(l), "###\n")

expectout = function(l) paste("###\n", debuglist(l), "###")


# test that the learner has params in 'trainps' during training and params in 'predps' during prediction
expect_learner_output = function(learner, task, name, trainps = list(), predps = list(), ...) {
  oldopts = getMlrOptions()
  configureMlr(show.learner.output = TRUE)
  wrapperArgs = list(...)
  wrapperoutput = function(which) capture.output(
      for (w in names(wrapperArgs)) {
        catf("wrapper %s %s", w, which)
        dput(wrapperArgs[[w]][[paste0(w, ".spare1")]])
        dput(wrapperArgs[[w]][[paste0(w, ".spare2")]])
        if (which == "train") {
          for (pname in c(".remove.factors", ".remove.ordered", ".remove.NA", ".convert.fac2num", ".convert.ord2fac", ".convert.ord2num")) {
            pcomplete = paste0(w, pname)
            if (isTRUE(wrapperArgs[[w]][[pcomplete]])) {
              dput(pcomplete)
            }
          }
        }
      }
      )
  learneroutput = expectout(c(list(myname = name), trainps))
  expect_output(model <- train(learner, task), paste(c(wrapperoutput("train"), learneroutput), collapse = "\n"), fixed = TRUE)
  predictoroutput = expectout(c(list(myname = name), predps))
  expect_output(predict(model, task), paste(c(wrapperoutput("predict"), predictoroutput), collapse = "\n"), fixed = TRUE)
  do.call(configureMlr, oldopts)
}


testLearner = function(name, parset, properties, isClassif = TRUE, ...) {  # isClassif==FALSE -> regr
  ret = (if (isClassif) makeRLearnerClassif else makeRLearnerRegr)(name, character(0), parset, properties = properties, ...)
  if (isClassif) {
    ret$fix.factors.prediction = TRUE
  }
  pf = globalenv()
  
  assign(paste0("trainLearner.", name), envir = pf, value = function (.learner, .task, .subset, .weights = NULL, ...) {
    debuglistout(list(myname = name, ...))
    list(data = getTaskData(.task, .subset), target = getTaskTargetNames(.task))
  })
  assign(paste0("predictLearner.", name), envir = pf, value = function (.learner, .model, .newdata, ...) {
    debuglistout(list(myname = name, ...))
    sample(.model$learner.model$data[[.model$learner.model$target]], nrow(.newdata), replace = TRUE)
  })
  ret
}

predefParams = list(
    int1 = makeIntegerLearnerParam("int1", 0, Inf, 0),
    int2 = makeIntegerLearnerParam("int2", 0, 10),
    int3 = makeIntegerLearnerParam("int3", default = 0, when = "predict"),
    int4 = makeIntegerLearnerParam("int4", when = "both", requires = quote(int1==0)),
    int5 = makeIntegerVectorLearnerParam("int5", len = 3, 0, 10, c(0, 1, 2)),
    int6 = makeIntegerVectorLearnerParam("int6", lower = 0, upper = 10),
    real1 = makeNumericLearnerParam("real1", 0, Inf, default = 0),
    real2 = makeNumericLearnerParam("real2", 0, 10),
    real3 = makeNumericLearnerParam("real3", default = 0, when = "predict"),
    real4 = makeNumericLearnerParam("real4", when = "both", requires = quote(real1==0)),
    real5 = makeNumericVectorLearnerParam("real5", len = 3, 0, 10, default = c(0, 1, 2)),
    real6 = makeNumericVectorLearnerParam("real6", lower = 0, upper = 10),
    cat1 = makeDiscreteLearnerParam("cat1", c("a", "b", "c")),
    cat2 = makeDiscreteLearnerParam("cat2", c("a", "b", "c", "d"), "c"),
    cat3 = makeDiscreteLearnerParam("cat3", c("a", "b", "c"), "a", when = "predict"),
    cat4 = makeDiscreteLearnerParam("cat4", c("a", "b", "c"), when = "both", requires = quote(cat1=="a")),
    cat5 = makeDiscreteVectorLearnerParam("cat5", len = 3, c("a", "b", "c"), default = list("a", "b", "a")),
    cat6 = makeDiscreteVectorLearnerParam("cat6", values = c("a", "b", "c")),
    bool1 = makeLogicalLearnerParam("bool1", TRUE),
    bool2 = makeLogicalLearnerParam("bool2"),
    bool3 = makeLogicalLearnerParam("bool3", default = FALSE, when = "predict"),
    bool4 = makeLogicalLearnerParam("bool4", when = "both", requires = quote(bool1==TRUE)),
    bool5 = makeLogicalVectorLearnerParam("bool5", len = 3, default = c(FALSE, TRUE, FALSE)),
    bool6 = makeLogicalVectorLearnerParam("bool6", default = FALSE))





optionalProps = c("numerics", "factors", "ordered", "missings", "twoclass", "multiclass")
allOPs = do.call(c, lapply(seq_along(optionalProps), function(i) combn(optionalProps, i, simplify = FALSE)))
names(allOPs) = as.character(seq_along(allOPs))
propertyLearners = mapply(testLearner, name = names(allOPs), properties = allOPs, MoreArgs = list(parset = makeParamSet()), SIMPLIFY = FALSE)
autolearnersBASIC = lapply(propertyLearners, autolearner)


defaultExpFun = function(mustBeHandled, mayBeHandled = character(0)) function(x) {  # yes we curry
  return(all(mustBeHandled %in% x) &&  # all things that must be handled are present
           any(intersect(union(mustBeHandled, mayBeHandled), c("numerics", "factors", "ordered")) %in% x))  # at least one type of column can be processed
}

defaultExpFun2 = function(mustBeHandledList) function(x) {  # yes we curry
  possible = sapply(mustBeHandledList, function(mustBeHandled) all(mustBeHandled %in% x) &&  # all things that must be handled are present
           any(intersect(mustBeHandled, c("numerics", "factors", "ordered")) %in% x))
  return(any(possible))  # at least one type of column can be processed
}


checkLearnersPresent = function(autolearnersPL, task, propertiesExpected, optionalProperties = character(0),
    expfun = defaultExpFun2(propertiesExpected), debugOut = FALSE) {
  expectedLearners = names(allOPs)[sapply(allOPs, expfun)]
  presentLearners = unlist(getParamSet(buildLearners(autolearnersPL, task))$pars$selected.learner$values)
  if (debugOut) {
    catf("Expected Learners: %s", paste(expectedLearners, collapse = ", "))
    catf("Present Learners: %s", paste(presentLearners, collapse = ", "))
  }
  expect_set_equal(presentLearners, expectedLearners)
}

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


checkWrapperEffectEx = function(autolearnersPL, transformation = list, debugOut = FALSE, ...) {
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
          checkLearnersPresent(autolearnersPL, testTask, transformation(totalprops), debugOut = debugOut, ...)
        }
      }
    }
  }
}


bl = function(...) {
  buildLearners(list(...), pid.task)
}


isFeasibleNoneMissing = function(par, x) {
  nalist = rep(NA, length(par$pars))
  names(nalist) = getParamIds(par)
  isFeasible(par, insert(nalist, x))
}

getpars = function(learner) getParamSet(learner)$pars

checkLearnerBehaviour = function(learner, task, params, ...) {
  expect_true(isFeasibleNoneMissing(getParamSet(learner), params))
#predict(train(setHyperPars(learner, par.vals = params), task), task)
  expect_learner_output(setHyperPars(learner, par.vals = params), task, ...)
}


