

## Test wrappers are used in the way expected of them.
 ## - [ ] wrappers
 ##   - [ ] no wrappers given, or only one required wrapper given: no wrapper selector; otherwise there is a selector
 ##   - [ ] one required wrapper: is always used
 ##   - [ ] one nonrequired wrapper: is only used some of the time
 ##   - [ ] two required wrappers: order can be selected; also changes in reallife
 ##   - [ ] two optional wrappers: selector, which one is used and the order match
 ##   - [ ] two optional, two requireds: selector, order and which one is used match


w1 = autolearner(
    autoWrapper("w1", function(learner, ...) changeColsWrapper(learner, "w1", ...), identity),
    list(sp("w1.spare1", "int", c(0, 10))),
    "wrapper")
w1r = autolearner(
    autoWrapper("w1r", function(learner, ...) changeColsWrapper(learner, "w1r", ...), identity),
    list(sp("w1r.spare1", "int", c(0, 10), special='dummy')),
    "requiredwrapper")
w2 = autolearner(
    autoWrapper("w2", function(learner, ...) changeColsWrapper(learner, "w2", ...), identity),
    list(sp("w2.spare1", "int", c(0, 10)),
         sp("w2.spare2", "int", c(0, 10), req=quote(w2.spare1==1))),
    "wrapper")
w2r = autolearner(
    autoWrapper("w2r", function(learner, ...) changeColsWrapper(learner, "w2r", ...), identity),
    list(sp("w2r.spare1", "fix", 0, special='dummy'),
         sp("w2r.spare2", "int", c(0, 10), req=quote(w2r.spare1==1))),
    "requiredwrapper")
test1 = autolearner(
    testLearner("test1", makeParamSet(predefParams$int1), c("numerics", "twoclass")),
    list(sp("int1", "int", c(0, 10))))
test2 = autolearner(
    testLearner("test2", makeParamSet(predefParams$int1), c("numerics", "twoclass")),
    list(sp("int1", "int", c(0, 10))))

expect_set_equal(getParamIds(getParamSet(bl(test1, test2))), c("selected.learner", "test1.int1", "test2.int1"))
expect_set_equal(getParamIds(getParamSet(bl(w1r, test1, test2))), c("selected.learner", "test1.int1", "test2.int1", "w1r.spare1"))
expect_set_equal(getParamIds(getParamSet(bl(w1r, test1, test2))), c("selected.learner", "test1.int1", "test2.int1", "w1r.spare1"))
expect_set_equal(getParamIds(getParamSet(bl(w1, test1, test2))), c("selected.learner", "test1.int1", "test2.int1", "w1.spare1", "automlr.wrappersetup"))
expect_set_equal(getParamIds(getParamSet(bl(w1, w1r, test1, test2))),
                 c("selected.learner", "test1.int1", "test2.int1", "w1r.spare1", "w1.spare1", "automlr.wrappersetup"))

lw1w1r = bl(w1, w1r, test1, test2)
lw1w1rPS = getParamSet(lw1w1r)
expect_set_equal(getParamIds(lw1w1rPS),  c("selected.learner", "test1.int1", "test2.int1", "w1.spare1", "w1r.spare1", "automlr.wrappersetup"))
expect_set_equal(as.character(lw1w1rPS$pars$automlr.wrappersetup$values), c("w1r", "w1$w1r", "w1r$w1"))

expect_true(isFeasible(lw1w1rPS, list(test1.int1=1, w1r.spare1=4, selected.learner="test1", automlr.wrappersetup="w1r")))
expect_true(isFeasible(lw1w1rPS, list(test1.int1=1, w1.spare1=3, w1r.spare1=4, selected.learner="test1", automlr.wrappersetup="w1$w1r")))
lx = setHyperPars(lw1w1r, test1.int1=1, selected.learner="test1", w1r.spare1=4, automlr.wrappersetup="w1r")
m <- train(lx, pid.task)
invisible(predict(m, pid.task))


lx = setHyperPars(lw1w1r, test1.int1=1, selected.learner="test1", w1.spare1=3, w1r.spare1=4, automlr.wrappersetup="w1$w1r")
train(lx, pid.task)




