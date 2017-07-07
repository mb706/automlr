
# interesting parameters to use.
# there are 6 of each integer, real, factorial, boolean
# the system is:
# finite, infinite, when=predict, requirement, vector dim=3, vector dim=1
predefParams = list(
    int1 = makeIntegerLearnerParam("int1", 0, Inf, 0),
    int2 = makeIntegerLearnerParam("int2", 0, 10),
    int3 = makeIntegerLearnerParam("int3", default = 0, when = "predict"),
    int4 = makeIntegerLearnerParam("int4", when = "both",
        requires = quote(int1==0)),
    int5 = makeIntegerVectorLearnerParam("int5", len = 3, 0, 10, c(0, 1, 2)),
    int6 = makeIntegerVectorLearnerParam("int6", lower = 0, upper = 10),

    real1 = makeNumericLearnerParam("real1", 0, Inf, default = 0),
    real2 = makeNumericLearnerParam("real2", 0, 10),
    real3 = makeNumericLearnerParam("real3", default = 0, when = "predict"),
    real4 = makeNumericLearnerParam("real4", when = "both",
        requires = quote(real1==0)),
    real5 = makeNumericVectorLearnerParam("real5", len = 3, 0, 10,
        default = c(0, 1, 2)),
    real6 = makeNumericVectorLearnerParam("real6", lower = 0, upper = 10),

    cat1 = makeDiscreteLearnerParam("cat1", c("a", "b", "c")),
    cat2 = makeDiscreteLearnerParam("cat2", c("a", "b", "c", "d"), "c"),
    cat3 = makeDiscreteLearnerParam("cat3", c("a", "b", "c"), "a",
        when = "predict"),
    cat4 = makeDiscreteLearnerParam("cat4", c("a", "b", "c"), when = "both",
        requires = quote(cat1=="a")),
    cat5 = makeDiscreteVectorLearnerParam("cat5", len = 3, c("a", "b", "c"),
        default = list("a", "b", "a")),
    cat6 = makeDiscreteVectorLearnerParam("cat6", values = c("a", "b", "c")),

    bool1 = makeLogicalLearnerParam("bool1", TRUE),
    bool2 = makeLogicalLearnerParam("bool2"),
    bool3 = makeLogicalLearnerParam("bool3", default = FALSE, when = "predict"),
    bool4 = makeLogicalLearnerParam("bool4", when = "both",
        requires = quote(bool1==TRUE)),
    bool5 = makeLogicalVectorLearnerParam("bool5", len = 3,
        default = c(FALSE, TRUE, FALSE)),
    bool6 = makeLogicalVectorLearnerParam("bool6", default = FALSE))


# define tasks and learnerswfor factors, numerics, ordereds, numerics+factors; each with and without missings
# also make numerics+ordered task with and without missings, and make an additional learner for all things
NumericsTask = createTestClassifTask("NumericsTask", 200, nNumeric = 3)
FactorsTask = createTestClassifTask("FactorsTask", 200, nFactor = 3)
OrderedTask = createTestClassifTask("OrderedTask", 200, nOrdered = 3)
NumericsFactorsTask = createTestClassifTask("NumericsFactorsTask", 200, nNumeric = 3, nFactor = 3)
NumericsOrderedTask = createTestClassifTask("NumericsOrderedTask", 200, nNumeric = 3, nOrdered = 3)
MissingsNumericsTask = createTestClassifTask("MissingsNumericsTask", 200, nNumeric = 3, missings = TRUE)
MissingsFactorsTask = createTestClassifTask("MissingsFactorsTask", 200, nFactor = 3, missings = TRUE)
MissingsOrderedTask = createTestClassifTask("MissingsOrderedTask", 200, nOrdered = 3, missings = TRUE)
MissingsNumericsFactorsTask = createTestClassifTask("MissingsNumericsFactorsTask", 200, nNumeric = 3, nFactor = 3, missings = TRUE)
MissingsNumericsOrderedTask = createTestClassifTask("MissingsNumericsOrderedTask", 200, nNumeric = 3, nOrdered = 3, missings = TRUE)

NumericsLearner = autolearner(
    testLearner("NumericsLearner", makeParamSet(predefParams$int1), c("numerics", "twoclass")),
    list(sp("int1", "int", c(0, 10))))

FactorsLearner = autolearner(
    testLearner("FactorsLearner", makeParamSet(predefParams$int1), c("factors", "twoclass")),
    list())

OrderedsLearner = autolearner(
    testLearner("OrderedsLearner", makeParamSet(predefParams$int1), c("ordered", "twoclass")),
    list())

MissingsNumericsLearner = autolearner(
    testLearner("MissingsNumericsLearner", makeParamSet(predefParams$int1), c("numerics", "twoclass", "missings")),
    list(sp("int1", "int", c(0, 10), req = quote(automlr.has.missings==TRUE))))

MissingsFactorsLearner = autolearner(
    testLearner("MissingsFactorsLearner", makeParamSet(predefParams$int1), c("factors", "twoclass", "missings")),
    list())

MissingsFactorsNumericsLearner = autolearner(
    testLearner("MissingsFactorsNumericsLearner", makeParamSet(predefParams$int1, predefParams$real1, predefParams$bool1),
                c("numerics", "twoclass", "missings", "factors")),
    list(sp("int1", "int", c(0, 10), req = quote(automlr.has.missings==TRUE)),
         sp("real1", "real", c(10, 10), req = quote(automlr.has.factors==TRUE)),
         sp("bool1", "bool", req = quote(automlr.has.missings != automlr.has.factors))))

FactorsNumericsLearner = autolearner(
    testLearner("FactorsNumericsLearner", makeParamSet(predefParams$real1, predefParams$bool1), c("numerics", "twoclass", "factors")),
    list(sp("real1", "real", c(0, 10), req = quote(automlr.has.factors==FALSE)),
         sp("bool1", "cat", FALSE, req = quote(automlr.has.missings == automlr.has.factors)),
         sp("bool1.AMLRFIX1", "cat", TRUE, req = quote(automlr.has.missings != automlr.has.factors))))

AllLearner = autolearner(
    testLearner("AllLearner", makeParamSet(predefParams$int1, predefParams$real1, predefParams$bool1), c("numerics", "twoclass", "factors", "ordered", "missings")),
    list(sp("int1", "int", c(0, 10), req = quote(automlr.has.missings==FALSE)),
         sp("real1", "real", c(0, 10), req = quote(automlr.has.factors %in% c(TRUE, FALSE))),
         sp("bool1", "fix", TRUE, req = quote(automlr.has.ordered == automlr.has.factors)),
         sp("int1.AMLRFIX1", "int", c(2, 2), req = quote(automlr.has.missings==TRUE && automlr.has.factors == TRUE)),
         sp("int1.AMLRFIX2", "int", c(11, 20), req = quote(automlr.has.missings==TRUE && automlr.has.factors == FALSE))))


fremover = makeCPO("fremover", .properties.adding = "factors", .stateless = TRUE, .datasplit = "onlyfactor",
  cpo.trafo = function(data, target) { data[character(0)] }, cpo.retrafo = function(data) { data[character(0)] })

asnumcpo = makeCPO("as.numeric", .properties.adding = c("factors", "ordered"), .properties.needed = "numerics",
  .stateless = TRUE, .datasplit = "factor", cpo.trafo = function(data, target) {
    as.data.frame(lapply(data, as.numeric), row.names = rownames(data)) }, cpo.retrafo = function(data) {
      as.data.frame(lapply(data, as.numeric), row.names = rownames(data)) })
numasordcpo = makeCPO("num.as.ordered", .properties.adding = "numerics", .properties.needed = "ordered",
  .stateless = TRUE, .datasplit = "numeric", cpo.trafo = function(data, target) {
    as.data.frame(lapply(data, as.ordered), row.names = rownames(data)) }, cpo.retrafo = function(data) {
      as.data.frame(lapply(data, as.ordered), row.names = rownames(data)) })
facasordcpo = makeCPO("fac.as.ordered", .properties.adding = "factors", .properties.needed = "ordered",
  .stateless = TRUE, .datasplit = "onlyfactor", cpo.trafo = function(data, target) {
    as.data.frame(lapply(data, as.ordered), row.names = rownames(data)) }, cpo.retrafo = function(data) {
      as.data.frame(lapply(data, as.ordered), row.names = rownames(data)) })
ordasfaccpo = makeCPO("ord.as.factor", .properties.adding = "ordered", .properties.needed = "factors",
  .stateless = TRUE, .datasplit = "ordered", cpo.trafo = function(data, target) {
    as.data.frame(lapply(data, factor, ordered = FALSE), row.names = rownames(data)) }, cpo.retrafo = function(data) {
      as.data.frame(lapply(data, factor, ordered = FALSE), row.names = rownames(data)) })
reversefacorder = makeCPO("reverse", .stateless = TRUE, .datasplit = "factor",
  cpo.trafo = function(data, target) {
    as.data.frame(lapply(data, function(x) factor(x, levels = rev(levels(x)))),
      row.names = rownames(data))
  }, cpo.retrafo = function(data) {
    as.data.frame(lapply(data, function(x) factor(x, levels = rev(levels(x)))),
      row.names = rownames(data))
  })
splitnumcpo = makeCPO("as.factor", numsplits: integer[2, 5],
  .properties.adding = "numerics", .properties.needed = "factors",
  .datasplit = "numeric", cpo.trafo = {
    breaks = lapply(data, function(d)
      unique(c(-Inf, quantile(d, (1:(numsplits - 1)) / numsplits), Inf)))
    cpo.retrafo = function(data) {
      as.data.frame(mapply(function(d, b) cut(d, breaks = b), d = data, b = breaks, SIMPLIFY = FALSE),
        row.names = rownames(data))
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)

# there are three types of wrappers: converters, imputers, preprocessors.

# imputers
nimp1 = autolearner(
    autoWrapper("numimputer1", cpoImputeMean(make.dummy.cols = FALSE) %>>% fremover(),
      "numerics", "missings"), stacktype = "wrapper")
nimp2 = autolearner(
    autoWrapper("numimputer2", cpoImputeMin(make.dummy.cols = FALSE) %>>% fremover(),
      "numerics", "missings"),
    list(sp("multiplier", "real", c(0, 2))), stacktype = "wrapper")
fimp1 = autolearner(
    autoWrapper("factimputer1", cpoImputeConstant(make.dummy.cols = FALSE, id = "fimp"),
      "factors", "missings"),
    list(sp("fimp.const", "cat", c("NAx", "MISSING"))), stacktype = "wrapper")
fimp2 = autolearner(
    autoWrapper("factimputer2", cpoImputeHist(make.dummy.cols = FALSE),
      "factors", "missings"), stacktype = "wrapper")
oimp1 = autolearner(
    autoWrapper("ordimputer1", cpoImputeConstant(make.dummy.cols = FALSE, id = "oimp") %>>% fremover(),
      "ordered", "missings"),
    list(sp("oimp.const", "cat", c("NAx", "MISSING"))), stacktype = "wrapper")
oimp2 = autolearner(
    autoWrapper("ordimputer2", cpoImputeHist(make.dummy.cols = FALSE) %>>% fremover(),
      "ordered", "missings"), stacktype = "wrapper")

# converters
fnconv1 = autolearner(
    autoWrapper("fnconv1", cpoDummyEncode(),
      "numerics", "factors"), list(sp("reference.cat", "bool")), "wrapper")
fnconv2 = autolearner(
    autoWrapper("fnconv2", asnumcpo(),
      "numerics", "factors"), stacktype = "wrapper")
onconv1 = autolearner(
    autoWrapper("onconv1", cpoDummyEncode(id = "onconv"),
      "numerics", "ordered"), list(sp("onconv.reference.cat", "bool")), "wrapper")
onconv2 = autolearner(
    autoWrapper("onconv2", asnumcpo(),
      "numerics", "ordered"), stacktype = "wrapper")
nfconv1 = autolearner(
    autoWrapper("nfconv1", splitnumcpo(),
      "factors", "numerics"),
    list(sp("numsplits", "int", c(2, 5))), "wrapper")
nfconv2 = autolearner(
    autoWrapper("nfconv2", splitnumcpo(id = "conv2"),
      "factors", "numerics"),
    list(sp("conv2.numsplits", "int", c(2, 5))), "wrapper")
ofconv1 = autolearner(
    autoWrapper("ofconv1", ordasfaccpo(),
      "factors", "ordered"), stacktype = "wrapper")
ofconv2 = autolearner(
    autoWrapper("ofconv2", asnumcpo() %>>% splitnumcpo(id = "ofconv"),
      "factors", "ordered"), list(sp("ofconv.numsplits", "int", c(2, 5))),"wrapper")
noconv1 = autolearner(
    autoWrapper("noconv1", splitnumcpo(id = "noconv") %>>% facasordcpo(), "ordered", "numerics"),
    list(sp("noconv.numsplits", "int", c(2, 5))), "wrapper")
noconv2 = autolearner(
    autoWrapper("noconv2", numasordcpo(), "ordered", "numerics"),
    stacktype = "wrapper")
foconv1 = autolearner(
    autoWrapper("foconv1", facasordcpo(), "ordered", "factors"),
    stacktype = "wrapper")
foconv2 = autolearner(
    autoWrapper("foconv2", facasordcpo() %>>% reversefacorder(), "ordered", "factors"),
    stacktype = "wrapper")

