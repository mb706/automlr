


context("searchspace")

test_that("each searchspace item works", {

  done = c("classif.penalized", "classif.glmnet",
    "classif.cvglmnet", "classif.binomial",
    "classif.lqa",
    "classif.plr",
    "classif.lda",
    "classif.qda",
    "classif.linDA",
    "classif.sparseLDA",
    "classif.rrlda",
    "classif.rda",
    "classif.sda",
    "classif.plsdaCaret",
    "classif.mda",
    "classif.quaDA",
    "classif.geoDA",
    "classif.kknn",
    "classif.knn",
    "classif.rknn",
    "classif.fnn",
    "classif.IBk",
    "classif.ctree",
    "classif.J48",
    "classif.PART",
    "classif.nodeHarvest",
    "classif.rpart",
    "classif.bartMachine",
    "classif.cforest",
    "classif.randomForest",
    "classif.RRF",
    "classif.extraTrees",
    "classif.randomForestSRC",
    "classif.ranger",
    "classif.rFerns",
    "classif.rotationForest",
    "classif.blackboost",
    "classif.boosting",
    "classif.bst",
    "classif.C50",
    "classif.gbm",
    "classif.glmboost",
    "classif.gamboost",
    "classif.dcSVM",
    "classif.clusterSVM",
    "classif.gaterSVM",
    "classif.ksvm",
    "classif.evtree",
    "classif.ada",
    "classif.xgboost",
    "classif.lssvm",
    "classif.svm",
    "classif.LiblineaRL1L2SVC",
    "classif.LiblineaRL2L1SVC",
    "classif.LiblineaRL2SVC",
    "classif.LiblineaRMultiClassSVC",
    "classif.LiblineaRL1LogReg",

    NULL)
  for (lrn in mlrLearnersNoWrap) {
    print(lrn$learner)
    if (lrn$learner %in% done) {
      print("skipping")
      next
    }
    set.seed(123)
    try(utils::capture.output(res <- automlr(pid.task,
      budget = c(evals = 1), verbosity=3,
      searchspace = list(lrn), backend = "random")))
  }


  # classif.binomial?

  # classif.lqa: 'gamma' infeasible bounds 1, 10????
  # linDA: 'no parameters were passed'
  # rrlda:
# unable to load shared object #'/home/mb706/lmu/master/library/robustbase/libs/robustbase.so':
#  libopenblas.so.0: cannot open shared object file: No such file or directory
  #
  # quaDA, geoDA: "no parameters were passed"
  # lvq1: no params passed
  # naiveBayes: no params passed


  # neuralnet fails

})
