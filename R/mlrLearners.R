
#' @include autolearner.R wrappers.R

#' @title
#' A list of learners with corresponding \code{par.set}s that can be searched over.
#' 
#' @description
#' This is a list of learners that the infinitely wise developer of this package
#' collected \emph{himself} that work well with automlr.
#' @name autolearners
#' @docType data
#' @export
autolearners = makeNamedAlList(
##### automatically generated:
    autolearner("classif.glmnet",
        list(
# ** vp
            sp("alpha", "real", c(0, 1)),
            sp("s", "real", c(0.001, 1), "exp"),
# ** cp
            sp("lambda.min.ratio", "fix", .0001),
            sp("nlambda", "fix", 200),
            sp("mnlam", "fix", 200),
            sp("standardize", "fix", FALSE),
# ** dp
            sp("lambda", "def", NULL),
            sp("exact", "def", FALSE),
            sp("intercept", "def", TRUE),
            sp("thresh", "def", 1e-7),
            sp("dfmax", "def", NULL),
            sp("pmax", "def", NULL),
            sp("exclude", "def", NULL),
            sp("penalty.factor", "def", NULL),
            sp("lower.limits", "def", NULL),
            sp("upper.limits", "def", NULL),
            sp("maxit", "def", 1e5),
            sp("type.logistic", "def", NULL),
            sp("type.multinomial", "def", NULL),
            sp("fdev", "def", 1e-5),
            sp("devmax", "def", .999),
            sp("eps", "def", 1e-6),
            sp("big", "def", 9.9e35),
            sp("pmin", "def", 1e-9),
            sp("exmx", "def", 250),
            sp("prec", "def", 1e-10),
            sp("mxit", "def", 100),
            sp("factory", "def", FALSE))),
    autolearner("classif.binomial",
        list(
# ** vp
            sp("link", "cat", c("logit", "probit", "cauchit", "log", "cloglog")),
# ** dp
            sp("model", "def", TRUE))),
    autolearner("classif.lqa",
        list(
# ** vp
            sp("penalty", "cat", c("adaptive.lasso", "ao", "bridge", "enet", "fused.lasso", "genet", "icb", "lasso", "licb", "oscar", "penalreg", "ridge", "scad", "weighted.fusion")),
            sp("lambda", "real", c(.001, 100), "exp", req = quote(penalty %in% c("adaptive.lasso", "ao", "bridge", "genet", "lasso", "oscar", "penalreg", "ridge", "scad"))),
            sp("gamma", "real", c(1.0001, 100), "exp", req = quote(penalty %in% c("ao", "bridge", "genet", "weighted.fusion"))),
            sp("alpha", "real", c(.001, 100), "exp", req = quote(penalty == "genet")),
            sp("c", "real", c(.001, 100), "exp", req = quote(penalty == "oscar")),
            sp("a", "real", c(2, 100), "exp", req = quote(penalty == "scad")),
            sp("lambda1", "real", c(.001, 100), "exp", req = quote(penalty %in% c("enet", "fused.lasso", "icb", "licb", "weighted.fusion"))),
            sp("lambda2", "real", c(.001, 100), "exp", req = quote(penalty %in% c("enet", "fused.lasso", "icb", "licb", "weighted.fusion"))),
            sp("method", "cat", c("lqa.update2", "ForwardBoost", "GBlockBoost")),
# ** dp
            sp("var.eps", "def", .Machine$double.eps),
            sp("max.steps", "def", 5000),
            sp("conv.eps", "def", .001),
            sp("conv.stop", "def", TRUE),
            sp("c1", "def", 1e-8),
            sp("digits", "def", 5))),
    autolearner("classif.logreg"),
    autolearner("classif.probit"),
    autolearner("classif.plr",
        list(
# ** vp
            sp("lambda", "real", c(1e-5, 100), "exp"),
            sp("cp.type", "cat", c("bic", "aic")),
# ** dp
            sp("cp", "def", 2))),
    autolearner("classif.lda",
        list(
# ** vp
            sp("method", "cat", c("moment", "mle", "mve", "t"), id = "da.method"),
            sp("nu", "real", c(2, 64), "exp", id = "da.nu", req = quote(method=='t')),
            sp("predict.method", "cat", c("plug-in", "predictive", "debiased"), id = "da.pm"),
# ** dp
            sp("tol", "def", .0001),
            sp("CV", "def", FALSE))),
    autolearner("classif.qda",
        list(
# ** vp
            sp("method", "cat", c("moment", "mle", "mve", "t"), id = "da.method"),
            sp("nu", "real", c(2, 64), "exp", id = "da.nu", req = quote(method=='t')),
            sp("predict.method", "cat", c("plug-in", "predictive", "debiased"), id = "da.pm"))),
    autolearner("classif.linDA",
        list(
# ** dp
            sp("validation", "def", NULL))),
    autolearner("classif.sparseLDA",
        list(
# ** vp
            sp("lambda", "real", c(1e-10, 1), "exp"),
            sp("maxIte", "int", c(50, 400), "exp"),
# ** dp
            sp("trace", "def", FALSE),
            sp("tol", "def", 1e-6))),
    autolearner("classif.rrlda",
        list(
# ** vp
            sp("lambda", "real", c(0.01, 10)),
            sp("hp", "real", c(0.3, 1)),
            sp("nssamples", "int", c(10, 1000), "exp"),
            sp("maxit", "int", c(50, 400), "exp"),
            sp("penalty", "cat", c("L1", "L2")),
# ** dp
            sp("prior", "def", NULL))),
    autolearner("classif.rda",
        list(
# ** vp
            sp("kernel", "cat", c("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian")),
            sp("crossval", "bool", req = quote(fold > 1)),
            sp("crossval.AMLRFIX1", "cat", c("FALSE"), req = quote(fold == 1)),
            sp("train.fraction", "real", c(0.1, 0.9)),
            sp("fold", "int", c(1, 32), "exp"),
            sp("K", "int", c(30, 3000), "exp", req = quote(SimAnn == TRUE && schedule == 1)),
            sp("alpha", "real", c(1, 4), req = quote(SimAnn == TRUE && schedule == 2)),
            sp("zero.temp", "real", c(.001, 0.1), "exp", req = quote(SimAnn == TRUE && schedule == 1)),
            sp("halflife", "real", c(5, 1000), "exp", req = quote(SimAnn == TRUE && schedule == 1)),
            sp("T.start", "real", c(.01, 100), "exp", req = quote(SimAnn == TRUE)),
            sp("schedule", "cat", c(1, 2), req = quote(SimAnn == FALSE)),
            sp("trafo", "bool"),
            sp("SimAnn", "bool"),
# ** cp
            sp("estimate.error", "fix", FALSE),
# ** dp
            sp("lambda", "def", NULL),
            sp("gamma", "def", NULL),
            sp("output", "def", FALSE))),
    autolearner("classif.sda",
        list(
# ** vp
            sp("lambda", "real", c(0, 1)),
            sp("lambda.var", "real", c(0, 1)),
            sp("lambda.freqs", "real", c(0, 1)),
            sp("diagonal", "bool"),
# ** dp
            sp("verbose", "def", TRUE))),
    autolearner("classif.plsdaCaret",
        list(
# ** vp
            sp("ncomp", "int", c(2, 64), "exp"),
            sp("probMethod", "cat", c("softmax", "Bayes")))),
    autolearner("classif.mda",
        list(
# ** vp
            sp("subclasses", "int", c(1, 32), "exp"),
            sp("sub.df", "int", c(1, 32), "exp"),
            sp("method", "cat", c("polyreg", "mars", "bruto", "gen.ridge")),
            sp("start.method", "cat", c("kmeans", "lvq")),
            sp("tries", "int", c(5, 20)),
            sp("criterion", "cat", c("misclassification", "deviance")),
# ** cp
            sp("keep.fitted", "fix", FALSE),
# ** dp
            sp("tot.df", "def", NULL),
            sp("dimension", "def", NULL),
            sp("eps", "def", .Machine$double.eps),
            sp("iter", "def", 5),
            sp("trace", "def", FALSE))),
    autolearner("classif.hdrda",
        list(
# ** vp
            sp("lambda", "real", c(0, 1)),
            sp("gamma", "real", c(0.001, 0.3), "exp"),
            sp("shrinkage_type", "cat", c("ridge", "convex")),
# ** dp
            sp("prior", "def", NULL),
            sp("tol", "def", 1e-6),
            sp("projected", "def", FALSE))),
    autolearner("classif.quaDA",
        list(
# ** vp
            sp("do.validation", "bool", id = "da.val", special = "dummy"),
            sp("validation", "cat", c("crossval"), req = quote(do.validation == TRUE)))),
    autolearner("classif.geoDA",
        list(
# ** vp
            sp("do.validation", "bool", id = "da.val", special = "dummy"),
            sp("validation", "cat", c("crossval"), req = quote(do.validation == TRUE)))),
    autolearner("classif.kknn",
        list(
# ** vp
            sp("k", "int", c(1, 100), "exp", id = "knn.k"),
            sp("distance", "real", c(0.5, 4), "exp"),
            sp("kernel", "cat", c("triangular", "rectangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian")),
# ** cp
            sp("scale", "fix", FALSE))),
    autolearner("classif.knn",
        list(
# ** vp
            sp("k", "int", c(1, 100), "exp", id = "knn.k"),
# ** dp
            sp("l", "def", 0),
            sp("prob", "def", FALSE),
            sp("use.all", "def", TRUE))),
    autolearner("classif.rknn",
        list(
# ** vp
            sp("k", "int", c(1, 100), "exp"),
            sp("r", "int", c(100, 4000), "exp"),
            sp("mtry", "int", c(3, 40)),
# ** dp
            sp("seed", "def", NULL),
            sp("cluster", "def", NULL))),
    autolearner("classif.fnn",
        list(
# ** vp
            sp("k", "int", c(1, 100), "exp", id = "knn.k"),
            sp("algorithm", "cat", c("cover_tree", "kd_tree", "brute")),
# ** dp
            sp("prob", "def", FALSE))),
    autolearner("classif.IBk",
        list(
# ** vp
            sp("I", "bool"),
            sp("F", "bool", req = quote(I == FALSE)),
            sp("K", "int", c(1, 100), "exp", id = "knn.k"),
# ** dp
            sp("X", "def", NULL),
            sp("E", "def", NULL),
            sp("A", "def", "weka.core.neighboursearch.LinearNNSearch"),
            sp("W", "def", NULL),
            sp("output-debug-info", "def", FALSE))),
    autolearner("classif.ctree",
        list(
# ** vp
            sp("teststat", "cat", c("quad", "max")),
            sp("testtype", "cat", c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
            sp("mincriterion", "real", c(0.5, 0.99), req = quote(testtype != "Teststatistic")),
            sp("mincriterion.AMLRFIX1", "real", c(0.1, 1), req = quote(testtype == "Teststatistic")),
            sp("maxsurrogate", "int", c(0, 5)),
            sp("limitmtry", "bool", special = "dummy"),
            sp("mtry", "int", c(3, 20), req = quote(limitmtry == TRUE)),
            sp("minbucket", "int", c(1, 32), "exp", id = "tree.m"),
            sp("minsplit", "int", c(2, 64), "exp"),
            sp("stump", "bool"),
# ** cp
            sp("savesplitstats", "fix", FALSE),
# ** dp
            sp("nresample", "def", 9999),
            sp("maxdepth", "def", 0))),
    autolearner("classif.J48",
        list(
# ** vp
            sp("U", "bool", id = "tree.u"),
            sp("O", "bool"),
            sp("C", "real", c(0.1, .9), id = "tree.c", req = quote(U == FALSE && R == FALSE)),
            sp("M", "int", c(1, 32), "exp", id = "tree.m"),
            sp("R", "bool", id = "tree.r", req = quote(U == FALSE)),
            sp("N", "int", c(2, 8), id = "tree.n", req = quote(U == FALSE && R == TRUE)),
            sp("B", "bool", id = "tree.b"),
            sp("S", "bool", req = quote(U == FALSE)),
            sp("J", "bool", id = "tree.j"),
# ** cp
            sp("L", "fix", FALSE),
            sp("A", "fix", FALSE),
# ** dp
            sp("Q", "def", NULL),
            sp("output-debug-info", "def", FALSE))),
    autolearner("classif.PART",
        list(
# ** vp
            sp("C", "real", c(0.1, 0.9), id = "tree.c", req = quote(R == FALSE)),
            sp("M", "int", c(1, 32), "exp", id = "tree.m"),
            sp("R", "bool", id = "tree.r"),
            sp("N", "int", c(2, 8), id = "tree.n", req = quote(R == TRUE)),
            sp("B", "bool", id = "tree.b"),
            sp("U", "bool", id = "tree.u"),
            sp("J", "bool", id = "tree.j"),
# ** dp
            sp("Q", "def", NULL),
            sp("output-debug-info", "def", FALSE))),
    autolearner("classif.nodeHarvest",
        list(
# ** vp
            sp("nodesize", "int", c(1, 32), "exp"),
            sp("nodes", "int", c(100, 10000), "exp"),
            sp("maxinter", "int", c(1, 3)),
            sp("mode", "cat", c("mean", "outbag")),
            sp("biascorr", "bool"),
# ** cp
            sp("silent", "fix", TRUE),
# ** dp
            sp("onlyinter", "def", NULL),
            sp("addto", "def", NULL),
            sp("lambda", "def", NULL))),
    autolearner("classif.rpart",
        list(
# ** vp
            sp("minsplit", "int", c(2, 64), "exp"),
            sp("minbucket", "int", c(1, 32), "exp", id = "tree.m"),
            sp("cp", "real", c(1e-4, 0.5), "exp"),
            sp("usesurrogate", "cat", c(0, 1, 2)),
            sp("surrogatestyle", "cat", c(0, 1)),
            sp("maxdepth", "int", c(1, 30), "exp"),
# ** cp
            sp("xval", "fix", 0),
# ** dp
            sp("maxcompete", "def", 4),
            sp("maxsurrogate", "def", 5),
            sp("parms", "def", NULL))),
    autolearner("classif.bartMachine",
        list(
# ** vp
            sp("num_trees", "int", c(25, 200), "exp"),
            sp("alpha", "real", c(0, 1)),
            sp("beta", "real", c(1, 3)),
            sp("k", "real", c(1, 4)),
            sp("mh_prob_steps", "real", c(0.00000001, 1), dim = 3),
# ** cp
            sp("run_in_sample", "fix", FALSE),
            sp("verbose", "fix", FALSE),
# ** dp
            sp("num_burn_in", "def", 250),
            sp("num_iterations_after_burn_in", "def", 1000),
            sp("q", "def", 0.9),
            sp("prob_rule_class", "def", 0.5),
            sp("debug_log", "def", FALSE),
            sp("cov_prior_vec", "def", NULL),
            sp("use_missing_data", "def", TRUE),
            sp("use_missing_data_dummies_as_covars", "def", FALSE),
            sp("replace_missing_data_with_x_j_bar", "def", FALSE),
            sp("impute_missingness_with_rf_impute", "def", FALSE),
            sp("impute_missingness_with_x_j_bar_for_lm", "def", TRUE),
            sp("num_rand_samps_in_library", "def", 10000),
            sp("mem_cache_for_speed", "def", TRUE),
            sp("serialize", "def", FALSE),
            sp("seed", "def", NULL))),
    autolearner("classif.randomForest",
        list(
# ** vp
            sp("ntree", "int", c(100, 4000), "exp", id = "rf.numtree"),
            sp("mtry", "int", c(3, 40), id = "rf.mtry"),
            sp("replace", "bool", id = "rf.replace"),
            sp("nodesize", "int", c(1, 32), "exp", id = "rf.nodesize"),
# ** dp
            sp("classwt", "def", NULL),
            sp("cutoff", "def", NULL),
            sp("sampsize", "def", NULL),
            sp("maxnodes", "def", NULL),
            sp("importance", "def", FALSE),
            sp("localImp", "def", FALSE),
            sp("norm.votes", "def", TRUE),
            sp("do.trace", "def", FALSE),
            sp("keep.inbag", "def", FALSE))),
    autolearner("classif.extraTrees",
        list(
# ** vp
            sp("ntree", "int", c(100, 4000), "exp", id = "rf.numtree"),
            sp("mtry", "int", c(3, 40), id = "rf.mtry"),
            sp("nodesize", "int", c(1, 32), "exp", id = "rf.nodesize"),
            sp("numRandomCuts", "int", c(1, 16), "exp"),
            sp("evenCuts", "bool"),
            sp("subsetSizesIsNull", "bool", special = "dummy"),
            sp("subsetSizes", "int", c(10, 1000), "exp", req = quote(subsetSizesIsNull == FALSE)),
# ** cp
            sp("na.action", "fix", "fuse"),
# ** dp
            sp("numThreads", "def", 1),
            sp("subsetGroups", "def", NULL),
            sp("tasks", "def", NULL),
            sp("probOfTaskCuts", "def", NULL),
            sp("numRandomTaskCuts", "def", 1))),
    autolearner("classif.randomForestSRC",
        list(
# ** vp
            sp("ntree", "int", c(100, 4000), "exp", id = "rf.numtree"),
            sp("mtry", "int", c(3, 40), id = "rf.mtry"),
            sp("nodesize", "int", c(1, 32), "exp", id = "rf.nodesize"),
            sp("nsplit", "int", c(0, 32), "exp"),
# ** cp
            sp("bootstrap", "fix", "none"),
            sp("na.action", "fix", "na.omit"),
            sp("membership", "fix", FALSE),
# ** dp
            sp("nimpute", "def", 1),
            sp("xwar.wt", "def", NULL),
            sp("forest", "def", TRUE),
            sp("seed", "def", NULL),
            sp("do.trace", "def", FALSE),
            sp("statistics", "def", FALSE),
            sp("fast.restore", "def", FALSE))),
    autolearner("classif.ranger",
        list(
# ** vp
            sp("num.trees", "int", c(100, 4000), "exp", id = "rf.numtree"),
            sp("mtry", "int", c(3, 40), id = "rf.mtry"),
            sp("min.node.size", "int", c(1, 32), "exp", id = "rf.nodesize"),
            sp("replace", "bool", id = "rf.replace"),
# ** cp
            sp("respect.unordered.factors", "fix", TRUE),
            sp("num.threads", "fix", 1),
            sp("verbose", "fix", FALSE),
# ** dp
            sp("split.select.weights", "def", NULL),
            sp("always.split.variables", "def", NULL),
            sp("importance", "def", "none"),
            sp("scale.permutation.importance", "def", FALSE),
            sp("save.memory", "def", FALSE),
            sp("seed", "def", NULL))),
    autolearner("classif.rFerns",
        list(
# ** vp
            sp("depth", "int", c(1, 14), "exp"),
            sp("ferns", "int", c(100, 4000), "exp"),
# ** dp
            sp("importance", "def", FALSE),
            sp("reportErrorEvery", "def", 0),
            sp("saveForest", "def", TRUE),
            sp("saveErrorPropagation", "def", FALSE))),
    autolearner("classif.rotationForest",
        list(
# ** vp
            sp("K", "int", c(2, 40), trafo = function(x) max(1, round(sum(info$n.feat) / x))),
            sp("L", "int", c(25, 100)))),
    autolearner("classif.ada",
        list(
# ** vp
            sp("loss", "cat", c("exponential", "logistic")),
            sp("type", "cat", c("discrete", "real", "gentle")),
            sp("iter", "int", c(25, 400), "exp", id = "boostree.iter"),
            sp("nu", "real", c(0.001, 0.3), "exp", id = "boostree.nu"),
            sp("model.coef", "bool"),
# ** cp
            sp("max.iter", "fix", 40),
# ** dp
            sp("minsplit", "def", 20),
            sp("minbucket", "def", NULL),
            sp("cp", "def", 0.01),
            sp("usesurrogate", "def", 2),
            sp("surrogatestyle", "def", 0),
            sp("maxdepth", "def", 30),
            sp("bag.frac", "def", 0.5),
            sp("bag.shift", "def", FALSE),
            sp("delta", "def", 1e-10),
            sp("maxcompete", "def", 4),
            sp("maxsurrogate", "def", 5),
            sp("verbose", "def", FALSE),
            sp("xval", "def", 10))),
    autolearner("classif.blackboost",
        list(
# ** vp
            sp("family", "cat", c("AdaExp", "Binomial")),
            sp("mstop", "int", c(25, 400), "exp", id = "boostree.iter"),
            sp("nu", "real", c(.001, 0.3), "exp", id = "boostree.nu"),
            sp("teststat", "cat", c("quad", "max")),
            sp("testtype", "cat", c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
            sp("mincriterion", "real", c(0.5, 0.99), req = quote(testtype != "Teststatistic")),
            sp("mincriterion.AMLRFIX1", "real", c(0.1, 1), req = quote(testtype == "Teststatistic")),
            sp("maxsurrogate", "int", c(0, 5)),
            sp("limitmtry", "bool", special = "dummy"),
            sp("mtry", "int", c(3, 20), req = quote(limitmtry == TRUE)),
            sp("minbucket", "int", c(1, 32), "exp"),
            sp("minsplit", "int", c(2, 64), "exp"),
            sp("stump", "bool"),
# ** cp
            sp("risk", "fix", "none"),
            sp("savesplitstats", "fix", FALSE),
# ** dp
            sp("nresample", "def", 9999),
            sp("maxdepth", "def", 0))),
    autolearner("classif.boosting",
        list(
# ** vp
            sp("boos", "bool"),
            sp("mfinal", "int", c(25, 400), "exp"),
            sp("coeflearn", "cat", c("Breiman", "Freund", "Zhu")),
            sp("minsplit", "int", c(2, 64), "exp"),
            sp("minbucket", "int", c(1, 32), "exp"),
            sp("cp", "real", c(1e-4, 0.5), "exp"),
            sp("usesurrogate", "cat", c(0, 1, 2)),
            sp("surrogatestyle", "cat", c(0, 1)),
            sp("maxdepth", "int", c(1, 30), "exp"),
# ** cp
            sp("xval", "fix", 0),
# ** dp
            sp("maxcompete", "def", 4),
            sp("maxsurrogate", "def", 5))),
    autolearner("classif.gbm",
        list(
# ** vp
            sp("distribution", "cat", c("bernoulli", "adaboost", "huberized", "multinomial")),
            sp("n.trees", "int", c(100, 4000), "exp"),
            sp("interaction.depth", "int", c(1, 3)),
            sp("n.minobsinnode", "int", c(1, 32), "exp"),
            sp("shrinkage", "real", c(0.0001, 0.3), "exp"),
            sp("bag.fraction", "real", c(.1, .9)),
# ** dp
            sp("cv.folds", "def", 0),
            sp("train.fraction", "def", 1),
            sp("verbose", "def", FALSE))),
    autolearner("classif.glmboost",
        list(
# ** vp
            sp("family", "cat", c("AdaExp", "Binomial")),
            sp("mstop", "int", c(25, 400), "exp", req = quote(m == "mstop")),
            sp("mstop.AMLRFIX1", "cat", c(400), req = quote(m != "mstop")),
            sp("nu", "real", c(.001, .3), "exp"),
            sp("risk", "cat", c("inbag", "oobag", "none"), req = quote(m != "aic")),
            sp("stopintern", "bool"),
            sp("m", "cat", c("mstop", "cv", "aic")),
# ** dp
            sp("center", "def", FALSE),
            sp("trace", "def", FALSE))),
    autolearner("classif.xgboost",
        list(
# ** vp
            sp("booster", "cat", c("gbtree", "gblinear")),
            sp("eta", "real", c(0, 1), req = quote(booster == "gbtree")),
            sp("gamma", "real", c(.0001, 1), "exp", req = quote(booster == "gbtree")),
            sp("max_depth", "int", c(1, 32), "exp", req = quote(booster == "gbtree")),
            sp("min_child_weight", "int", c(1, 32), "exp", req = quote(booster == "gbtree")),
            sp("subsample", "real", c(.3, 1), req = quote(booster == "gbtree")),
            sp("colsample_bytree", "real", c(.3, 1), req = quote(booster == "gbtree")),
            sp("num_parallel_tree", "int", c(1, 100), "exp", req = quote(booster == "gbtree")),
            sp("lambda", "real", c(.001, 10), "exp", req = quote(booster == "gblinear")),
            sp("lambda_bias", "real", c(.001, 10), "exp", req = quote(booster == "gblinear")),
            sp("alpha", "real", c(.001, 10), "exp", req = quote(booster == "gblinear")),
            sp("base_score", "real", c(0, 1)),
            sp("nrounds", "int", c(1, 16), "exp"),
# ** cp
            sp("nthread", "fix", 1),
            sp("verbose", "fix", 1),
            sp("print.every.n", "fix", 1000),
            sp("missing", "fix", 2147359313),
            sp("objective", "fix", NULL),
# ** dp
            sp("silent", "def", 0),
            sp("eval_metric", "def", "error"),
            sp("maximize", "def", TRUE),
            sp("early.stop.round", "def", 1))),
    autolearner("classif.dcSVM",
        list(
# ** vp
            sp("k", "int", c(2, 6), "exp"),
            sp("kernel", "cat", c(1, 2, 3)),
            sp("max.levels", "int", c(1, 32), "exp"),
            sp("final.training", "bool"),
            sp("cluster.method", "cat", c("kmeans", "kernkmeans"), id = "svm.cluster"),
# ** cp
            sp("m", "fix", 1000),
# ** dp
            sp("pre.scale", "def", FALSE),
            sp("seed", "def", NULL),
            sp("verbose", "def", TRUE),
            sp("valid.x", "def", NULL),
            sp("valid.y", "def", NULL),
            sp("valid.metric", "def", NULL),
            sp("cluster.fun", "def", NULL),
            sp("cluster.predict", "def", NULL),
            sp("early", "def", 0))),
    autolearner("classif.clusterSVM",
        list(
# ** vp
            sp("centers", "int", c(2, 6), "exp"),
            sp("lambda", "real", c(0.5, 4), "exp"),
            sp("type", "cat", c(1, 2, 3, 5)),
            sp("cost", "real", c(0.1, 10), "exp"),
            sp("cluster.method", "cat", c("kmeans", "kernkmeans"), id = "svm.cluster"),
# ** dp
            sp("cluster.object", "def", NULL),
            sp("sparse", "def", TRUE),
            sp("valid.x", "def", NULL),
            sp("valid.y", "def", NULL),
            sp("valid.metric", "def", NULL),
            sp("epsilon", "def", NULL),
            sp("bias", "def", TRUE),
            sp("wi", "def", NULL),
            sp("verbose", "def", 1),
            sp("seed", "def", NULL),
            sp("cluster.fun", "def", NULL),
            sp("cluster.predict", "def", NULL))),
    autolearner("classif.ksvm",
        list(
# ** vp
            sp("type", "cat", c("C-svc", "nu-svc", "C-bsvc", "spoc-svc", "kbb-svc")),
            sp("kernel", "cat", c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot"), id = "svm.kernel"),
            sp("C", "real", c(.125, 8), "exp", id = "svm.c", req = quote(type %in% c("C-svc", "C-bsvc", "spoc-svc", "kbb-svc"))),
            sp("nu", "real", c(.001, .1), "exp", id = "svm.nu", req = quote(type == "nu-svc")),
            sp("epsilon", "real", c(.001, .5), "exp", req = quote(type %in% c("eps-svr", "nu-svr", "eps-bsvm"))),
            sp("sigma", "real", c(.001, 100), "exp", req = quote(kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot"))),
            sp("degree", "int", c(1, 10), "exp", id = "svm.degree", req = quote(kernel %in% c("polydot", "anovadot", "besseldot"))),
            sp("scale", "real", c(.001, 100), "exp", id = "svm.scale", req = quote(kernel %in% c("polydot", "tanhdot"))),
            sp("offset", "real", c(0, 4), id = "svm.offset", req = quote(kernel %in% c("polydot", "tanhdot"))),
            sp("order", "int", c(0, 6), id = "svm.order", req = quote(kernel == "besseldot")),
            sp("shrinking", "bool", id = "svm.shrink"),
# ** cp
            sp("scaled", "fix", FALSE),
            sp("cache", "fix", 400),
            sp("fit", "fix", FALSE),
# ** dp
            sp("tol", "def", .001),
            sp("class.weights", "def", NULL))),
    autolearner("classif.gaterSVM",
        list(
# ** vp
            sp("m", "int", c(10, 50), "exp"),
            sp("max.iter", "int", c(50, 400), "exp"),
            sp("hidden", "int", c(3, 200), "exp"),
            sp("learningrate", "real", c(0.01, 0.8), "exp"),
            sp("threshold", "real", c(1e-7, 1e-1), "exp"),
            sp("stepmax", "int", c(50, 400), "exp"),
            sp("c", "cat", c(1, 100, 10000)),
# ** cp
            sp("verbose", "fix", TRUE),
# ** dp
            sp("seed", "def", NULL),
            sp("valid.x", "def", NULL),
            sp("valid.y", "def", NULL),
            sp("valid.metric", "def", NULL))),
    autolearner("classif.lssvm",
        list(
# ** vp
            sp("kernel", "cat", c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot"), id = "svm.kernel"),
            sp("tau", "real", c(.001, .100), "exp"),
            sp("reduced", "bool"),
            sp("sigma", "real", c(.001, 100), "exp", req = quote(kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot"))),
            sp("degree", "int", c(1, 10), "exp", id = "svm.degree", req = quote(kernel %in% c("polydot", "anovadot", "besseldot"))),
            sp("scale", "real", c(.001, 100), "exp", id = "svm.scale", req = quote(kernel %in% c("polydot", "tanhdot"))),
            sp("offset", "real", c(0, 4), id = "svm.offset", req = quote(kernel %in% c("polydot", "tanhdot"))),
            sp("order", "int", c(0, 6), id = "svm.order", req = quote(kernel == "besseldot")),
# ** cp
            sp("scaled", "fix", FALSE),
            sp("fitted", "fix", FALSE),
# ** dp
            sp("tol", "def", .0001))),
    autolearner("classif.svm",
        list(
# ** vp
            sp("type", "cat", c("C-classification", "nu-classification")),
            sp("cost", "real", c(.125, 8), "exp", id = "svm.c", req = quote(type == "C-classification")),
            sp("nu", "real", c(.001, .1), "exp", id = "svm.nu", req = quote(type == "nu-classification")),
            sp("kernel", "cat", c("linear", "polynomial", "radial", "sigmoid")),
            sp("degree", "int", c(1, 10), "exp", id = "svm.degree", req = quote(kernel == "polynomial")),
            sp("coef0", "real", c(0, 4), id = "svm.offset", req = quote(kernel == "polynomial" || kernel == "sigmoid")),
            sp("gamma", "real", c(.001, 100), "exp", id = "svm.scale", req = quote(kernel != "linear")),
            sp("shrinking", "bool", id = "svm.shrink"),
# ** cp
            sp("cachesize", "fix", 400),
            sp("fitted", "fix", FALSE),
            sp("scale", "fix", FALSE),
# ** dp
            sp("class.weights", "def", NULL),
            sp("tolerance", "def", 0.001),
            sp("cross", "def", 0))),
    autolearner("classif.dbnDNN",
        list(
# ** vp
            sp("numlayers", "cat", c(2, 5, 7), id = "nn.nlayer", special = "dummy"),
            sp("hidden", "int", c(3, 100), "exp", id = "nn.h2", req = quote(numlayers==2), dim = 2),
            sp("hidden.AMLRFIX1", "int", c(3, 100), "exp", id = "nn.h5", req = quote(numlayers==5), dim = 5),
            sp("hidden.AMLRFIX2", "int", c(3, 100), "exp", id = "nn.h7", req = quote(numlayers==7), dim = 7),
            sp("activationfun", "cat", c("sigm", "linear", "tanh"), id = "nn.afun"),
            sp("learningrate", "real", c(0.01, 2), "exp", id = "nn.lrate"),
            sp("momentum", "real", c(0, 0.8), id = "nn.momentum"),
            sp("learningrate_scale", "real", c(0.2, 1), "exp", id = "nn.lrs"),
            sp("numepochs", "int", c(1, 6), id = "nn.epochs"),
            sp("batchsize", "int", c(5, 500), "exp", id = "nn.bs"),
            sp("hidden_dropout", "real", c(0.5, 1), id = "nn.dropout"),
            sp("output", "cat", c("sigm", "linear", "softmax"), id = "nn.output"),
            sp("cd", "int", c(1, 5)),
# ** dp
            sp("visible_dropout", "def", 0))),
    autolearner("classif.multinom",
        list(
# ** vp
            sp("decay", "real", c(0.0001, 0.3), "exp", id = "nn.shallowdecay"),
            sp("maxit", "int", c(50, 400), "exp", id = "nn.shallowmaxit"),
            sp("MaxNWts", "cat", c(100000), special = "inject"),
# ** dp
            sp("Hess", "def", FALSE),
            sp("summ", "def", 0),
            sp("censored", "def", FALSE),
            sp("model", "def", FALSE),
            sp("rang", "def", 0.7),
            sp("trace", "def", TRUE),
            sp("abstoll", "def", 0.0001),
            sp("reltoll", "def", 1e-8))),
    autolearner("classif.nnet",
        list(
# ** vp
            sp("size", "int", c(3, 200), "exp"),
            sp("skip", "bool"),
            sp("decay", "real", c(0.0001, 0.3), "exp", id = "nn.shallowdecay"),
            sp("maxit", "int", c(50, 400), "exp", id = "nn.shallowmaxit"),
# ** cp
            sp("MaxNWts", "fix", 100000),
# ** dp
            sp("rang", "def", 0.7),
            sp("Hess", "def", FALSE),
            sp("trace", "def", TRUE),
            sp("abstoll", "def", 0.0001),
            sp("reltoll", "def", 1e-8))),
    autolearner("classif.nnTrain",
        list(
# ** vp
            sp("numlayers", "cat", c(2, 5, 7), id = "nn.nlayer", special = "dummy"),
            sp("hidden", "int", c(3, 100), "exp", id = "nn.h2", req = quote(numlayers==2), dim = 2),
            sp("hidden.AMLRFIX1", "int", c(3, 100), "exp", id = "nn.h5", req = quote(numlayers==5), dim = 5),
            sp("hidden.AMLRFIX2", "int", c(3, 100), "exp", id = "nn.h7", req = quote(numlayers==7), dim = 7),
            sp("activationfun", "cat", c("sigm", "linear", "tanh"), id = "nn.afun"),
            sp("learningrate", "real", c(0.01, 2), "exp", id = "nn.lrate"),
            sp("momentum", "real", c(0, 0.8), id = "nn.momentum"),
            sp("learningrate_scale", "real", c(0.2, 1), "exp", id = "nn.lrs"),
            sp("numepochs", "int", c(1, 6), id = "nn.epochs"),
            sp("batchsize", "int", c(5, 500), "exp", id = "nn.bs"),
            sp("hidden_dropout", "real", c(0.5, 1), id = "nn.dropout"),
            sp("output", "cat", c("sigm", "linear", "softmax"), id = "nn.output"),
# ** dp
            sp("initW", "def", NULL),
            sp("initB", "def", NULL),
            sp("visible_dropout", "def", 0))),
    autolearner("classif.neuralnet",
        list(
# ** vp
            sp("numlayers", "cat", c(2, 5, 7), id = "nn.nlayer", special = "dummy"),
            sp("hidden", "int", c(3, 100), "exp", id = "nn.h2", req = quote(numlayers==2), dim = 2),
            sp("hidden.AMLRFIX1", "int", c(3, 100), "exp", id = "nn.h5", req = quote(numlayers==5), dim = 5),
            sp("hidden.AMLRFIX2", "int", c(3, 100), "exp", id = "nn.h7", req = quote(numlayers==7), dim = 7),
            sp("threshold", "real", c(.0001, .1), "exp"),
            sp("stepmax", "int", c(50, 400), "exp"),
            sp("rep", "int", c(1, 16), "exp"),
            sp("algorithm", "cat", c("backprop", "rprop+", "rprop-", "sag", "slr")),
            sp("learningrate.limit", "real", c(.01, 2), "exp", req = quote(algorithm != "backprop"), dim = 2),
            sp("learningrate.factor", "real", c(.01, 2), "exp", req = quote(algorithm != "backprop"), dim = 2),
            sp("learningrate", "real", c(0.01, 2), "exp", id = "nn.lrate", req = quote(algorithm == "backprop")),
            sp("err.fct", "cat", c("sse", "ce")),
            sp("act.fct", "cat", c("logistic", "tanh")),
# ** dp
            sp("startweights", "def", NULL),
            sp("lifesign", "def", "none"),
            sp("lifesign.step", "def", 1000),
            sp("exclude", "def", NULL),
            sp("constant.weights", "def", NULL),
            sp("likelihood", "def", FALSE),
            sp("linear.output", "def", TRUE))),
    autolearner("classif.saeDNN",
        list(
# ** vp
            sp("numlayers", "cat", c(2, 5, 7), id = "nn.nlayer", special = "dummy"),
            sp("hidden", "int", c(3, 100), "exp", id = "nn.h2", req = quote(numlayers==2), dim = 2),
            sp("hidden.AMLRFIX1", "int", c(3, 100), "exp", id = "nn.h5", req = quote(numlayers==5), dim = 5),
            sp("hidden.AMLRFIX2", "int", c(3, 100), "exp", id = "nn.h7", req = quote(numlayers==7), dim = 7),
            sp("activationfun", "cat", c("sigm", "linear", "tanh"), id = "nn.afun"),
            sp("learningrate", "real", c(0.01, 2), "exp", id = "nn.lrate"),
            sp("momentum", "real", c(0, 0.8), id = "nn.momentum"),
            sp("learningrate_scale", "real", c(0.2, 1), "exp", id = "nn.lrs"),
            sp("numepochs", "int", c(1, 6), id = "nn.epochs"),
            sp("batchsize", "int", c(5, 500), "exp", id = "nn.bs"),
            sp("hidden_dropout", "real", c(0.5, 1), id = "nn.dropout"),
            sp("output", "cat", c("sigm", "linear", "softmax"), id = "nn.output"),
            sp("sae_output", "cat", c("sigm", "linear", "softmax")),
# ** dp
            sp("visible_dropout", "def", 0))),
    autolearner("classif.bdk",
        list(
# ** vp
            sp("xdim", "int", c(5, 100), "exp", id = "koho.x"),
            sp("ydim", "int", c(5, 100), "exp", id = "koho.y"),
            sp("topo", "cat", c("rectangular", "hexagonal"), id = "koho.topo"),
            sp("rlen", "int", c(50, 400), "exp", id = "koho.rlen"),
            sp("alpha", "real", c(0, 1), id = "koho.alpha", trafo = function(x) c(.02, .001) * 20^x, dim = 2),
            sp("xweight", "real", c(0.5, 0.9), id = "koho.xweight"),
            sp("n.hood", "cat", c("circular", "square"), id = "koho.shape"),
            sp("toroidal", "bool", id = "koho.toro"),
# ** cp
            sp("contin", "fix", FALSE),
# ** dp
            sp("radius", "def", NULL))),
    autolearner("classif.lvq1"),
    autolearner("classif.naiveBayes",
        list(
# ** dp
            sp("laplace", "def", 0))),
    autolearner("classif.OneR",
        list(
# ** vp
            sp("B", "int", c(1, 32), "exp"),
# ** dp
            sp("output-debug-info", "def", FALSE))),
    autolearner("classif.JRip",
        list(
# ** vp
            sp("F", "int", c(2, 8)),
            sp("N", "int", c(2, 64), "exp"),
            sp("O", "int", c(1, 10)),
            sp("P", "bool"),
# ** dp
            sp("D", "def", FALSE),
            sp("S", "def", NULL),
            sp("E", "def", FALSE),
            sp("output-debug-info", "def", FALSE))),
    autolearner("classif.xyf",
        list(
# ** vp
            sp("xdim", "int", c(5, 100), "exp", id = "koho.x"),
            sp("ydim", "int", c(5, 100), "exp", id = "koho.y"),
            sp("topo", "cat", c("rectangular", "hexagonal"), id = "koho.topo"),
            sp("rlen", "int", c(50, 400), "exp", id = "koho.rlen"),
            sp("alpha", "real", c(0, 1), id = "koho.alpha", trafo = function(x) c(.02, .001) * 20^x, dim = 2),
            sp("xweight", "real", c(0.5, 0.9), id = "koho.xweight"),
            sp("n.hood", "cat", c("circular", "square"), id = "koho.shape"),
            sp("toroidal", "bool", id = "koho.toro"),
# ** cp
            sp("contin", "fix", FALSE),
# ** dp
            sp("radius", "def", NULL))))