



metaFilterCPO = function(filters, type) {
  cpoMeta(.cpo.name = paste0("filter.", type),
    .par.set = makeParamSet(
        makeDiscreteLearnerParam("cutofftype", c("perc.of.features", "perc.of.range")),
        makeNumericLearnerParam("cutoff", lower = 0, upper = 1),
        makeDiscreteLearnerParam("filter", filters)),
    .datasplit = "task", .properties = c("missings", type),
    cpo.build = function(data, target, cutofftype, cutoff, filter) {
      fval = generateFilterValuesData(data, filter)
      if (cutofftype == "perc.of.features") {
        rng = range(fval$data[[3]])
        cpoFilterFeatures(filter, fval = fval,
          threshold = cutoff * rng[1] + (1 - cutoff) * rng[2])
      } else {
        cpoFilterFeatures(filter, perc = cutoff)
      }
    })
}

cpoImputeConstFact = makeCPO("cfimputer", .datasplit = "factor",
  .properties.adding = "missings", .stateless = TRUE,
  cpo.trafo = {
    for (n in names(data)) {
      data[[n]] = factor(data[[n]], levels = c("MISSING", levels(data[[n]])))
      data[[n]][is.na(data[[n]])] = "MISSING"
    }
    data
  }, cpo.retrafo = {
    for (n in names(data)) {
      data[[n]] = factor(data[[n]], levels = c("MISSING", levels(data[[n]])))
      data[[n]][is.na(data[[n]])] = "MISSING"
    }
    data
  })

cpoCollapseFact = makeCPO("collapse.fact",
  max.collapsed.class.prevalence: numeric[0, ~1],
  .datasplit = "factor",
  cpo.trafo = {
    newlevels = sapply(data, function(d) {
      if (all(is.na(d))) {
        return(levels(d))
      }
      fractions = cumsum(sort(table(d))) / sum(!is.na(d))
      collapse = names(fractions)[fractions < max.collapsed.class.prevalence]
      if (length(collapse) > 1) {
        nocollapse = setdiff(levels(d), collapse)
        lvls = list(collapsed = collapse)
        insert(lvls, stats::setNames(as.list(nocollapse), nocollapse))
      } else {
        levels(d)
      }
    }, simplify = FALSE)
    cpo.retrafo = function(data) {
      for (n in names(data)) {
        levels(data[[n]]) = newlevels[[n]]
      }
      data
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)

splitnumcpo = makeCPO("as.factor", numsplits: integer[2, 5],
  .properties.adding = "numerics", .properties.needed = "factors",
  .datasplit = "numeric", cpo.trafo = {
    breaks = lapply(data, function(d)
      unique(c(-Inf, quantile(d, (1:(numsplits - 1)) / numsplits, na.rm = TRUE), Inf)))
    cpo.retrafo = function(data) {
      as.data.frame(mapply(function(d, b) cut(d, breaks = b), d = data, b = breaks, SIMPLIFY = FALSE),
        row.names = rownames(data))
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)

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


amlr.numfilters = c("anova.test", "chi.squared", "variance", "gain.ratio",
  "randomForestSRC.rfsrc", "univariate.model.score")
automlrNumFilterCPO = metaFilterCPO(amlr.numfilters, "numerics")

amlr.facfilters = c("chi.squared", "gain.ratio",
  "randomForestSRC.rfsrc", "univariate.model.score")
automlrFacFilterCPO = metaFilterCPO(amlr.facfilters, "factors")

amlr.ordfilters = c("randomForestSRC.rfsrc", "univariate.model.score")
automlrOrdFilterCPO = metaFilterCPO(amlr.ordfilters, "ordered")

