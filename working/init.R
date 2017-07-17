

rm(list=objects()[grep('^(train|predict)Learner.[0-9]*$',objects())])
rm(list=objects()[objects() =="last.dump"])

reloadPckgs <- function(toReload) {
  lns <- loadedNamespaces()
  lnsAsPkg <- sub('.', '', lns, fixed=TRUE)
  pkgWithLibs = character(0)
  for (p in sapply(.dynLibs(), get, x='name')) {
    pkgWithLibs = c(pkgWithLibs, lns[lnsAsPkg == p])
  }
  delendum <- which(pkgWithLibs %in% names(toReload))
  if (length(delendum) == 0) {
    return()
  }
  firstPkgToUnload <- min(delendum)
  delendum <- pkgWithLibs[firstPkgToUnload:length(pkgWithLibs)]
  deleteQueue <- list()
  for (d in rev(delendum)) deleteQueue[[d]] = 1

  tried <- list()
  while (length(deleteQueue)) {
     #  print(names(deleteQueue))
    removing <- names(deleteQueue)[1]
    deps <- setdiff(getNamespaceUsers(removing), names(tried))
    for (dependency in deps) {
      deleteQueue[[dependency]] <- 1
    }
    deleteQueue[[removing]] <- NULL
    if (length(deps) == 0) {
      print(c("Removing", removing))
      tried[[removing]] <- 1
      pkgload::unload(find.package(removing))
    } else {
      deleteQueue[[removing]] <- 1
    }
  }
  lapply(toReload, devtools::load_all)
}

upstart <- function() {
  reloadPckgs(list(ParamHelpers="../../ParamHelpers", mlr="../../mlr",
                   smoof="../../smoof", mlrMBO="../../mlrMBO"))
  library('testthat')
  library('checkmate')
  library(roxygen2)
  roxygenise('..')
  options(error=dump.frames)
  options(warn=1)
  options(width=100)
}
