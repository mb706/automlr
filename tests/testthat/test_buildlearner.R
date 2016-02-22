
context("buildlearner")

createTestData = function(nrow, nNumeric=0, nFactor=0, nOrdered=0) {
  res = as.data.frame(c(
                replicate(nNumeric, rnorm(nrow), FALSE),
                replicate(nFactor, factor(sample(c("a", "b", "c"), nrow, TRUE), ordered=FALSE), FALSE),
                replicate(nOrdered, factor(sample(c("a", "b", "c"), nrow, TRUE), ordered=TRUE), FALSE)))
  names(res) = c(
      if (nNumeric) paste0("num.", seq_len(nNumeric)),
      if (nFactor) paste0("fac.", seq_len(nFactor)),
      if (nOrdered) paste0("ord.", seq_len(nOrdered)))
  res
}




createTestClassifTask = function(nrow, nNumeric, nFactor, nOrdered, nClasses) {
  data = 
  
}