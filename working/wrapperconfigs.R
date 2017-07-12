
expect_nle = function(a, b) {
  expect_set_equal(names(a), names(b))
  for (n in names(a)) {
    expect_set_equal(a[[n]], b[[n]])
  }
}


makeBogusWrapper = function(name, required, missingsok, handles, conversions = NULL) {
  assertChoice(handles, c("numerics", "factors", "ordered"))
  if (is.null(conversions)) {
    conversions = list()
  }
  assertSubset(names(conversions), c("numerics", "factors", "ordered", "missings"))
  if ("missings" %in% names(conversions)) {
    assert(missingsok)
  }
  for (n in names(conversions)) {
    assert(n %nin% conversions[[n]])
  }
  for (h in handles) {
    conversions[[h]] = union(conversions[[h]], h)
  }
  if (missingsok && "missings" %nin% names(conversions)) {
    conversions[["missings"]] = "missings"
  }

  autolearner(
      stacktype = ifelse(required, "requiredwrapper", "wrapper"),
      searchspace = list(),
      learner = autoWrapper(
          name = name,
          constructor = identity,
          conversion = conversions))
}

numwrappers = makeNamedAlList(
    makeBogusWrapper("nh", FALSE, FALSE, "numerics"),
    makeBogusWrapper("nhm", FALSE, TRUE, "numerics", list(missings = "numerics")))

facwrappers = makeNamedAlList(
    makeBogusWrapper("fh", FALSE, FALSE, "numerics", list(factors = "ordered")),
    makeBogusWrapper("fhm", FALSE, TRUE, "numerics", list(missings = "factors")))

multiwrappers = makeNamedAlList(
    makeBogusWrapper("w1", FALSE, TRUE, "numerics", list(missings = "factors", factors = "numerics")),
    makeBogusWrapper("w2", FALSE, TRUE, "numerics", list(numerics = "ordered")))

manywrappers = makeNamedAlList(
    makeBogusWrapper("wa", FALSE, TRUE, "factors", list(missings = "factors")),
    makeBogusWrapper("wb", FALSE, TRUE, "factors", list(factors = "ordered")),
    makeBogusWrapper("wc", FALSE, TRUE, "factors", list(factors = "ordered", missings = "factors")),
    makeBogusWrapper("wd", FALSE, TRUE, "numerics"),
    makeBogusWrapper("we", FALSE, TRUE, "factors", list(ordered = "numerics")))

manywrappers2 = makeNamedAlList(
    makeBogusWrapper("wa", FALSE, TRUE, "factors", list(missings = "factors")),
    makeBogusWrapper("wb", FALSE, TRUE, "factors", list(factors = "ordered", ordered = "numerics")),
    makeBogusWrapper("wc", FALSE, TRUE, "factors", list(factors = "ordered", missings = "factors")))

manywrappers3 = makeNamedAlList(
    makeBogusWrapper("wa", FALSE, TRUE, "factors", list(missings = "factors", factors = "ordered")),
    makeBogusWrapper("wb", FALSE, TRUE, "factors", list(factors = "ordered", ordered = "numerics")))

idconv = list(factors = "factors", ordered = "ordered", numerics = "numerics", missings = "missings")

expect_nle(calculateConversionLimit(list()), idconv)

misnum = idconv
misnum$missings %c=% "numerics"
expect_nle(calculateConversionLimit(numwrappers), misnum)

misfac = idconv
misfac$missings %c=% c("factors", "ordered")
misfac$factors %c=% "ordered"
expect_nle(calculateConversionLimit(facwrappers), misfac)

mismulti = idconv
mismulti$missings %c=% "factors"
mismulti$factors %c=% c("numerics", "ordered")
mismulti$numerics %c=% "ordered"
expect_nle(calculateConversionLimit(multiwrappers), mismulti)

mismany = idconv
mismany$missings %c=% c("factors", "ordered", "numerics")
mismany$factors %c=% c("ordered", "numerics")
mismany$ordered %c=% "numerics"
expect_nle(calculateConversionLimit(manywrappers), mismany)

expect_nle(calculateConversionLimit(manywrappers2), mismany)

mismany3 = idconv
mismany3$missings %c=% c("factors", "ordered")
mismany3$factors %c=% c("ordered", "numerics")
mismany3$ordered %c=% "numerics"
expect_nle(calculateConversionLimit(manywrappers3), mismany3)











