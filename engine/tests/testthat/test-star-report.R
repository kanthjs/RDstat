# star_report(): STAR's own printout of a result, built on demand
data(GomezSplitRCBD, package = "STAR", envir = environment())
data(GomezTwoCropSeasonRCB, package = "STAR", envir = environment())
rcbd <- subset(GomezSplitRCBD, Variety == "IR5")[, c("Rep", "Nitrogen", "Yield")]

test_that("RCBD: the classic report uses the chosen test and does not change the result", {
  r <- star_aov(rcbd, "Yield", "Nitrogen", "Rep", design = "RCBD", test = "LSD")
  rep <- star_report(r)
  expect_match(rep, "ANOVA TABLE", all = FALSE)
  expect_match(rep, "Least Significant", all = FALSE)
  expect_null(r$report)
  expect_match(star_report(star_aov(rcbd, "Yield", "Nitrogen", "Rep", design = "RCBD", test = "duncan")), "Duncan", all = FALSE)
})

test_that("split plot and combined analysis have their classic reports", {
  expect_match(star_report(star_splitplot(GomezSplitRCBD, "Yield", "Nitrogen", "Variety", "Rep")), "Split Plot in Randomized Complete Block", all = FALSE)
  expect_match(star_report(star_combined(GomezTwoCropSeasonRCB, "GrainYield", "NitrogenRate", "Season", "Rep")), "Combined Analysis of Variance", all = FALSE)
})

test_that("a result without data gives an empty report", {
  expect_identical(star_report(list(data = NULL)), character())
})
