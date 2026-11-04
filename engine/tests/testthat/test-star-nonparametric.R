# Rank-based alternatives. Reference values: stats::kruskal.test, stats::friedman.test and stats::wilcox.test.
data(Insecticides, package = "STAR", envir = environment())
data(GomezTwoCropSeasonRCB, package = "STAR", envir = environment())

test_that("Kruskal-Wallis: statistic and letters on mean ranks", {
  d <- Insecticides
  resp <- names(d)[vapply(d, is.numeric, NA)][1]
  trt <- names(d)[!vapply(d, is.numeric, NA)][1]
  r <- star_nonparametric(d, resp, trt)
  kt <- stats::kruskal.test(d[[resp]], d[[trt]])
  expect_equal(r$statistic, unname(kt$statistic))
  expect_equal(r$p, kt$p.value)
  expect_equal(r$name, "Kruskal-Wallis")
  expect_equal(nrow(r$means), nlevels(factor(d[[trt]])))
  expect_identical(r$significant, kt$p.value < 0.05)
  if (r$significant) expect_false(anyNA(r$means$Group))
})

test_that("Friedman: statistic matches friedman.test; no letters when not significant", {
  d <- GomezTwoCropSeasonRCB[GomezTwoCropSeasonRCB$Season == "Dry", ]
  r <- star_nonparametric(d, "GrainYield", "NitrogenRate", block = "Rep")
  ft <- stats::friedman.test(d$GrainYield, d$NitrogenRate, d$Rep)
  expect_equal(r$name, "Friedman")
  expect_equal(r$statistic, unname(ft$statistic))
  expect_equal(r$p, ft$p.value)
  expect_equal(sum(r$means$Rank), 3 * 5 * 6 / 2)
  if (!r$significant) expect_true(all(is.na(r$means$Group)))
  expect_error(star_nonparametric(d[-1, ], "GrainYield", "NitrogenRate", block = "Rep"), "exactly one observation")
})

test_that("t test results carry the Wilcoxon alternative", {
  d <- data.frame(g = rep(c("a", "b"), each = 6), y = c(5.1, 4.8, 5.6, 5.0, 4.9, 5.3, 6.2, 6.0, 5.8, 6.5, 6.1, 5.9))
  r <- star_ttest(d, "independent", response = "y", group = "g")
  w <- stats::wilcox.test(d$y[1:6], d$y[7:12], conf.int = TRUE)
  expect_equal(r$nonparametric$P, w$p.value)
  expect_equal(r$nonparametric$Estimate, unname(w$estimate))
  one <- star_ttest(d, "one", response = "y", mu = 5)
  w1 <- stats::wilcox.test(d$y, mu = 5, conf.int = TRUE, exact = FALSE)
  # the Hodges-Lehmann estimate is a uniroot() solution, so the two calls agree to its tolerance only
  expect_equal(one$nonparametric$Estimate, unname(w1$estimate) - 5, tolerance = 1e-4)
  pd <- data.frame(x = d$y[1:6], z = d$y[7:12])
  pr <- star_ttest(pd, "paired", x = "x", y = "z")
  expect_match(pr$nonparametric$Method, "signed-rank")
})
