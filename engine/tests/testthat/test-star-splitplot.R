# Split plot in RCBD (Gomez & Gomez rice data). Reference values: original STAR 2.0.2 on R 2.15.1
# (engine/tests/testthat/expected/AOV_SplitRCBD.txt).
data(GomezSplitRCBD, package = "STAR", envir = environment())

test_that("split plot: ANOVA table, CVs and letters match the original STAR", {
  r <- star_splitplot(GomezSplitRCBD, "Yield", main = "Nitrogen", sub = "Variety", block = "Rep", test = "auto")
  expect_s3_class(r, "star_aov")
  expect_equal(r$anova$Source, c("Rep", "Nitrogen", "Error(a)", "Variety", "Nitrogen:Variety", "Error(b)", "Total"))
  expect_equal(r$anova$DF, c(2, 5, 10, 3, 15, 36, 71))
  expect_equal(round(r$anova$F[c(1, 2, 4, 5)], 2), c(3.81, 42.87, 85.71, 13.22))
  expect_equal(round(r$anova$SS[c(3, 6)], 4), c(1419678.8056, 12584873.1667))
  expect_equal(round(c(r$cv_a, r$cv_b), 2), c(6.88, 10.79))
  expect_identical(r$interaction_significant, TRUE)
  # Variety within each Nitrogen level (LSD, Error(b)): level 1 = a a a... see original report
  sm <- r$simple[["Variety within Nitrogen"]]
  lv1 <- sm[sm$Nitrogen == levels(GomezSplitRCBD$Nitrogen)[1], ]
  expect_equal(lv1$Group[match(c("C4-63", "IR5", "IR8", "Peta"), lv1$Variety)], c("b", "a", "a", "a"))
  # Nitrogen within each Variety (Tukey, pooled error 297676.8278 on 43 df)
  sn <- r$simple[["Nitrogen within Variety"]]
  c4 <- sn[sn$Variety == "C4-63", ]
  expect_equal(c4$Group[match(levels(GomezSplitRCBD$Nitrogen), c4$Nitrogen)], c("b", "a", "a", "a", "a", "a"))
  pe <- sn[sn$Variety == "Peta", ]
  expect_equal(pe$Group[match(levels(GomezSplitRCBD$Nitrogen), pe$Nitrogen)], c("a", "a", "a", "a", "b", "b"))
  expect_equal(r$apa[1], "The main effect of Nitrogen on Yield was significant, F(5, 10) = 42.87, p < .001, partial eta squared = .96.")
})

test_that("split plot: an unbalanced layout is refused", {
  expect_error(star_splitplot(GomezSplitRCBD[-1, ], "Yield", "Nitrogen", "Variety", "Rep"), "complete and balanced")
  expect_error(star_splitplot(GomezSplitRCBD, "Yield", c("Nitrogen", "Variety"), "Variety", "Rep"), "exactly one")
})

test_that("split plot: residuals are checked on Error(b) and on Error(a)", {
  r <- star_splitplot(GomezSplitRCBD, "Yield", main = "Nitrogen", sub = "Variety", block = "Rep")
  dg <- r$diagnostics
  expect_s3_class(dg, "star_diagnostics")
  # sub-plot residuals are those of the Within stratum: SS = Error(b) SS
  expect_equal(sum(dg$residuals$Residual^2), r$anova$SS[r$anova$Source == "Error(b)"])
  expect_equal(nrow(dg$residuals), 72)
  # main-plot residuals come from the whole-plot means: SS x (sub-plot levels) = Error(a) SS, df = Error(a) df
  mp <- dg$main_plot
  expect_equal(nrow(mp$residuals), 18)
  expect_equal(mp$df, 10)
  expect_equal(4 * sum(mp$residuals$Residual^2), r$anova$SS[r$anova$Source == "Error(a)"])
  expect_true(any(grepl("^Main plots \\(Error\\(a\\)\\)", dg$messages)))
})

test_that("split plot: a wrong single value is flagged in the sub plots, a wrong main plot in the main plots", {
  d <- GomezSplitRCBD
  d$Yield[5] <- d$Yield[5] * 2
  dg <- star_splitplot(d, "Yield", "Nitrogen", "Variety", "Rep")$diagnostics
  expect_equal(dg$outliers$Row[1], "5")
  expect_equal(dg$outliers$Flag[1], "outlier")

  d <- GomezSplitRCBD
  i <- d$Rep == d$Rep[1] & d$Nitrogen == d$Nitrogen[1]
  d$Yield[i] <- d$Yield[i] + 2500
  dg <- star_splitplot(d, "Yield", "Nitrogen", "Variety", "Rep")$diagnostics
  expect_equal(dg$main_plot$n_outliers, 1)
  expect_match(dg$main_plot$outliers$Row[1], paste0("Rep ", d$Rep[1], " / Nitrogen ", d$Nitrogen[1]), fixed = TRUE)
  expect_false(dg$ok)
})
