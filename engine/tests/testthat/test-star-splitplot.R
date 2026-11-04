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
