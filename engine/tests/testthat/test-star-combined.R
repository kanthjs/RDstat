# Combined analysis over environments (Gomez & Gomez two crop seasons, RCBD). Reference values: the original STAR
# combAOVTest on the same data (design = "RCBD", pwTest = "LSD").
data(GomezTwoCropSeasonRCB, package = "STAR", envir = environment())

test_that("combined RCBD: ANOVA table, CV and letters match STAR's combAOVTest", {
  r <- star_combined(GomezTwoCropSeasonRCB, "GrainYield", "NitrogenRate", "Season", "Rep", test = "LSD")
  expect_s3_class(r, "star_aov")
  expect_equal(r$anova$Source, c("Season", "Rep within Season", "NitrogenRate", "NitrogenRate:Season", "Error", "Total"))
  expect_equal(r$anova$DF, c(1, 4, 4, 4, 16, 29))
  expect_equal(round(r$anova$SS, 4), c(4.4954, 1.2616, 18.7488, 9.6544, 7.0636, 41.2239))
  # Season is tested against Rep within Season, the rest against the pooled error
  expect_equal(round(r$anova$F[1:4], 2), c(14.25, 0.71, 10.62, 5.47))
  expect_equal(round(r$anova$P[1:4], 4), c(0.0195, 0.5942, 0.0002, 0.0057))
  expect_equal(round(r$cv, 2), 12.05)
  expect_identical(r$interaction_significant, TRUE)
  sn <- r$simple[["NitrogenRate within Season"]]
  expect_equal(sn$Group[sn$Season == "Dry"], c("b", "a", "a", "a", "a"))
  expect_equal(sn$Group[sn$Season == "Wet"], c("c", "a", "ab", "bc", "c"))
  ss <- r$simple[["Season within NitrogenRate"]]
  expect_equal(ss$Group[ss$NitrogenRate == "150"], c("a", "b"))
  expect_equal(r$effects$ErrDF, c(16, 4, 16))
  expect_equal(r$error_terms[["Season"]], "Rep within Season")
})

test_that("combined RCBD: each environment and Bartlett's test", {
  r <- star_combined(GomezTwoCropSeasonRCB, "GrainYield", "NitrogenRate", "Season", "Rep")
  expect_equal(r$by_env$Season, c("Dry", "Wet"))
  expect_equal(r$by_env$ErrorDF, c(8, 8))
  expect_equal(round(r$by_env$F, 2), c(6.43, 10.91))
  # pooled error of the two seasons = the combined error mean square
  expect_equal(mean(r$by_env$ErrorMS), r$anova$MS[5])
  expect_equal(round(r$bartlett$p, 4), 0.4323)
  expect_match(r$summary, "homogeneous")
})

test_that("combined CRD and refused layouts", {
  r <- star_combined(GomezTwoCropSeasonRCB, "GrainYield", "NitrogenRate", "Season", design = "CRD")
  expect_equal(r$anova$Source, c("Season", "NitrogenRate", "NitrogenRate:Season", "Error", "Total"))
  expect_equal(r$anova$DF, c(1, 4, 4, 20, 29))
  expect_error(star_combined(GomezTwoCropSeasonRCB[-1, ], "GrainYield", "NitrogenRate", "Season", "Rep"), "complete and balanced")
  expect_error(star_combined(GomezTwoCropSeasonRCB[GomezTwoCropSeasonRCB$Season == "Dry", ], "GrainYield", "NitrogenRate", "Season", "Rep"), "at least two levels")
})
