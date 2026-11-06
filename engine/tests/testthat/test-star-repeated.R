wide_of <- function(d) {
  w <- reshape(d, idvar = setdiff(names(d), c("Day", "Height")), timevar = "Day", direction = "wide")
  list(Y = as.matrix(w[grep("^Height", names(w))]), f = factor(w$Fertilizer))
}

test_that("star_repeated() matches anova.mlm() and mauchly.test() (RCBD)", {
  d <- read.csv(test_path("..", "..", "..", "prototype", "app", "example_rep_rcbd.csv"))
  r <- star_repeated(d, "Height", "Fertilizer", "Day", "Block", design = "RepRCBD")
  w <- wide_of(d)
  ref <- anova(lm(w$Y ~ w$f), X = ~1, test = "Spherical")
  expect_equal(r$sphericity$tests$F, ref$F[1:2])
  expect_equal(r$sphericity$tests$P_GG, ref$`G-G Pr`[1:2])
  expect_equal(r$sphericity$tests$P_HF, ref$`H-F Pr`[1:2])
  expect_equal(r$sphericity$mauchly$p, mauchly.test(lm(w$Y ~ w$f), X = ~1)$p.value)
  # the within-plot error of the contrasts is Error(b) of the split plot in time
  expect_equal(r$sphericity$ss_error, r$anova$SS[r$anova$Source == "Error(b)"])
  expect_equal(r$design, "RepRCBD")
  expect_identical(r$sphericity$correction, "none")
})

test_that("star_repeated() corrects the within-plot tests when sphericity fails (CRD)", {
  d <- read.csv(test_path("..", "..", "..", "prototype", "app", "example_rep_crd.csv"))
  r <- star_repeated(d, "Height", "Fertilizer", "Day", "Pot", design = "RepCRD")
  s <- r$sphericity
  expect_identical(s$correction, "GG")
  expect_equal(r$effects$P[r$effects$Effect != "Fertilizer"], s$tests$P_GG)
  # the treatment (between plots) is not affected
  expect_equal(r$effects$P[1], r$anova$P[1])
  expect_match(r$apa[2], "Greenhouse-Geisser corrected")
  hf <- star_repeated(d, "Height", "Fertilizer", "Day", "Pot", design = "RepCRD", correction = "HF")
  expect_equal(hf$effects$P[2:3], s$tests$P_HF)
})

test_that("two times need no correction, and incomplete plots are refused", {
  d <- read.csv(test_path("..", "..", "..", "prototype", "app", "example_rep_crd.csv"))
  two <- star_repeated(d[d$Day %in% c("D30", "D60"), ], "Height", "Fertilizer", "Day", "Pot", design = "RepCRD")
  expect_identical(two$sphericity$correction, "none")
  expect_equal(two$sphericity$gg, 1)
  expect_error(star_repeated(d[-1, ], "Height", "Fertilizer", "Day", "Pot", design = "RepCRD"), "not complete")
  expect_error(star_repeated(d, "Height", "Fertilizer", "Day", NULL, design = "RepCRD"), "plot column")
})

test_that("times are put in time order", {
  expect_identical(.star_time_order(c("D30", "D120", "D60", "D30")), c("D30", "D60", "D120"))
  expect_identical(.star_time_order(c("Week 10", "Week 2")), c("Week 2", "Week 10"))
  expect_identical(.star_time_order(c("Before", "After")), c("Before", "After"))
  d <- read.csv(test_path("..", "..", "..", "prototype", "app", "example_rep_rcbd.csv"))
  r <- star_repeated(d, "Height", "Fertilizer", "Day", "Block", design = "RepRCBD")
  expect_identical(r$means$Day$Treatment, c("D30", "D60", "D90", "D120"))
})
