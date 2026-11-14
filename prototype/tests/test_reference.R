# Checks the app's analyses against independent software: other R packages, or base R fitted another way. The tests in
# test_server.R compare with the original STAR program (R 2.15.1) and with the R code the app writes; this file covers
# what those cannot: the tests this app computes itself (Games-Howell, Dunn, trimmed means) and the error strata of the
# split plot. A reference package that is not installed is skipped with a note, so the file runs anywhere; CI installs
# them. Run from anywhere: Rscript prototype/tests/test_reference.R
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
app_dir <- file.path(here, "..", "app")
for (f in list.files(file.path(app_dir, "star"), "^star_.*[.]R$", full.names = TRUE)) source(f)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE) else cat("ok  ", msg, "\n")
skipped <- character()
have <- function(pkg) { ok <- requireNamespace(pkg, quietly = TRUE); if (!ok) skipped <<- c(skipped, pkg); ok }
same <- function(a, b, tol = 1e-6) isTRUE(all.equal(unname(as.numeric(a)), unname(as.numeric(b)), tolerance = tol))

crd <- read.csv(file.path(app_dir, "example_crd.csv"), stringsAsFactors = FALSE)   # Gomez & Gomez insecticide trial
crd$Insecticide <- factor(crd$Insecticide, levels = unique(crd$Insecticide))
sp <- read.csv(file.path(app_dir, "example_split.csv"), stringsAsFactors = FALSE)  # Gomez & Gomez nitrogen x variety
pairs_of <- function(lv) { p <- utils::combn(length(lv), 2); paste(lv[p[1, ]], "-", lv[p[2, ]]) }

# ---- Welch's ANOVA and Games-Howell (rstatix) ----
w <- star_welch(crd, "GrainYield", "Insecticide")
ref <- stats::oneway.test(GrainYield ~ Insecticide, crd, var.equal = FALSE)
check(same(w$statistic, ref$statistic) && same(w$df2, ref$parameter[2]) && same(w$p, ref$p.value), "Welch's ANOVA: F, df and p equal stats::oneway.test()")
if (have("rstatix")) {
  gh <- as.data.frame(rstatix::games_howell_test(crd, GrainYield ~ Insecticide))
  key <- paste(gh$group1, "-", gh$group2)
  i <- match(w$pairs$Comparison, key)
  check(!anyNA(i) && same(abs(w$pairs$Difference), abs(gh$estimate[i])) && same(w$pairs$P, gh$p.adj[i], tol = 1e-3),
        "Games-Howell: differences and p values equal rstatix::games_howell_test()")
}

# ---- Kruskal-Wallis and Dunn's test (dunn.test) ----
np <- star_nonparametric(crd, "GrainYield", "Insecticide", posthoc = "dunn_holm")
kw <- stats::kruskal.test(GrainYield ~ Insecticide, crd)
check(same(np$statistic, kw$statistic) && same(np$p, kw$p.value), "Kruskal-Wallis: H and p equal stats::kruskal.test()")
if (have("dunn.test")) {
  dt <- NULL; invisible(utils::capture.output(dt <- dunn.test::dunn.test(crd$GrainYield, crd$Insecticide, method = "none", altp = TRUE, kw = FALSE, table = FALSE)))
  # dunn.test labels pairs "B - A" in its own order; compare the unordered pair. Its Holm column skips the step-down's
  # running maximum, so the adjustment is checked against p.adjust() of its unadjusted p values instead.
  key <- vapply(strsplit(dt$comparisons, " - ", fixed = TRUE), function(z) paste(sort(z), collapse = "|"), "")
  mine <- vapply(strsplit(np$pairs$Comparison, " - ", fixed = TRUE), function(z) paste(sort(z), collapse = "|"), "")
  i <- match(mine, key)
  check(!anyNA(i) && same(abs(np$pairs$z), abs(dt$Z[i])) && same(np$pairs$P.adj, stats::p.adjust(dt$altP[i], "holm")),
        "Dunn's test (Holm): z and p equal dunn.test::dunn.test(), adjusted with p.adjust(\"holm\")")
}

# ---- trimmed means: Wilcox's one-way test and Yuen's test (WRS2) ----
tm <- star_trimmed(crd, "GrainYield", "Insecticide", B = 200)
if (have("WRS2")) {
  t1 <- WRS2::t1way(GrainYield ~ Insecticide, data = crd, tr = 0.2)
  check(same(tm$statistic, t1$test) && same(tm$df2, t1$df2) && same(tm$p, t1$p.value), "Trimmed-means ANOVA: F, df and p equal WRS2::t1way()")
  lv <- levels(crd$Insecticide)
  ok <- all(vapply(seq_len(nrow(tm$pairs)), function(j) {
    ab <- strsplit(tm$pairs$Comparison[j], " - ", fixed = TRUE)[[1]]
    sub <- droplevels(crd[crd$Insecticide %in% ab, ]); sub$Insecticide <- factor(sub$Insecticide, levels = ab)
    y <- WRS2::yuen(GrainYield ~ Insecticide, data = sub, tr = 0.2)
    same(abs(tm$pairs$t[j]), abs(y$test)) && same(tm$pairs$DF[j], y$df) && same(tm$pairs$P[j], y$p.value)
  }, NA))
  check(ok, "Yuen's test: t, df and p of every pair equal WRS2::yuen()")
}

# ---- split plot in RCBD: the two error strata fitted as aov(Error()) ----
r <- star_splitplot(sp, "Yield", "Nitrogen", "Variety", block = "Rep", design = "SplitRCBD")
spf <- sp; for (k in c("Rep", "Nitrogen", "Variety")) spf[[k]] <- factor(spf[[k]])
s <- summary(stats::aov(Yield ~ Nitrogen * Variety + Error(Rep / Nitrogen), data = spf))
whole <- s[["Error: Rep:Nitrogen"]][[1]]; within <- s[["Error: Within"]][[1]]
fv <- function(tab, src) tab[["F value"]][trimws(rownames(tab)) == src]
check(same(r$anova$F[r$anova$Source == "Nitrogen"], fv(whole, "Nitrogen")) && same(r$anova$F[r$anova$Source == "Variety"], fv(within, "Variety")) &&
        same(r$anova$F[r$anova$Source == "Nitrogen:Variety"], fv(within, "Nitrogen:Variety")),
      "Split plot RCBD: F of main plot, sub plot and interaction equal aov() with Error(Rep / Nitrogen)")
# (not compared with nlme::lme(): here Error(a) MS is below Error(b) MS, so REML sets the main-plot variance to zero and
# pools the two errors; the ANOVA of Gomez & Gomez keeps them apart, as aov(Error()) does)

# ---- RCBD with a missing plot: Yates's estimate equals the value that minimises the error sum of squares ----
rc <- subset(sp, Variety == unique(sp$Variety)[1])[c("Rep", "Nitrogen", "Yield")]
miss <- rc; miss$Yield[2] <- NA
r <- star_aov(miss, "Yield", "Nitrogen", block = "Rep", design = "RCBD")
sse <- function(v) { z <- miss; z$Yield[2] <- v; z$Rep <- factor(z$Rep); z$Nitrogen <- factor(z$Nitrogen); sum(stats::resid(stats::lm(Yield ~ Rep + Nitrogen, z))^2) }
opt <- stats::optimize(sse, range(rc$Yield) + c(-1, 1) * diff(range(rc$Yield)))
check(same(r$anova$SS[r$anova$Source == "Error"], opt$objective, tol = 1e-5) && r$anova$DF[r$anova$Source == "Error"] == (3 - 1) * (length(unique(rc$Nitrogen)) - 1) - 1,
      "RCBD missing plot: error SS is the minimum over the missing value (Yates), with one error df less")

if (length(skipped)) cat("\nskipped (package not installed):", paste(unique(skipped), collapse = ", "), "\n")
cat("\nAll reference checks passed.\n")
