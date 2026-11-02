## Internal helpers shared by star_aov() and star_splitplot().
## Note: zzz_compat.R masks data.frame() with stringsAsFactors = TRUE, so every call here sets it explicitly.

.star_check_columns <- function(data, cols, what) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols)) stop("Column '", missing_cols[1], "' not found.", call. = FALSE)
  if (anyDuplicated(cols)) stop(what, " must be different columns.", call. = FALSE)
  invisible(TRUE)
}

# LSD for few levels, Tukey otherwise, unless the user chose a test
.star_test_for <- function(test, n_levels) {
  if (test != "auto") test else if (n_levels <= 5) "LSD" else "HSD"
}

# One mean-separation test on a set of observations (agricolae), returned as a tidy data frame.
# `model_se = TRUE` replaces the per-group SE with the model-based sqrt(mse / n).
.star_compare_means <- function(y, tr, test, dfe, mse, alpha, model_se = FALSE) {
  tr <- droplevels(tr)
  unequal <- length(unique(table(tr[!is.na(y)]))) > 1
  cmp <- switch(test,
    LSD = agricolae::LSD.test(y, tr, dfe, mse, alpha, console = FALSE),
    bonferroni = agricolae::LSD.test(y, tr, dfe, mse, alpha, p.adj = "bonferroni", console = FALSE),
    HSD = agricolae::HSD.test(y, tr, dfe, mse, alpha, unbalanced = unequal, console = FALSE),
    duncan = agricolae::duncan.test(y, tr, dfe, mse, alpha, console = FALSE),
    SNK = agricolae::SNK.test(y, tr, dfe, mse, alpha, console = FALSE),
    scheffe = agricolae::scheffe.test(y, tr, dfe, mse, Fc = NA_real_, alpha = alpha, console = FALSE),
    stop("Unknown mean comparison test: ", test, call. = FALSE))
  m <- cmp$means
  out <- data.frame(
    Treatment = rownames(m), Mean = m$y, N = m$r, SE = m$std / sqrt(m$r),
    Group = trimws(as.character(cmp$groups[rownames(m), "groups"])), stringsAsFactors = FALSE)
  out <- out[match(levels(tr), out$Treatment), ]
  if (model_se) out$SE <- sqrt(mse / out$N)
  rownames(out) <- NULL
  out
}

# Mean, n and SE for every combination of two factors (first factor varies slowest)
.star_cell_means <- function(y, d, f1, f2) {
  ok <- !is.na(y)
  groups <- interaction(d[[f1]], d[[f2]], lex.order = TRUE)
  v <- split(y[ok], groups[ok])
  cells <- expand.grid(b = levels(d[[f2]]), a = levels(d[[f1]]), stringsAsFactors = FALSE)[, 2:1]
  names(cells) <- c(f1, f2)
  cells$Mean <- vapply(v, mean, 0, USE.NAMES = FALSE)
  cells$N <- lengths(v, use.names = FALSE)
  cells$SE <- vapply(v, stats::sd, 0, USE.NAMES = FALSE) / sqrt(cells$N)
  cells
}

# Compare `cmp_f` within each level of `at_f`; `dfe` / `mse` give the error term used.
.star_simple_effect <- function(y, d, cmp_f, at_f, test, dfe, mse, alpha, model_se = FALSE) {
  sm <- do.call(rbind, lapply(levels(d[[at_f]]), \(lv) {
    idx <- d[[at_f]] == lv & !is.na(y)
    m <- .star_compare_means(y[idx], droplevels(d[[cmp_f]][idx]), test, dfe, mse, alpha, model_se)
    out <- data.frame(lv, m, stringsAsFactors = FALSE)
    names(out)[1:2] <- c(at_f, cmp_f)
    out
  }))
  rownames(sm) <- NULL
  structure(sm, compare = cmp_f, at = at_f, test = test)
}

# Overall means of each factor. `dfe` / `mse` are named by factor (the error term used for that factor).
# Letters are dropped when the interaction is significant (main effects are then not compared).
.star_main_means <- function(y, d, factors, test, dfe, mse, alpha, interaction_sig, model_se = FALSE) {
  tests <- vapply(factors, \(f) .star_test_for(test, nlevels(d[[f]])), "")
  ok <- !is.na(y)
  means <- Map(\(f, tst) {
    m <- .star_compare_means(y[ok], d[[f]][ok], tst, dfe[[f]], mse[[f]], alpha, model_se)
    if (interaction_sig) m$Group <- NA_character_
    m
  }, factors, tests)
  list(means = means, tests = tests)
}

# Simple effects of a two-factor layout: each factor compared within every level of the other.
.star_simple_effects <- function(y, d, factors, test, dfe, mse, alpha, model_se = FALSE) {
  out <- Map(\(cmp_f, at_f) {
    tst <- .star_test_for(test, nlevels(d[[cmp_f]]))
    .star_simple_effect(y, d, cmp_f, at_f, tst, dfe[[cmp_f]], mse[[cmp_f]], alpha, model_se)
  }, factors, rev(factors))
  names(out) <- paste(factors, "within", rev(factors))
  out
}

.star_best <- function(m) {
  sprintf("%s (%s)", m$Treatment[which.max(m$Mean)], format(round(max(m$Mean), 2), nsmall = 2))
}

.star_stat <- function(anova, nm) {
  i <- anova$Source == nm
  sprintf("F = %.2f, p %s", anova$F[i], .star_fmt_p(anova$P[i]))
}
