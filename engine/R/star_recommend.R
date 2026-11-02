## New API (not part of the original STAR): guidance on which test suits the data, Welch's ANOVA with the Games-Howell
## comparisons, and Dunn's comparisons after a Kruskal-Wallis test. The guidance is advice only: the app shows it with a
## button to switch, and the analyst decides.
##
## The order of the checks: normality of the residuals (or of each group, for two groups), with a mixed rule for large
## samples (30 or more per group with |skewness| <= 1 and |excess kurtosis| <= 2 is robust by the central limit theorem)
## and a warning for small ones (fewer than 15 per group: the test has little power); outliers are checked before a rank
## test is suggested; then equal variances (Levene, Brown-Forsythe). Unequal variances lead to Welch's ANOVA (CRD) or to
## a transformation or the Friedman test (RCBD, where no Welch version exists), not straight to a rank test.

## sample skewness (g1) and excess kurtosis (g2)
.star_shape <- function(x) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 4 || stats::sd(x) == 0) return(c(skew = NA_real_, kurt = NA_real_))
  m <- x - mean(x); s2 <- mean(m^2)
  c(skew = mean(m^3) / s2^1.5, kurt = mean(m^4) / s2^2 - 3)
}

## Levene's test with deviations from the group medians (Brown-Forsythe)
.star_levene <- function(y, g) {
  g <- droplevels(as.factor(g))
  if (nlevels(g) < 2 || any(table(g) < 2)) return(c(F = NA_real_, p = NA_real_))
  z <- abs(y - stats::ave(y, g, FUN = stats::median))
  a <- stats::anova(stats::lm(z ~ g))
  c(F = a[["F value"]][1], p = a[["Pr(>F)"]][1])
}

## letters from a matrix of pairwise p values (rows and columns in the order of `lv`), highest value first
.star_letters <- function(lv, values, p, alpha) {
  o <- order(values, decreasing = TRUE)
  g <- agricolae::orderPvalue(lv[o], values[o], alpha, p[o, o, drop = FALSE], console = FALSE)
  stats::setNames(trimws(as.character(g[lv, "groups"])), lv)
}

## Welch's one-way ANOVA (no equal-variance assumption) with Games-Howell comparisons; CRD, one treatment factor
star_welch <- function(data, response, treatment, alpha = 0.05) {
  if (length(treatment) != 1) stop("Welch's ANOVA needs exactly one treatment factor.", call. = FALSE)
  d <- star_prepare_data(data, response, treatment)
  d <- d[!is.na(d[[response]]), , drop = FALSE]
  y <- d[[response]]; g <- droplevels(d[[treatment]]); lv <- levels(g)
  if (length(lv) < 2) stop("The treatment needs at least two levels.", call. = FALSE)
  if (any(table(g) < 2)) stop("Welch's ANOVA needs at least two observations of every treatment.", call. = FALSE)
  ow <- stats::oneway.test(y ~ g, var.equal = FALSE)
  n <- as.numeric(table(g)); m <- as.numeric(tapply(y, g, mean)); v <- as.numeric(tapply(y, g, stats::var)); k <- length(lv)
  pairs <- utils::combn(k, 2)
  gh <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(j) {
    a <- pairs[1, j]; b <- pairs[2, j]
    se2 <- v[a] / n[a] + v[b] / n[b]
    df <- se2^2 / ((v[a] / n[a])^2 / (n[a] - 1) + (v[b] / n[b])^2 / (n[b] - 1))
    t <- (m[a] - m[b]) / sqrt(se2)
    data.frame(Comparison = paste(lv[a], "-", lv[b]), Difference = m[a] - m[b], SE = sqrt(se2), t = t, DF = df,
               P = stats::ptukey(abs(t) * sqrt(2), k, df, lower.tail = FALSE), stringsAsFactors = FALSE)
  }))
  P <- matrix(1, k, k, dimnames = list(lv, lv))
  P[t(pairs)] <- gh$P; P[t(pairs[2:1, , drop = FALSE])] <- gh$P
  sig <- ow$p.value < alpha
  means <- data.frame(Treatment = lv, Mean = m, SD = sqrt(v), N = n, SE = sqrt(v / n),
                      Group = if (sig) unname(.star_letters(lv, m, P, alpha)) else NA_character_, stringsAsFactors = FALSE)
  res <- list(name = "Welch's ANOVA", statistic = unname(ow$statistic), df1 = unname(ow$parameter[1]), df2 = unname(ow$parameter[2]),
              p = ow$p.value, significant = sig, means = means, pairs = gh, treatment = treatment, response = response, alpha = alpha)
  res$summary <- sprintf("Welch's ANOVA: F(%d, %.2f) = %.2f, p %s. %s", res$df1, res$df2, res$statistic, .star_fmt_p(res$p),
                         if (sig) sprintf("The %s means differ (letters: Games-Howell test).", treatment) else sprintf("No significant difference among the %s means.", treatment))
  p_apa <- if (res$p < 0.001) "p < .001" else paste("p =", sub("^0\\.", ".", formatC(res$p, format = "f", digits = 3)))
  res$apa <- sprintf("A Welch's ANOVA showed that %s %s among the levels of %s, F(%d, %.2f) = %.2f, %s.", response,
                     if (sig) "differed significantly" else "did not differ significantly", treatment, res$df1, res$df2, res$statistic, p_apa)
  structure(res, class = "star_welch")
}

## Dunn's comparisons of mean ranks after a Kruskal-Wallis test (ties corrected), p values adjusted with Holm or Bonferroni
.star_dunn <- function(y, g, adjust = c("holm", "bonferroni"), alpha = 0.05) {
  adjust <- match.arg(adjust)
  g <- droplevels(as.factor(g)); lv <- levels(g); k <- length(lv); N <- length(y)
  r <- rank(y); rb <- as.numeric(tapply(r, g, mean)); n <- as.numeric(table(g))
  tie <- sum(table(y)^3 - table(y)) / (12 * (N - 1))
  pairs <- utils::combn(k, 2)
  z <- apply(pairs, 2, function(ab) (rb[ab[1]] - rb[ab[2]]) / sqrt((N * (N + 1) / 12 - tie) * (1 / n[ab[1]] + 1 / n[ab[2]])))
  p <- stats::p.adjust(2 * stats::pnorm(-abs(z)), adjust)
  P <- matrix(1, k, k, dimnames = list(lv, lv))
  P[t(pairs)] <- p; P[t(pairs[2:1, , drop = FALSE])] <- p
  list(pairs = data.frame(Comparison = paste(lv[pairs[1, ]], "-", lv[pairs[2, ]]), z = z, P.adj = p, stringsAsFactors = FALSE),
       letters = .star_letters(lv, rb, P, alpha), adjust = adjust)
}

## a recommendation: what was observed, what is advised, and which method a switch button would choose (NULL: none)
.star_advice <- function(level, observed, advice, switch_to = NULL, switch_label = NULL, stay_label = NULL) {
  list(level = level, observed = observed, advice = advice, switch_to = switch_to, switch_label = switch_label, stay_label = stay_label)
}

## guidance for a one-factor CRD or RCBD analysed with star_aov(); `y` and `g` are the response and the treatment,
## `outlier_rows` the rows the diagnostics flagged. NULL when there is nothing to check.
star_recommend_aov <- function(r) {
  dg <- r$diagnostics
  if (is.null(dg) || !r$design %in% c("CRD", "RCBD") || length(r$factors) != 1) return(NULL)
  a <- r$alpha; f <- r$factors
  n_min <- min(table(r$data[[f]][!is.na(r$data[[r$response]])]))
  res <- dg$residuals$Residual
  sh <- .star_shape(res)
  sw_p <- dg$normality$p; lev_p <- dg$homogeneity$p
  norm_fail <- isFALSE(dg$normality$ok); var_fail <- isFALSE(dg$homogeneity$ok)
  clt <- norm_fail && n_min >= 30 && isTRUE(abs(sh[["skew"]]) <= 1 && abs(sh[["kurt"]]) <= 2)
  out <- dg$residuals$Flag %in% "outlier"
  sw_wo <- if (norm_fail && any(out) && sum(!out) >= 3) tryCatch(stats::shapiro.test(res[!out])$p.value, error = \(e) NA_real_) else NA_real_
  y <- r$data[[r$original_response %||% r$response]]
  positive <- is.numeric(y) && all(y[!is.na(y)] > 0)
  tf <- if (positive && isTRUE(sh[["skew"]] > 0.5)) " A log or square-root transformation (Advanced options) may also make the residuals normal." else ""
  rank_name <- if (r$design == "CRD") "Kruskal-Wallis" else "Friedman"
  shape <- if (all(is.finite(sh))) sprintf(" Skewness = %.2f, excess kurtosis = %.2f.", sh[["skew"]], sh[["kurt"]]) else ""
  small <- if (n_min < 15) sprintf(" With %d observations per treatment the normality test has little power, so also look at the residual plot.", n_min) else ""

  if (norm_fail && !clt) {
    if (isTRUE(sw_wo >= a))
      return(.star_advice("warn",
        sprintf("The residuals depart from a normal distribution (Shapiro-Wilk p %s), but without the %d outlier(s) they look normal (p %s).", .star_fmt_p(sw_p), sum(out), .star_fmt_p(sw_wo)),
        sprintf("Check the outlier(s) on the Outliers & assumptions tab for a data-entry or measurement error first. If the values are genuine, the %s test (rank-based) does not depend on the distribution.%s", rank_name, tf),
        "rank", sprintf("Switch to %s", rank_name), "Continue with ANOVA"))
    return(.star_advice("warn",
      sprintf("The residuals depart from a normal distribution (Shapiro-Wilk p %s), which can make the ANOVA F test and letters unreliable.%s", .star_fmt_p(sw_p), shape),
      sprintf("Consider the %s test (rank-based), which does not assume a normal distribution.%s", rank_name, tf),
      "rank", sprintf("Switch to %s (recommended)", rank_name), "Continue with ANOVA"))
  }
  if (var_fail) {
    obs <- sprintf("The variances differ among the treatments (Levene p %s%s).", .star_fmt_p(lev_p),
                   if (is.finite(dg$homogeneity$ratio)) sprintf("; largest/smallest = %.1f", dg$homogeneity$ratio) else "")
    if (r$design == "CRD")
      return(.star_advice("warn", obs, "Consider Welch's ANOVA, which does not assume equal variances; its means are compared with the Games-Howell test.",
                          "welch", "Switch to Welch's ANOVA (recommended)", "Continue with standard ANOVA"))
    return(.star_advice("warn", obs,
      sprintf("There is no Welch version for blocks. A transformation (Advanced options) often makes the variances equal%s; otherwise the Friedman test (rank-based) can be used.", if (positive) ", for example log for yields or counts" else ""),
      "rank", "Switch to Friedman", "Continue with ANOVA"))
  }
  note <- if (clt) sprintf(" The residuals depart from normal (Shapiro-Wilk p %s), but with %d or more per treatment and a moderate shape (%s) the ANOVA is robust (central limit theorem).",
                           .star_fmt_p(sw_p), n_min, trimws(shape)) else ""
  .star_advice(if (clt || n_min < 15) "note" else "ok",
    paste0(sprintf("Residuals are consistent with a normal distribution (Shapiro-Wilk p %s) and the variances are similar (Levene p %s).", .star_fmt_p(sw_p), .star_fmt_p(lev_p)),
           note, small),
    "The standard ANOVA suits these data.")
}

## guidance for a t-test result of star_ttest()
star_recommend_tt <- function(r) {
  a <- r$alpha; d <- r$data
  vals <- switch(r$type,
    independent = split(d$Value, d$Group),
    paired = { w <- stats::reshape(d[c("Pair", "Group", "Value")], idvar = "Pair", timevar = "Group", direction = "wide"); list(Differences = w[[2]] - w[[3]]) },
    one = list(d$Value))
  n_min <- min(lengths(vals))
  nrm <- r$normality; bad <- !is.na(nrm$P) & nrm$P < a
  shapes <- lapply(vals, .star_shape)
  clt <- any(bad) && n_min >= 30 && all(vapply(shapes, \(s) isTRUE(abs(s[["skew"]]) <= 1 && abs(s[["kurt"]]) <= 2), NA))
  iqr_out <- sum(vapply(vals, \(v) { q <- stats::quantile(v, c(.25, .75)); h <- 1.5 * diff(q); sum(v < q[1] - h | v > q[2] + h) }, 0))
  what <- if (r$type == "paired") "The differences" else if (r$type == "one") "The values" else sprintf("The values of %s", paste(nrm$Data[bad], collapse = " and "))
  small <- if (n_min < 15) sprintf(" With %d values%s the normality test has little power, so also look at the plot.", n_min, if (r$type == "independent") " in the smaller group" else "") else ""
  wname <- if (r$type == "independent") "Wilcoxon rank-sum (Mann-Whitney)" else "Wilcoxon signed-rank"
  if (any(bad) && !clt)
    return(.star_advice("warn",
      sprintf("%s depart from a normal distribution (Shapiro-Wilk p %s)%s.", what, .star_fmt_p(min(nrm$P[bad])),
              if (iqr_out) sprintf("; %d value(s) lie beyond 1.5 x IQR, so check them for errors first", iqr_out) else ""),
      sprintf("Consider the %s test, which does not assume a normal distribution.", wname),
      "wilcox", "Switch to Wilcoxon (recommended)", "Continue with the t-test"))
  if (r$type == "independent") {
    lev <- .star_levene(d$Value, d$Group)
    if (isTRUE(lev[["p"]] < a) && isTRUE(r$var_equal))
      return(.star_advice("warn", sprintf("The variances of the two groups differ (Levene p %s).", .star_fmt_p(lev[["p"]])),
                          "Use Welch's t-test, which does not assume equal variances.", "welch_var", "Switch to Welch (recommended)", "Keep the pooled t-test"))
  }
  .star_advice(if (clt || n_min < 15) "note" else "ok",
    paste0(if (clt) sprintf("The data depart from normal, but with %d or more values per group and a moderate shape the t-test is robust (central limit theorem).", n_min)
           else "The data are consistent with a normal distribution.", small),
    "The t-test suits these data.")
}
