## New API (not part of the original STAR): guidance on which test suits the data, Welch's ANOVA with the Games-Howell
## comparisons, and Dunn's comparisons after a Kruskal-Wallis test. The guidance is advice only: the app shows it with a
## button to switch, and the analyst decides.
##
## The order of the checks: normality of the residuals (or of each group, for two groups), with a mixed rule for large
## samples (30 or more per group with |skewness| <= 1 and |excess kurtosis| <= 2 is robust by the central limit theorem)
## and a warning for small ones (fewer than 15 per group: the test has little power); outliers are checked before a rank
## test is suggested. The standard ANOVA and the pooled t-test are the defaults of the app; after the run the guidance
## suggests Welch's ANOVA (CRD, one factor) or Welch's t-test when the variances differ or the group sizes are unequal,
## because Welch's test loses almost nothing when the variances are equal and the standard F test can be badly off when
## they are not (Delacre et al. 2017, 2019; Zimmerman 2004). In an RCBD, where no Welch version exists, unequal variances
## lead to a transformation or the Friedman test.

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

## a recommendation. `light` is the traffic light of the assumption checks: "green" (met: the parametric analysis can
## be used fully), "yellow" (a mild departure the analysis is robust to, or a check with little power) or "red" (a
## clear violation or outliers: another method is advised). `options` are the methods a button can switch to, each a
## list(id, label, goal), the first being the recommended one; `checks` holds the light of each check for display.
.star_advice <- function(light, observed, advice, options = list(), checks = NULL) {
  list(light = light, observed = observed, advice = advice, options = options, checks = checks,
       switch_to = if (length(options)) options[[1]]$id, switch_label = if (length(options)) options[[1]]$label)
}
.star_option <- function(id, label, goal = NULL) list(id = id, label = label, goal = goal)
.star_worst <- function(lights) { lv <- c("green", "yellow", "red"); l <- intersect(lights, lv); if (!length(l)) "green" else lv[max(match(l, lv))] }
.star_goal_rank <- "To compare ranks or medians (common practice)"
.star_goal_means <- "To compare means without relying on the normal curve"

## the light of each assumption check of a star_aov() result: normality of the residuals (yellow: the mixed rule for
## large samples), equal variances (yellow: unequal but the largest/smallest variance ratio is at most 4 with equal
## group sizes, a case the F test tolerates) and outliers (yellow: suspect residuals only, 2 < |r| <= 3)
star_assumption_lights <- function(r) {
  dg <- r$diagnostics
  if (is.null(dg)) return(NULL)
  cells <- table(interaction(r$data[r$factors], drop = TRUE)[!is.na(r$data[[r$response]])])
  n_min <- min(cells); balanced <- length(unique(as.integer(cells))) == 1
  sh <- .star_shape(dg$residuals$Residual)
  robust <- n_min >= 30 && isTRUE(abs(sh[["skew"]]) <= 1 && abs(sh[["kurt"]]) <= 2)
  nl <- if (is.na(dg$normality$ok)) NA_character_ else if (isTRUE(dg$normality$ok)) "green" else if (robust) "yellow" else "red"
  vl <- if (is.null(dg$homogeneity) || is.na(dg$homogeneity$ok)) NA_character_ else if (isTRUE(dg$homogeneity$ok)) "green"
        else if (balanced && is.finite(dg$homogeneity$ratio) && dg$homogeneity$ratio <= 4) "yellow" else "red"
  ol <- if ((dg$n_outliers %||% 0) > 0) "red" else if ((dg$n_suspect %||% 0) > 0) "yellow" else "green"
  data.frame(Check = c("normality", "variance", "outliers"), Light = c(nl, vl, ol), stringsAsFactors = FALSE)
}

## guidance for a one-factor CRD or RCBD analysed with star_aov(). NULL when there is nothing to check.
star_recommend_aov <- function(r) {
  dg <- r$diagnostics
  if (is.null(dg) || !r$design %in% c("CRD", "RCBD") || length(r$factors) != 1) return(NULL)
  a <- r$alpha; f <- r$factors; crd <- r$design == "CRD"
  lights <- star_assumption_lights(r); L <- stats::setNames(lights$Light, lights$Check)
  n_min <- min(table(r$data[[f]][!is.na(r$data[[r$response]])]))
  res <- dg$residuals$Residual
  sh <- .star_shape(res)
  sw_p <- dg$normality$p; lev_p <- dg$homogeneity$p
  out <- dg$residuals$Flag %in% "outlier"
  sw_wo <- if (isFALSE(dg$normality$ok) && any(out) && sum(!out) >= 3) tryCatch(stats::shapiro.test(res[!out])$p.value, error = \(e) NA_real_) else NA_real_
  y <- r$data[[r$original_response %||% r$response]]
  positive <- is.numeric(y) && all(y[!is.na(y)] > 0)
  tf <- if (positive && isTRUE(sh[["skew"]] > 0.5)) " A log or square-root transformation (Advanced options) may also make the residuals normal." else ""
  rank_name <- if (crd) "Kruskal-Wallis" else "Friedman"
  shape <- if (all(is.finite(sh))) sprintf(" Skewness = %.2f, excess kurtosis = %.2f.", sh[["skew"]], sh[["kurt"]]) else ""
  small <- if (n_min < 15) sprintf(" With %d observations per treatment the normality test has little power, so also look at the residual plot.", n_min) else ""
  few <- if (n_min < 10) sprintf(" With %d per treatment, trimmed means and bootstrap intervals are rough; treat them with care.", n_min) else ""
  lev_txt <- sprintf("Levene p %s%s", .star_fmt_p(lev_p), if (is.finite(dg$homogeneity$ratio)) sprintf(", largest/smallest variance = %.1f", dg$homogeneity$ratio) else "")
  goal_options <- c(list(.star_option("rank", sprintf("Use %s", rank_name), .star_goal_rank)),
                    if (crd) list(.star_option("trimmed", "Use trimmed means (20%) with bootstrap CI", .star_goal_means)))

  # red: a clear violation of normality, or outliers
  if (identical(L[["normality"]], "red") || identical(L[["outliers"]], "red")) {
    obs <- c(
      if (identical(L[["normality"]], "red")) sprintf("The residuals depart from a normal distribution (Shapiro-Wilk p %s), which can make the F test and letters unreliable.%s", .star_fmt_p(sw_p), shape),
      if (any(out)) sprintf("%d outlier(s) (|studentized residual| > 3)%s.", sum(out), if (isTRUE(sw_wo >= a)) sprintf("; without them the residuals look normal (p %s)", .star_fmt_p(sw_wo)) else ""))
    adv <- paste0(if (any(out)) "Check the flagged value(s) on the Outliers & assumptions tab for a data-entry or measurement error first. " else "",
                  if (nzchar(tf)) paste0(trimws(tf), " ") else "", if (crd && nzchar(few)) paste0(trimws(few), " ") else "",
                  "If the values are genuine, choose by what you want to report", if (crd) ":" else sprintf(": the %s test (rank-based).", rank_name))
    return(.star_advice("red", paste(obs, collapse = " "), adv, goal_options, lights))
  }
  note <- if (identical(L[["normality"]], "yellow")) sprintf(" The residuals depart from normal (Shapiro-Wilk p %s), but with %d or more per treatment and a moderate shape (%s) the means-based analysis is robust (central limit theorem).",
                                                         .star_fmt_p(sw_p), n_min, trimws(shape)) else ""
  norm_txt <- if (identical(L[["normality"]], "green")) sprintf("Residuals are consistent with a normal distribution (Shapiro-Wilk p %s).", .star_fmt_p(sw_p)) else ""
  var_txt <- sprintf(" The variances %s (%s).", if (isFALSE(dg$homogeneity$ok)) "differ among the treatments" else "look similar", lev_txt)
  # CRD: the standard ANOVA is the default; Welch's ANOVA is suggested when the variances differ or the group sizes are unequal
  if (crd) {
    cells <- table(r$data[[f]][!is.na(r$data[[r$response]])]); balanced <- length(unique(as.integer(cells))) == 1
    ratio <- dg$homogeneity$ratio
    welch_better <- L[["variance"]] %in% c("yellow", "red") || (!balanced && isTRUE(ratio >= 2))
    light <- .star_worst(c(L[["normality"]], L[["variance"]], L[["outliers"]], if (n_min < 15) "yellow"))
    if (welch_better)
      return(.star_advice(light, paste0(norm_txt, note, var_txt, if (!balanced) " The numbers of observations differ among the treatments." else "", small),
        paste("Welch's ANOVA does not assume equal variances, which the standard F test needs, especially when the group sizes are unequal.",
              "Its means are compared with the Games-Howell test."),
        list(.star_option("welch", "Switch to Welch's ANOVA (recommended)")), lights))
    return(.star_advice(light, paste0(norm_txt, note, var_txt, small), "The standard ANOVA suits these data.", checks = lights))
  }
  # RCBD: no Welch version; clearly unequal variances call for a transformation or Friedman
  if (identical(L[["variance"]], "red"))
    return(.star_advice("red", sprintf("The variances differ among the treatments (%s).", lev_txt),
      sprintf("There is no Welch version for blocks. A transformation (Advanced options) often makes the variances equal%s; otherwise the Friedman test (rank-based) can be used.", if (positive) ", for example log for yields or counts" else ""),
      list(.star_option("rank", "Use Friedman", .star_goal_rank)), lights))
  .star_advice(.star_worst(c(L[["normality"]], L[["variance"]], L[["outliers"]], if (n_min < 15) "yellow")),
    paste0(norm_txt, note, var_txt, small),
    if (identical(L[["variance"]], "yellow")) "The variances differ only moderately with equal group sizes, which the ANOVA tolerates; the ANOVA can be used." else "The ANOVA suits these data.",
    checks = lights)
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
  robust <- n_min >= 30 && all(vapply(shapes, \(s) isTRUE(abs(s[["skew"]]) <= 1 && abs(s[["kurt"]]) <= 2), NA))
  beyond <- function(v, k) { q <- stats::quantile(v, c(.25, .75)); h <- k * diff(q); sum(v < q[1] - h | v > q[2] + h) }
  iqr_out <- sum(vapply(vals, beyond, 0, k = 1.5)); far_out <- sum(vapply(vals, beyond, 0, k = 3))
  nl <- if (!any(!is.na(nrm$P))) NA_character_ else if (!any(bad)) "green" else if (robust) "yellow" else "red"
  ol <- if (far_out > 0) "red" else if (iqr_out > 0) "yellow" else "green"
  lev <- if (r$type == "independent") .star_levene(d$Value, d$Group) else c(F = NA_real_, p = NA_real_)
  vl <- if (is.na(lev[["p"]])) NA_character_ else if (lev[["p"]] >= a) "green" else "yellow"   # Welch's t-test handles unequal variances
  lights <- data.frame(Check = c("normality", "variance", "outliers"), Light = c(nl, vl, ol), stringsAsFactors = FALSE)
  what <- if (r$type == "paired") "The differences" else if (r$type == "one") "The values" else sprintf("The values of %s", paste(nrm$Data[bad], collapse = " and "))
  small <- if (n_min < 15) sprintf(" With %d values%s the normality test has little power, so also look at the plot.", n_min, if (r$type == "independent") " in the smaller group" else "") else ""
  few <- if (n_min < 10) sprintf(" With %d values%s, trimmed means and bootstrap intervals are rough; treat them with care.", n_min, if (r$type == "independent") " in the smaller group" else "") else ""
  wname <- if (r$type == "independent") "Wilcoxon rank-sum (Mann-Whitney)" else "Wilcoxon signed-rank"
  if (identical(nl, "red") || identical(ol, "red"))
    return(.star_advice("red",
      paste0(if (identical(nl, "red")) sprintf("%s depart from a normal distribution (Shapiro-Wilk p %s). ", what, .star_fmt_p(min(nrm$P[bad]))) else "",
             if (far_out) sprintf("%d value(s) lie beyond 3 x IQR (extreme). ", far_out) else if (iqr_out) sprintf("%d value(s) lie beyond 1.5 x IQR. ", iqr_out) else ""),
      paste0(if (iqr_out) "Check the values beyond the quartiles for errors first. " else "", if (nzchar(few)) paste0(trimws(few), " ") else "", "If the values are genuine, choose by what you want to report:"),
      list(.star_option("wilcox", sprintf("Use the %s test", wname), .star_goal_rank),
           .star_option("trimmed", "Use trimmed means (20%) with bootstrap CI", .star_goal_means)), lights))
  obs <- paste0(if (identical(nl, "yellow")) sprintf("The data depart from normal, but with %d or more values per group and a moderate shape the t-test is robust (central limit theorem).", n_min)
                else "The data are consistent with a normal distribution.",
                if (iqr_out) sprintf(" %d value(s) lie beyond 1.5 x IQR.", iqr_out) else "",
                if (!is.na(lev[["p"]])) sprintf(" The variances %s (Levene p %s).", if (lev[["p"]] < a) "differ" else "look similar", .star_fmt_p(lev[["p"]])) else "", small)
  light <- .star_worst(c(nl, ol, vl, if (n_min < 15) "yellow"))
  vr <- if (r$type == "independent") { v <- vapply(vals, stats::var, 0); max(v) / min(v) } else NA_real_
  welch_better <- r$type == "independent" && (isTRUE(lev[["p"]] < a) || (length(unique(lengths(vals))) > 1 && isTRUE(vr >= 2)))
  if (r$type == "independent" && isTRUE(r$var_equal) && welch_better)
    return(.star_advice(light, paste("The pooled (Student's) t-test is chosen.", obs, if (length(unique(lengths(vals))) > 1) " The group sizes differ." else ""),
      "Welch's t-test does not assume equal variances, which the pooled test needs, especially when the group sizes are unequal; it gives almost the same result when the variances are equal.",
      list(.star_option("welch_var", "Switch to Welch (recommended)")), lights))
  .star_advice(light, obs, "The t-test suits these data.", checks = lights)
}
