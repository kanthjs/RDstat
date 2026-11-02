## New API (not part of the original STAR): structured results for t tests. The original CompareMeans() prints text;
## this returns data frames so that an app can render tables, draw plots and export to Excel.
##   type = "independent" : `response` compared between two levels of `group` (long format). With more than two levels,
##                          `levels` names the two to compare. Both the pooled (Student) and the Welch (Satterthwaite)
##                          tests are returned, as in STAR; `var_equal` chooses the one used for the conclusion.
##   type = "paired"      : two numeric columns `x` and `y` measured on the same units (one row per unit, wide format);
##                          the test is on the differences x - y, using rows where both are present.
##   type = "one"         : the mean of `response` compared with the value `mu`.
## alternative is "two.sided", "greater" or "less" (for two samples: first group / x minus second group / y).
## Confidence intervals are at level 1 - alpha.

star_ttest <- function(data, type = c("independent", "paired", "one"), response = NULL, group = NULL, levels = NULL,
                       x = NULL, y = NULL, mu = 0, var_equal = FALSE,
                       alternative = c("two.sided", "greater", "less"), alpha = 0.05) {
  type <- match.arg(type)
  alternative <- match.arg(alternative)
  if (!is.data.frame(data)) stop("'data' must be a data frame.", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1 || !(alpha > 0 && alpha < 0.5)) stop("'alpha' must be a number between 0 and 0.5.", call. = FALSE)
  conf <- 1 - alpha
  rows <- seq_len(nrow(data))

  if (type == "paired") {
    if (is.null(x) || is.null(y)) stop("A paired t test needs two numeric columns (the two measurements on each unit).", call. = FALSE)
    d <- .star_numeric_frame(data, c(x, y))
    notes <- attr(d, "notes")
    ok <- stats::complete.cases(d)
    if (any(!ok)) notes <- c(notes, sprintf("%d row(s) without both '%s' and '%s' were left out (e.g. row %d).", sum(!ok), x, y, which(!ok)[1]))
    a <- d[[x]][ok]; b <- d[[y]][ok]
    if (length(a) < 2) stop("A paired t test needs at least two complete pairs.", call. = FALSE)
    dif <- a - b
    if (stats::sd(dif) == 0) stop("All differences are equal, so the paired t test cannot be computed.", call. = FALSE)
    tt <- stats::t.test(a, b, paired = TRUE, alternative = alternative, conf.level = conf)
    groups <- .star_tt_desc(list(a, b, dif), c(x, y, paste(x, "-", y)))
    test <- .star_tt_row("Paired t test", tt, mean(dif), stats::sd(dif) / sqrt(length(dif)))
    d_eff <- mean(dif) / stats::sd(dif)
    normality <- .star_tt_normal(list(dif), paste("Differences", x, "-", y))
    long <- data.frame(Group = factor(rep(c(x, y), each = length(a)), levels = c(x, y)), Value = c(a, b),
                       Pair = rep(seq_along(a), 2), Row = rep(rows[ok], 2), stringsAsFactors = FALSE)
    labels <- c(x, y); pair_r <- stats::cor(a, b)
    variance <- NULL; chosen <- 1L
  } else if (type == "independent") {
    if (is.null(response) || is.null(group)) stop("An independent t test needs a numeric response and a group column.", call. = FALSE)
    .star_check_columns(data, c(response, group), "The response and the group")
    d <- .star_numeric_frame(data, response)
    notes <- attr(d, "notes")
    g <- data[[group]]
    g <- ifelse(star_is_blank(g), NA, star_squash_space(as.character(g)))
    present <- unique(g[!is.na(g)])
    lv <- star_level_order(present)
    if (is.null(levels)) {
      if (length(lv) != 2) stop(sprintf("The group column '%s' has %d groups; choose the two to compare (or use analysis of variance for more than two).", group, length(lv)), call. = FALSE)
      levels <- lv
    }
    levels <- as.character(levels)
    if (length(levels) != 2 || levels[1] == levels[2]) stop("Choose exactly two different groups to compare.", call. = FALSE)
    if (!all(levels %in% present)) stop("Group '", setdiff(levels, present)[1], "' is not in the column '", group, "'.", call. = FALSE)
    v <- d[[response]]
    miss <- is.na(v) & g %in% levels
    if (any(miss)) notes <- c(notes, sprintf("%d row(s) without a value of '%s' were left out (e.g. row %d).", sum(miss), response, which(miss)[1]))
    if (length(lv) > 2) notes <- c(notes, sprintf("Only groups '%s' and '%s' of '%s' are compared; the other %d group(s) were left out.", levels[1], levels[2], group, length(lv) - 2))
    if (any(is.na(g))) notes <- c(notes, sprintf("%d row(s) without a group were left out.", sum(is.na(g))))
    va <- v[g %in% levels[1] & !is.na(v)]; vb <- v[g %in% levels[2] & !is.na(v)]
    if (length(va) < 2 || length(vb) < 2) stop("Each group needs at least two observations.", call. = FALSE)
    if (stats::sd(va) == 0 && stats::sd(vb) == 0) stop("Both groups are constant, so the t test cannot be computed.", call. = FALSE)
    t_pool <- stats::t.test(va, vb, var.equal = TRUE, alternative = alternative, conf.level = conf)
    t_welch <- stats::t.test(va, vb, var.equal = FALSE, alternative = alternative, conf.level = conf)
    na <- length(va); nb <- length(vb)
    sp <- sqrt(((na - 1) * stats::var(va) + (nb - 1) * stats::var(vb)) / (na + nb - 2))
    diff <- mean(va) - mean(vb)
    test <- rbind(.star_tt_row("Equal variances (pooled, Student)", t_pool, diff, sp * sqrt(1 / na + 1 / nb)),
                  .star_tt_row("Unequal variances (Welch)", t_welch, diff, sqrt(stats::var(va) / na + stats::var(vb) / nb)))
    chosen <- if (isTRUE(var_equal)) 1L else 2L
    groups <- .star_tt_desc(list(va, vb), levels)
    groups <- rbind(groups, data.frame(Group = paste(levels[1], "-", levels[2]), N = NA_integer_, Mean = diff, Median = NA_real_, SD = NA_real_,
                                       SE = test$SE[chosen], Min = NA_real_, Max = NA_real_, stringsAsFactors = FALSE))
    vt <- stats::var.test(va, vb)
    variance <- data.frame(Test = "F test (ratio of variances)", F = unname(vt$statistic), DF1 = unname(vt$parameter[1]),
                           DF2 = unname(vt$parameter[2]), P = vt$p.value, stringsAsFactors = FALSE)
    d_eff <- diff / sp
    normality <- .star_tt_normal(list(va, vb), levels)
    keep <- g %in% levels & !is.na(v)
    long <- data.frame(Group = factor(g[keep], levels = levels), Value = v[keep], Pair = NA_integer_, Row = rows[keep], stringsAsFactors = FALSE)
    labels <- levels; pair_r <- NULL
  } else {
    if (is.null(response)) stop("A one-sample t test needs a numeric response.", call. = FALSE)
    if (!is.numeric(mu) || length(mu) != 1 || !is.finite(mu)) stop("The test value must be a single number.", call. = FALSE)
    d <- .star_numeric_frame(data, response)
    notes <- attr(d, "notes")
    v <- d[[response]]; ok <- !is.na(v)
    if (any(!ok)) notes <- c(notes, sprintf("%d row(s) without a value of '%s' were left out (e.g. row %d).", sum(!ok), response, which(!ok)[1]))
    v <- v[ok]
    if (length(v) < 2) stop("A one-sample t test needs at least two observations.", call. = FALSE)
    if (stats::sd(v) == 0) stop("All values are equal, so the t test cannot be computed.", call. = FALSE)
    tt <- stats::t.test(v, mu = mu, alternative = alternative, conf.level = conf)
    groups <- .star_tt_desc(list(v), response)
    tt$conf.int <- tt$conf.int - mu   # interval of the difference from mu, like the other columns
    test <- .star_tt_row(sprintf("One-sample t test (mu = %s)", format(mu)), tt, mean(v) - mu, stats::sd(v) / sqrt(length(v)))
    d_eff <- (mean(v) - mu) / stats::sd(v)
    normality <- .star_tt_normal(list(v), response)
    long <- data.frame(Group = factor(rep(response, length(v)), levels = response), Value = v, Pair = NA_integer_, Row = rows[ok], stringsAsFactors = FALSE)
    labels <- response; pair_r <- NULL; variance <- NULL; chosen <- 1L
  }

  # rank-based alternative (Wilcoxon) for data that are far from normal
  nonpar <- switch(type,
    paired = .star_tt_wilcox("Wilcoxon signed-rank test", a, b, paired = TRUE, alternative = alternative, conf = conf),
    independent = .star_tt_wilcox("Wilcoxon rank-sum test (Mann-Whitney)", va, vb, alternative = alternative, conf = conf),
    one = .star_tt_wilcox(sprintf("Wilcoxon signed-rank test (mu = %s)", format(mu)), v, mu = mu, alternative = alternative, conf = conf))

  p <- test$P[chosen]
  sig <- !is.na(p) && p < alpha
  method_name <- test$Method[chosen]
  res <- list(type = type, method_name = method_name, alternative = alternative, alpha = alpha, conf_level = conf, mu = mu,
              response = response, group = group, x = x, y = y, labels = labels, var_equal = isTRUE(var_equal),
              groups = groups, test = test, chosen = chosen, variance = variance, normality = normality,
              effect_size = d_eff, pair_r = pair_r, p = p, significant = sig, nonparametric = nonpar, data = long, data_notes = notes)
  res$summary <- .star_tt_summary(res)
  res$nonparametric_summary <- .star_tt_np_summary(res)
  res$nonparametric_apa <- .star_tt_np_apa(res)
  res$apa <- .star_tt_apa(res)
  structure(res, class = "star_ttest")
}

.star_tt_desc <- function(vals, names) {
  data.frame(Group = names, N = vapply(vals, length, 0L), Mean = vapply(vals, mean, 0), Median = vapply(vals, stats::median, 0), SD = vapply(vals, stats::sd, 0),
             SE = vapply(vals, \(v) stats::sd(v) / sqrt(length(v)), 0), Min = vapply(vals, min, 0), Max = vapply(vals, max, 0),
             stringsAsFactors = FALSE)
}

.star_tt_row <- function(method, tt, diff, se) {
  data.frame(Method = method, t = unname(tt$statistic), DF = unname(tt$parameter), P = tt$p.value, Difference = diff, SE = se,
             Lower = tt$conf.int[1], Upper = tt$conf.int[2], stringsAsFactors = FALSE)
}

# Wilcoxon test with the Hodges-Lehmann estimate of the shift (or of the median difference from mu) and its interval.
# With ties or zero differences the normal approximation is used (as wilcox.test does) and the interval may be missing.
.star_tt_wilcox <- function(method, a, b = NULL, paired = FALSE, mu = 0, alternative, conf) {
  w <- tryCatch(suppressWarnings(stats::wilcox.test(a, b, paired = paired, mu = mu, alternative = alternative, conf.int = TRUE, conf.level = conf)),
                error = \(e) suppressWarnings(stats::wilcox.test(a, b, paired = paired, mu = mu, alternative = alternative)))
  shift <- if (is.null(b)) mu else 0   # one sample: report the estimate as a difference from mu, like the t test
  est <- if (is.null(w$estimate)) NA_real_ else unname(w$estimate) - shift
  ci <- if (is.null(w$conf.int)) c(NA_real_, NA_real_) else w$conf.int - shift
  data.frame(Method = method, Statistic = paste(names(w$statistic), "=", format(unname(w$statistic))), P = w$p.value,
             Estimate = est, Lower = ci[1], Upper = ci[2], stringsAsFactors = FALSE)
}

# Shapiro-Wilk test of each set of values (needs 3 to 5000 values that are not all equal)
.star_tt_normal <- function(vals, names) {
  out <- data.frame(Data = names, N = vapply(vals, length, 0L), W = NA_real_, P = NA_real_, stringsAsFactors = FALSE)
  for (i in seq_along(vals)) {
    v <- vals[[i]]
    if (length(v) >= 3 && length(v) <= 5000 && stats::sd(v) > 0) {
      sw <- stats::shapiro.test(v)
      out$W[i] <- unname(sw$statistic); out$P[i] <- sw$p.value
    }
  }
  out
}

.star_tt_effect_word <- function(d) {
  a <- abs(d)
  if (is.na(a)) "" else if (a < 0.2) "negligible" else if (a < 0.5) "small" else if (a < 0.8) "medium" else "large"
}

# summary and APA sentence for the Wilcoxon test, for when it is the test reported
.star_tt_np_sig <- function(r) { p <- r$nonparametric$P; !is.na(p) && p < r$alpha }
.star_tt_np_ci <- function(r) {
  w <- r$nonparametric
  if (is.na(w$Lower)) "no interval could be computed" else sprintf("%g%% CI %s to %s", 100 * r$conf_level, .star_tt_num(w$Lower), .star_tt_num(w$Upper))
}
.star_tt_np_summary <- function(r) {
  w <- r$nonparametric; sig <- .star_tt_np_sig(r); md <- r$groups$Median
  verb <- if (sig) c(two.sided = "differ significantly from", greater = "are significantly greater than", less = "are significantly less than")
          else c(two.sided = "do not differ significantly from", greater = "are not significantly greater than", less = "are not significantly less than")
  verb <- verb[[r$alternative]]
  stat <- sprintf("%s, p %s", w$Statistic, .star_fmt_p(w$P))
  est <- sprintf("Hodges-Lehmann estimate %s, %s", .star_tt_num(w$Estimate), .star_tt_np_ci(r))
  switch(r$type,
    independent = sprintf("%s: the values of %s in %s (median %s) %s those in %s (median %s) (%s; %s).", w$Method, r$response, r$labels[1],
                          .star_tt_num(md[1]), verb, r$labels[2], .star_tt_num(md[2]), stat, est),
    paired = sprintf("%s: the values of %s (median %s) %s those of %s (median %s) (%s; %s of the median difference).", w$Method, r$x,
                     .star_tt_num(md[1]), verb, r$y, .star_tt_num(md[2]), stat, est),
    one = sprintf("%s: the values of %s (median %s) %s %s (%s; %s of the difference).", w$Method, r$response, .star_tt_num(md[1]),
                  verb, format(r$mu), stat, est))
}
.star_tt_np_apa <- function(r) {
  nz <- function(x, d) sub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = d))
  w <- r$nonparametric; md <- r$groups$Median
  p <- if (is.na(w$P)) "p = NA" else if (w$P < 0.001) "p < .001" else paste("p =", nz(w$P, 3))
  ci <- if (is.na(w$Lower)) "" else sprintf(", %g%% CI [%s, %s]", 100 * r$conf_level, .star_tt_num(w$Lower), .star_tt_num(w$Upper))
  core <- sprintf("%s, %s, Hodges-Lehmann estimate = %s%s", w$Statistic, p, .star_tt_num(w$Estimate), ci)
  res <- if (.star_tt_np_sig(r)) "significant" else "not significant"
  switch(r$type,
    independent = sprintf("A Wilcoxon rank-sum (Mann-Whitney) test showed that the difference in %s between %s (Mdn = %s) and %s (Mdn = %s) was %s, %s.",
                          r$response, r$labels[1], .star_tt_num(md[1]), r$labels[2], .star_tt_num(md[2]), res, core),
    paired = sprintf("A Wilcoxon signed-rank test showed that the difference between %s (Mdn = %s) and %s (Mdn = %s) was %s, %s.",
                     r$x, .star_tt_num(md[1]), r$y, .star_tt_num(md[2]), res, core),
    one = sprintf("A Wilcoxon signed-rank test showed that %s (Mdn = %s) was %s different from %s, %s.",
                  r$response, .star_tt_num(md[1]), if (res == "significant") "significantly" else "not significantly", format(r$mu), core))
}

.star_tt_summary <- function(r) {
  tr <- r$test[r$chosen, ]
  stat <- sprintf("t(%s) = %.2f, p %s", .star_tt_df(tr$DF), tr$t, .star_fmt_p(tr$P))
  verb <- if (r$significant) c(two.sided = "differs significantly from", greater = "is significantly greater than", less = "is significantly less than")
          else c(two.sided = "does not differ significantly from", greater = "is not significantly greater than", less = "is not significantly less than")
  verb <- verb[[r$alternative]]
  ci <- sprintf("%g%% CI of the difference %s to %s", 100 * r$conf_level, .star_tt_num(tr$Lower), .star_tt_num(tr$Upper))
  main <- switch(r$type,
    independent = sprintf("The mean %s of %s (%s) %s that of %s (%s); the difference is %s (%s; %s).", r$response, r$labels[1],
                          .star_tt_num(r$groups$Mean[1]), verb, r$labels[2], .star_tt_num(r$groups$Mean[2]), .star_tt_num(tr$Difference), stat, ci),
    paired = sprintf("The mean of %s (%s) %s the mean of %s (%s); the mean difference is %s (%s; %s).", r$x, .star_tt_num(r$groups$Mean[1]),
                     verb, r$y, .star_tt_num(r$groups$Mean[2]), .star_tt_num(tr$Difference), stat, ci),
    one = sprintf("The mean %s (%s) %s %s (%s; %s).", r$response, .star_tt_num(r$groups$Mean[1]), verb, format(r$mu), stat, ci))
  es <- sprintf("Effect size: Cohen's d%s = %.2f (%s).", if (r$type == "paired") "z" else "", r$effect_size, .star_tt_effect_word(r$effect_size))
  paste(main, es)
}

.star_tt_apa <- function(r) {
  nz <- function(x, d) sub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = d))
  tr <- r$test[r$chosen, ]
  p <- if (tr$P < 0.001) "p < .001" else paste("p =", nz(tr$P, 3))
  desc <- function(i) sprintf("M = %s, SD = %s", .star_tt_num(r$groups$Mean[i]), .star_tt_num(r$groups$SD[i]))
  core <- sprintf("t(%s) = %.2f, %s, d%s = %.2f, %g%% CI [%s, %s]", .star_tt_df(tr$DF), tr$t, p, if (r$type == "paired") "z" else "",
                  r$effect_size, 100 * r$conf_level, .star_tt_num(tr$Lower), .star_tt_num(tr$Upper))
  res <- if (r$significant) "significant" else "not significant"
  switch(r$type,
    independent = sprintf("An independent-samples t test (%s) showed that the difference in %s between %s (%s) and %s (%s) was %s, %s.",
                          if (r$var_equal) "equal variances assumed" else "Welch", r$response, r$labels[1], desc(1), r$labels[2], desc(2), res, core),
    paired = sprintf("A paired-samples t test showed that the difference between %s (%s) and %s (%s) was %s, %s.",
                     r$x, desc(1), r$y, desc(2), res, core),
    one = sprintf("A one-sample t test showed that the mean %s (%s) was %s different from %s, %s.",
                  r$response, desc(1), if (r$significant) "significantly" else "not significantly", format(r$mu), core))
}

.star_tt_df <- function(df) if (abs(df - round(df)) < 1e-8) format(round(df)) else formatC(df, format = "f", digits = 2)
.star_tt_num <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")

print.star_ttest <- function(x, ...) {
  cat(x$summary, "\n\n")
  print(x$groups, row.names = FALSE, digits = 4)
  cat("\n")
  print(x$test, row.names = FALSE, digits = 4)
  invisible(x)
}

## Plot of a t test: observations with the mean and its confidence interval for each group. A paired test also joins
## the two measurements of each unit; a one-sample test shows the test value as a dashed line.
star_ggplot_ttest <- function(x, main = NULL, xlab = NULL, ylab = NULL, points = TRUE, base_size = 12,
                              palette = NULL, theme = "classic", legend = "auto") {
  d <- x$data
  lv <- levels(d$Group)
  m <- do.call(rbind, lapply(lv, \(g) {
    v <- d$Value[d$Group == g]; n <- length(v); se <- stats::sd(v) / sqrt(n)
    h <- stats::qt(1 - (1 - x$conf_level) / 2, n - 1) * se
    data.frame(Group = factor(g, levels = lv), Mean = mean(v), Lower = mean(v) - h, Upper = mean(v) + h, stringsAsFactors = FALSE)
  }))
  # means sit beside the observations; in a paired plot the first one goes on the outer side, clear of the lines
  m$X <- seq_along(lv) + if (x$type == "paired") c(-0.22, 0.22) else 0.22
  cols <- .star_gg_palette(length(lv), palette)
  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$Group, y = .data$Value))
  if (x$type == "paired") {
    g <- g + ggplot2::geom_line(ggplot2::aes(group = .data$Pair), colour = "grey70", linewidth = 0.4)
    if (points) g <- g + ggplot2::geom_point(ggplot2::aes(colour = .data$Group), size = 1.8, alpha = 0.7)
  } else if (points) {
    g <- g + ggplot2::geom_point(ggplot2::aes(colour = .data$Group), size = 1.8, alpha = 0.6,
                                 position = ggplot2::position_jitter(width = 0.08, height = 0, seed = 1))
  }
  if (x$type == "one") g <- g + ggplot2::geom_hline(yintercept = x$mu, linetype = "dashed", colour = "grey40")
  tr <- x$test[x$chosen, ]
  sub <- sprintf("Mean with %g%% CI. %s: t = %.2f, p %s%s", 100 * x$conf_level, tr$Method, tr$t, .star_fmt_p(tr$P),
                 if (x$type == "one") sprintf("; dashed line = %s", format(x$mu)) else "")
  default_title <- switch(x$type, independent = paste(x$response, "by", x$group), paired = paste(x$x, "vs", x$y), one = paste("Mean", x$response))
  g + ggplot2::geom_errorbar(data = m, ggplot2::aes(x = .data$X, ymin = .data$Lower, ymax = .data$Upper), inherit.aes = FALSE,
                             width = 0.15, linewidth = 0.7) +
    ggplot2::geom_point(data = m, ggplot2::aes(x = .data$X, y = .data$Mean, fill = .data$Group), inherit.aes = FALSE, shape = 23, size = 3.4) +
    ggplot2::scale_colour_manual(values = cols, guide = "none") +
    ggplot2::scale_fill_manual(values = cols, guide = "none") +
    ggplot2::labs(title = .star_gg_or(main, default_title), subtitle = sub,
                  x = .star_gg_or(xlab, if (x$type == "independent") x$group else ""),
                  y = .star_gg_or(ylab, if (x$type == "paired") "Value" else x$response)) +
    star_gg_theme(base_size, theme, .star_gg_legend(legend, "none")) + .star_gg_xlab_theme(lv)
}
