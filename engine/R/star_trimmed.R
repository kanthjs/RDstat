## New API (not part of the original STAR): comparisons of trimmed means, for comparing means without relying on the
## normal distribution (Wilcox 2017). With 20% trimming the lowest and highest 20% of each group are set aside, so a
## few extreme values or a skewed tail do not drive the result; the tests use Winsorized variances and do not assume
## equal variances. Percentile bootstrap intervals (fixed seed, so the same data give the same interval) are given
## beside the t-based ones.
##   - two independent groups: Yuen's test (Yuen 1974)
##   - paired data or one sample: the trimmed mean of the differences (or of the values minus mu), one-sample test
##   - several groups (CRD, one factor): heteroscedastic one-way ANOVA on trimmed means (Wilcox's t1way), with Yuen's
##     tests between pairs, Holm adjusted, for the letters
## With trimming 0 the methods are Welch's t-test and Welch's ANOVA.

.star_tmean <- function(x, tr) mean(x, trim = tr)

## Winsorized variance: the g lowest values are replaced by the next one, the g highest by the one below them
.star_winvar <- function(x, tr) {
  n <- length(x); g <- floor(tr * n); y <- sort(x)
  if (g > 0) { y[seq_len(g)] <- y[g + 1]; y[(n - g + 1):n] <- y[n - g] }
  stats::var(y)
}

## the squared standard error of a trimmed mean, as used by Yuen: (n - 1) s_w^2 / (h (h - 1)), h = n - 2g
.star_tm_d <- function(x, tr) { n <- length(x); h <- n - 2 * floor(tr * n); (n - 1) * .star_winvar(x, tr) / (h * (h - 1)) }
.star_tm_h <- function(x, tr) length(x) - 2 * floor(tr * length(x))

.star_pt <- function(t, df, alternative) switch(alternative,
  two.sided = 2 * stats::pt(-abs(t), df), greater = stats::pt(t, df, lower.tail = FALSE), less = stats::pt(t, df))
.star_ci <- function(est, se, df, conf, alternative) {
  q <- stats::qt(if (alternative == "two.sided") 1 - (1 - conf) / 2 else conf, df)
  switch(alternative, two.sided = est + c(-1, 1) * q * se, greater = c(est - q * se, Inf), less = c(-Inf, est + q * se))
}

## run `expr` with a fixed seed and leave the caller's random numbers untouched
.star_with_seed <- function(seed, expr) {
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old, envir = globalenv()))
  set.seed(seed)
  expr
}
.star_boot <- function(stat, B, conf, seed = 1) {
  v <- .star_with_seed(seed, replicate(B, stat()))
  unname(stats::quantile(v, c((1 - conf) / 2, 1 - (1 - conf) / 2), na.rm = TRUE))
}

## Yuen's test of two independent groups
.star_yuen <- function(a, b, tr = 0.2, conf = 0.95, alternative = "two.sided") {
  da <- .star_tm_d(a, tr); db <- .star_tm_d(b, tr); ha <- .star_tm_h(a, tr); hb <- .star_tm_h(b, tr)
  est <- .star_tmean(a, tr) - .star_tmean(b, tr); se <- sqrt(da + db)
  df <- (da + db)^2 / (da^2 / (ha - 1) + db^2 / (hb - 1))
  t <- est / se
  list(estimate = est, se = se, t = t, df = df, p = .star_pt(t, df, alternative), ci = .star_ci(est, se, df, conf, alternative))
}
## one-sample test of a trimmed mean against mu (paired data: the differences against 0)
.star_trim1 <- function(x, mu = 0, tr = 0.2, conf = 0.95, alternative = "two.sided") {
  est <- .star_tmean(x, tr) - mu; se <- sqrt(.star_tm_d(x, tr)); df <- .star_tm_h(x, tr) - 1; t <- est / se
  list(estimate = est, se = se, t = t, df = df, p = .star_pt(t, df, alternative), ci = .star_ci(est, se, df, conf, alternative))
}

## heteroscedastic one-way ANOVA on trimmed means (Wilcox's t1way)
.star_t1way <- function(y, g, tr = 0.2) {
  g <- droplevels(as.factor(g)); x <- split(y, g); J <- length(x)
  d <- vapply(x, .star_tm_d, 0, tr = tr); h <- vapply(x, .star_tm_h, 0, tr = tr); tm <- vapply(x, .star_tmean, 0, tr = tr)
  w <- 1 / d; U <- sum(w); xt <- sum(w * tm) / U
  A <- sum(w * (tm - xt)^2) / (J - 1)
  S <- sum((1 - w / U)^2 / (h - 1))
  B <- 2 * (J - 2) / (J^2 - 1) * S
  F <- A / (1 + B); df1 <- J - 1; df2 <- 1 / (3 / (J^2 - 1) * S)
  list(F = F, df1 = df1, df2 = df2, p = stats::pf(F, df1, df2, lower.tail = FALSE))
}

## trimmed-means comparison of the treatments of a one-factor CRD
star_trimmed <- function(data, response, treatment, alpha = 0.05, tr = 0.2, B = 2000) {
  if (length(treatment) != 1) stop("The trimmed-means comparison needs exactly one treatment factor.", call. = FALSE)
  d <- star_prepare_data(data, response, treatment)
  d <- d[!is.na(d[[response]]), , drop = FALSE]
  y <- d[[response]]; g <- droplevels(d[[treatment]]); lv <- levels(g); k <- length(lv)
  if (k < 2) stop("The treatment needs at least two levels.", call. = FALSE)
  if (any(vapply(split(y, g), .star_tm_h, 0, tr = tr) < 2)) stop("Too few observations per treatment for trimmed means (at least 3 are needed).", call. = FALSE)
  x <- split(y, g); n <- lengths(x)
  ow <- .star_t1way(y, g, tr)
  pairs <- utils::combn(k, 2)
  yu <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(j) {
    a <- pairs[1, j]; b <- pairs[2, j]; r <- .star_yuen(x[[a]], x[[b]], tr)
    data.frame(Comparison = paste(lv[a], "-", lv[b]), Difference = r$estimate, SE = r$se, t = r$t, DF = r$df, P = r$p, stringsAsFactors = FALSE)
  }))
  yu$P.adj <- stats::p.adjust(yu$P, "holm")
  P <- matrix(1, k, k, dimnames = list(lv, lv)); P[t(pairs)] <- yu$P.adj; P[t(pairs[2:1, , drop = FALSE])] <- yu$P.adj
  tm <- vapply(x, .star_tmean, 0, tr = tr)
  ci <- t(vapply(x, function(v) .star_boot(function() .star_tmean(sample(v, replace = TRUE), tr), B, 1 - alpha), numeric(2)))
  sig <- ow$p < alpha
  means <- data.frame(Treatment = lv, TrimmedMean = unname(tm), N = as.integer(n), Lower = ci[, 1], Upper = ci[, 2],
                      Group = if (sig) unname(.star_letters(lv, unname(tm), P, alpha)) else NA_character_, stringsAsFactors = FALSE)
  res <- list(name = sprintf("Trimmed-means ANOVA (%g%%)", 100 * tr), statistic = ow$F, df1 = ow$df1, df2 = ow$df2, p = ow$p, significant = sig,
              means = means, pairs = yu, treatment = treatment, response = response, alpha = alpha, tr = tr, B = B, n_min = min(n),
              posthoc_label = "Yuen tests, Holm adjusted", rank_label = "Trimmed mean",
              data = data.frame(Treatment = g, y = y, stringsAsFactors = FALSE))
  res$summary <- sprintf("%s: F(%d, %.2f) = %.2f, p %s. %s", res$name, res$df1, res$df2, res$statistic, .star_fmt_p(res$p),
                         if (sig) sprintf("The %s trimmed means differ (letters: Yuen tests, Holm adjusted).", treatment) else sprintf("No significant difference among the %s trimmed means.", treatment))
  p_apa <- if (res$p < 0.001) "p < .001" else paste("p =", sub("^0\\.", ".", formatC(res$p, format = "f", digits = 3)))
  res$apa <- sprintf("A heteroscedastic one-way ANOVA on %g%% trimmed means showed that %s %s among the levels of %s, F(%d, %.2f) = %.2f, %s.",
                     100 * tr, response, if (sig) "differed significantly" else "did not differ significantly", treatment, res$df1, res$df2, res$statistic, p_apa)
  structure(res, class = "star_trimmed")
}

## trimmed-means test for a star_ttest() result: Yuen (two groups), or the trimmed mean of the differences / values
star_tt_trimmed <- function(r, tr = 0.2, B = 2000) {
  d <- r$data; conf <- r$conf_level; alt <- r$alternative
  if (r$type == "independent") {
    a <- d$Value[d$Group == levels(d$Group)[1]]; b <- d$Value[d$Group == levels(d$Group)[2]]
    if (min(.star_tm_h(a, tr), .star_tm_h(b, tr)) < 2) stop("Too few observations for trimmed means (at least 3 per group).", call. = FALSE)
    t <- .star_yuen(a, b, tr, conf, alt)
    boot <- .star_boot(function() .star_tmean(sample(a, replace = TRUE), tr) - .star_tmean(sample(b, replace = TRUE), tr), B, conf)
    method <- sprintf("Yuen's test (%g%% trimmed means)", 100 * tr)
    tms <- c(.star_tmean(a, tr), .star_tmean(b, tr)); n_min <- min(length(a), length(b))
  } else {
    v <- if (r$type == "paired") { w <- stats::reshape(d[c("Pair", "Group", "Value")], idvar = "Pair", timevar = "Group", direction = "wide"); w[[2]] - w[[3]] } else d$Value
    mu <- if (r$type == "one") r$mu else 0
    if (.star_tm_h(v, tr) < 2) stop("Too few observations for trimmed means (at least 3).", call. = FALSE)
    t <- .star_trim1(v, mu, tr, conf, alt)
    boot <- .star_boot(function() .star_tmean(sample(v, replace = TRUE), tr) - mu, B, conf)
    method <- sprintf("Trimmed mean (%g%%) of the %s", 100 * tr, if (r$type == "paired") "differences" else "values")
    tms <- if (r$type == "paired") c(.star_tmean(d$Value[d$Group == levels(d$Group)[1]], tr), .star_tmean(d$Value[d$Group == levels(d$Group)[2]], tr)) else .star_tmean(v, tr)
    n_min <- length(v)
  }
  sig <- !is.na(t$p) && t$p < r$alpha
  out <- list(table = data.frame(Method = method, t = t$t, DF = t$df, P = t$p, Estimate = t$estimate, SE = t$se, Lower = t$ci[1], Upper = t$ci[2],
                                 BootLower = boot[1], BootUpper = boot[2], stringsAsFactors = FALSE),
              trimmed_means = tms, significant = sig, tr = tr, B = B, n_min = n_min, method = method)
  what <- switch(r$type, independent = sprintf("the %g%% trimmed means of %s in %s (%s) and %s (%s)", 100 * tr, r$response, r$labels[1], .star_tt_num(tms[1]), r$labels[2], .star_tt_num(tms[2])),
                 paired = sprintf("the %g%% trimmed mean of the differences %s - %s", 100 * tr, r$x, r$y),
                 one = sprintf("the %g%% trimmed mean of %s (%s) and %s", 100 * tr, r$response, .star_tt_num(tms[1]), format(r$mu)))
  ci_txt <- sprintf("%g%% CI %s to %s; bootstrap %s to %s", 100 * conf, .star_tt_num(t$ci[1]), .star_tt_num(t$ci[2]), .star_tt_num(boot[1]), .star_tt_num(boot[2]))
  out$summary <- sprintf("%s: the difference between %s is %s (t(%.2f) = %.2f, p %s; %s). %s", method, what, .star_tt_num(t$estimate), t$df, t$t, .star_fmt_p(t$p), ci_txt,
                         if (sig) "The difference is significant." else "The difference is not significant.")
  p_apa <- if (t$p < 0.001) "p < .001" else paste("p =", sub("^0\\.", ".", formatC(t$p, format = "f", digits = 3)))
  out$apa <- sprintf("A %s showed that the difference was %s, t(%.2f) = %.2f, %s, difference = %s, %g%% CI [%s, %s] (percentile bootstrap [%s, %s]).",
                     sub("^Yuen", "Yuen", method), if (sig) "significant" else "not significant", t$df, t$t, p_apa, .star_tt_num(t$estimate), 100 * conf,
                     .star_tt_num(t$ci[1]), .star_tt_num(t$ci[2]), .star_tt_num(boot[1]), .star_tt_num(boot[2]))
  out
}
