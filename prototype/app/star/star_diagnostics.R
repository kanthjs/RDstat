## New API (not part of the original STAR): checks of the ANOVA assumptions and detection of outliers.
## Refits the same linear model that star_aov() analysed and reports
##   - outliers : observations whose externally studentized residual is large (|r| > 3 outlier, 2 < |r| <= 3 suspect),
##                with their row in the data, treatment labels, fitted value and Cook's distance
##   - normality: Shapiro-Wilk test on the residuals
##   - equal variances: Levene (Brown-Forsythe) test across treatment cells
##   - messages : plain-language advice for what to do next
## Row numbers refer to the rows of the data frame that was given to star_aov().

## Residual checks shared by star_diagnostics() (ANOVA) and star_regression(): outliers, normality, influence.
## `fit` is an lm fitted to the rows of `d` (no missing response); `label_cols` are shown next to flagged rows.
.star_resid_checks <- function(fit, d, response, label_cols, alpha, outlier_cut, suspect_cut) {
  n <- nrow(d)
  df_res <- stats::df.residual(fit)
  if (df_res < 1) stop("There are no residual degrees of freedom, so assumptions cannot be checked.")

  res <- stats::residuals(fit)
  fitted <- stats::fitted(fit)
  hat <- stats::hatvalues(fit)
  sres <- suppressWarnings(stats::rstudent(fit)) # externally studentized: one outlier cannot hide itself
  sres[!is.finite(sres)] <- NA
  cook <- stats::cooks.distance(fit)

  # ---- outliers ----
  a <- abs(sres)
  flag <- ifelse(!is.na(a) & a > outlier_cut, "outlier", ifelse(!is.na(a) & a > suspect_cut, "suspect", ""))
  rn <- rownames(d)
  outliers <- data.frame(
    Row = rn, d[, label_cols, drop = FALSE], Value = d[[response]], Fitted = fitted,
    Residual = res, Studentized = sres, Cook = cook, Flag = flag,
    row.names = NULL, stringsAsFactors = FALSE
  )
  outliers <- outliers[flag != "", , drop = FALSE]
  outliers <- outliers[order(-abs(outliers$Studentized)), , drop = FALSE]
  rownames(outliers) <- NULL
  n_out <- sum(flag == "outlier")
  n_sus <- sum(flag == "suspect")

  # ---- normality of residuals ----
  normality <- if (n >= 3 && n <= 5000 && diff(range(res)) > 0) {
    sw <- stats::shapiro.test(res)
    list(test = "Shapiro-Wilk", statistic = unname(sw$statistic), p = sw$p.value, ok = sw$p.value >= alpha)
  } else {
    list(test = "Shapiro-Wilk", statistic = NA_real_, p = NA_real_, ok = NA)
  }
  ks <- if (n >= 5 && diff(range(res)) > 0) { # Kolmogorov-Smirnov with Lilliefors correction (mean and SD estimated)
    p <- tryCatch(nortest::lillie.test(res)$p.value, error = \(e) NA_real_)
    list(test = "Kolmogorov-Smirnov (Lilliefors)", statistic = tryCatch(unname(nortest::lillie.test(res)$statistic), error = \(e) NA_real_), p = p, ok = p >= alpha)
  } else {
    list(test = "Kolmogorov-Smirnov (Lilliefors)", statistic = NA_real_, p = NA_real_, ok = NA)
  }

  list(n = n, res = res, fitted = fitted, sres = sres, cook = cook, flag = flag, rn = rn, outliers = outliers,
       n_out = n_out, n_sus = n_sus, normality = normality, ks = ks)
}

star_diagnostics <- function(data, response, treatment, block = NULL, alpha = 0.05,
                             outlier_cut = 3, suspect_cut = 2) {
  d <- star_prepare_data(data, response, treatment, block)
  d <- d[!is.na(d[[response]]), , drop = FALSE]
  rhs <- paste(c(if (!is.null(block)) paste0("`", block, "`"), paste0("`", treatment, "`", collapse = " * ")), collapse = " + ")
  fit <- stats::lm(stats::as.formula(paste0("`", response, "` ~ ", rhs)), data = d)
  ck <- .star_resid_checks(fit, d, response, treatment, alpha, outlier_cut, suspect_cut)
  n <- ck$n; res <- ck$res; sres <- ck$sres; cook <- ck$cook; flag <- ck$flag; rn <- ck$rn; fitted <- ck$fitted
  outliers <- ck$outliers; n_out <- ck$n_out; n_sus <- ck$n_sus; normality <- ck$normality; ks <- ck$ks

  # ---- homogeneity of variances (Levene, median-centred) ----
  cell <- interaction(d[, treatment, drop = FALSE], drop = TRUE)
  cell_n <- table(cell)
  homogeneity <- list(test = "Levene (Brown-Forsythe)", statistic = NA_real_, p = NA_real_, ok = NA, ratio = NA_real_)
  if (nlevels(cell) >= 2 && any(cell_n >= 2) && all(cell_n >= 1)) {
    z <- abs(d[[response]] - stats::ave(d[[response]], cell, FUN = stats::median))
    tab <- suppressWarnings(stats::anova(stats::lm(z ~ cell)))
    if (!is.na(tab$`F value`[1]) && tab$Df[2] > 0) {
      homogeneity <- list(
        test = "Levene (Brown-Forsythe)", statistic = tab$`F value`[1], p = tab$`Pr(>F)`[1],
        ok = tab$`Pr(>F)`[1] >= alpha, ratio = NA_real_
      )
    }
    v <- tapply(d[[response]], cell, stats::var)
    v <- v[is.finite(v) & v > 0]
    if (length(v) >= 2) homogeneity$ratio <- max(v) / min(v)
  }

  # ---- plain-language messages ----
  msg <- character()
  if (n_out > 0) {
    top <- outliers[outliers$Flag == "outlier", ][seq_len(min(3, n_out)), ]
    msg <- c(msg, sprintf(
      "%d outlier(s) found (|studentized residual| > %g): row %s. Check for a data-entry or measurement error before deciding to remove or keep %s; re-run the analysis with and without %s to see whether the conclusions change.",
      n_out, outlier_cut, paste0(top$Row, " (", top$Value, ")", collapse = ", "),
      if (n_out == 1) "it" else "them", if (n_out == 1) "it" else "them"
    ))
  } else {
    msg <- c(msg, sprintf("No outliers found (no |studentized residual| > %g).", outlier_cut))
  }
  if (n_sus > 0) msg <- c(msg, sprintf("%d observation(s) are suspect (|studentized residual| between %g and %g); see the table.", n_sus, suspect_cut, outlier_cut))
  hi_cook <- which(!is.na(cook) & cook > 4 / n)
  if (length(hi_cook) && n_out == 0 && n_sus == 0) {
    msg <- c(msg, sprintf("%d observation(s) have a large Cook's distance (> 4/n) but are not outliers; they influence the fit more than most.", length(hi_cook)))
  }
  if (!is.na(ks$p)) msg <- c(msg, sprintf("Kolmogorov-Smirnov (Lilliefors) on residuals: D = %.3f, p %s.", ks$statistic, .star_fmt_p(ks$p)))
  if (isTRUE(normality$ok)) {
    msg <- c(msg, sprintf("Residuals are consistent with a normal distribution (Shapiro-Wilk p %s).", .star_fmt_p(normality$p)))
  } else if (isFALSE(normality$ok)) {
    msg <- c(msg, sprintf(
      "Residuals depart from normality (Shapiro-Wilk p %s)%s. Consider a transformation (log, square root) or a non-parametric test.",
      .star_fmt_p(normality$p), if (n_out > 0) "; this may be caused by the outlier(s)" else ""
    ))
  }
  if (isTRUE(homogeneity$ok)) {
    msg <- c(msg, sprintf("Variances are similar across treatments (Levene p %s).", .star_fmt_p(homogeneity$p)))
  } else if (isFALSE(homogeneity$ok)) {
    msg <- c(msg, sprintf(
      "Variances differ across treatments (Levene p %s%s). The F test and mean comparisons may be unreliable; consider a transformation or a Welch/non-parametric analysis.",
      .star_fmt_p(homogeneity$p),
      if (is.finite(homogeneity$ratio)) sprintf("; largest/smallest variance = %.1f", homogeneity$ratio) else ""
    ))
  }
  if (is.na(normality$ok) || is.na(homogeneity$ok)) msg <- c(msg, "Some assumption tests could not be computed (too few observations per treatment).")

  structure(
    list(
      outliers = outliers, n_outliers = n_out, n_suspect = n_sus, normality = normality, ks = ks,
      homogeneity = homogeneity, messages = msg, ok = n_out == 0 && !isFALSE(normality$ok) && !isFALSE(ks$ok) && !isFALSE(homogeneity$ok),
      residuals = data.frame(Row = rn, Fitted = fitted, Residual = res, Studentized = sres, Flag = flag, stringsAsFactors = FALSE),
      response = response, alpha = alpha, outlier_cut = outlier_cut, suspect_cut = suspect_cut
    ),
    class = "star_diagnostics"
  )
}

print.star_diagnostics <- function(x, ...) {
  cat(paste("-", x$messages), sep = "\n")
  if (nrow(x$outliers)) {
    cat("\nFlagged observations\n")
    print(x$outliers, row.names = FALSE, digits = 4)
  }
  invisible(x)
}

## Residual plots: residuals vs fitted (outliers labelled) and normal Q-Q plot.
star_plot_diagnostics <- function(x) {
  op <- par(mfrow = c(1, 2), mar = c(5, 5, 3, 1))
  on.exit(par(op))
  r <- x$residuals
  col <- ifelse(r$Flag == "outlier", "#E45756", ifelse(r$Flag == "suspect", "#F58518", "#4C78A8"))
  plot(r$Fitted, r$Studentized, pch = 19, col = col, xlab = "Fitted value", ylab = "Studentized residual", main = "Residuals vs fitted")
  abline(h = 0, col = "grey60")
  abline(h = c(-1, 1) * x$outlier_cut, lty = 2, col = "#E45756")
  lab <- r$Flag != ""
  if (any(lab)) text(r$Fitted[lab], r$Studentized[lab], labels = r$Row[lab], pos = 3, cex = 0.8, col = col[lab])
  q <- stats::qqnorm(r$Studentized, plot.it = FALSE)
  plot(q$x, q$y, pch = 19, col = col, xlab = "Theoretical quantile", ylab = "Studentized residual", main = "Normal Q-Q")
  qqline(r$Studentized, col = "grey50")
  if (any(lab)) text(q$x[lab], q$y[lab], labels = r$Row[lab], pos = 3, cex = 0.8, col = col[lab])
  invisible(NULL)
}
