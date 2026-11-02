## New API (not part of the original STAR): structured results for a linear regression of one numeric response on
## one or more numeric predictors. The original LinearRegressionAnalysis() prints text; this returns data frames so
## that an app can render tables, draw plots and export to Excel.
##   predictors : one or more numeric columns (multiple regression when there are several)
##   degree     : 1 = straight line (default), 2 = quadratic, 3 = cubic; polynomials need exactly one predictor.
##                A quadratic response such as yield against fertiliser rate also reports the optimum.
## Rows with a missing value in any chosen column are left out. The residual checks (outliers, normality,
## constant variance) are returned as a `diagnostics` element that star_ggplot_diagnostics() can draw.

.star_fmt_num <- function(x, digits = 4) formatC(x, format = "g", digits = digits)

star_regression <- function(data, response, predictors, degree = 1, alpha = 0.05) {
  degree <- as.integer(degree)
  if (length(predictors) < 1) stop("Choose at least one predictor.", call. = FALSE)
  if (!degree %in% 1:3) stop("'degree' must be 1, 2 or 3.", call. = FALSE)
  if (degree > 1 && length(predictors) != 1) stop("A polynomial (degree > 1) needs exactly one predictor.", call. = FALSE)
  .star_check_columns(data, c(response, predictors), "The response and predictors")
  d0 <- .star_numeric_frame(data, c(response, predictors))
  notes <- attr(d0, "notes")
  ok <- stats::complete.cases(d0)
  if (!all(ok)) notes <- c(notes, sprintf("%d of %d rows have a missing value and were left out (row %s%s).", sum(!ok), nrow(d0),
                                         paste(utils::head(rownames(d0)[!ok], 5), collapse = ", "), if (sum(!ok) > 5) ", ..." else ""))
  d0 <- d0[ok, , drop = FALSE]
  n <- nrow(d0)
  p_terms <- length(predictors) * 1 + (degree - 1)
  if (n < p_terms + 3) stop("Too few complete rows (", n, ") for a model with ", p_terms, " predictor term(s); at least ", p_terms + 3, " are needed.", call. = FALSE)
  const <- c(response, predictors)[vapply(c(response, predictors), \(v) length(unique(d0[[v]])) < 2, NA)]
  if (length(const)) stop("The variable '", const[1], "' is constant; it cannot be used in a regression.", call. = FALSE)

  # model frame whose columns carry the term labels (x, x^2, ...), so coefficient names are readable
  mf <- d0[response]
  terms_x <- predictors
  if (degree > 1) {
    terms_x <- c(predictors, paste0(predictors, "^", 2:degree))
    for (k in 2:degree) mf[[paste0(predictors, "^", k)]] <- d0[[predictors]]^k
  }
  mf[predictors] <- d0[predictors]
  mf <- mf[c(response, terms_x)]
  fml <- stats::as.formula(paste0("`", response, "` ~ ", paste0("`", terms_x, "`", collapse = " + ")))
  fit <- stats::lm(fml, data = mf)
  if (anyNA(stats::coef(fit))) {
    stop("The predictors are perfectly collinear (", paste(names(stats::coef(fit))[is.na(stats::coef(fit))], collapse = ", "),
         " add no information); remove one of them.", call. = FALSE)
  }
  sm <- summary(fit)
  y <- mf[[response]]
  df_m <- length(terms_x)
  df_e <- stats::df.residual(fit)
  ss_t <- sum((y - mean(y))^2)
  ss_e <- sum(stats::residuals(fit)^2)
  ss_m <- ss_t - ss_e
  f_val <- (ss_m / df_m) / (ss_e / df_e)
  f_p <- stats::pf(f_val, df_m, df_e, lower.tail = FALSE)
  anova <- data.frame(Source = c("Model", "Error", "Total"), DF = c(df_m, df_e, n - 1), SS = c(ss_m, ss_e, ss_t),
                      MS = c(ss_m / df_m, ss_e / df_e, NA), F = c(f_val, NA, NA), P = c(f_p, NA, NA), stringsAsFactors = FALSE)
  rmse <- sqrt(ss_e / df_e)
  r2 <- ss_m / ss_t
  fit_stats <- list(n = n, r2 = r2, adj_r2 = sm$adj.r.squared, rmse = rmse, mean_y = mean(y), cv = 100 * rmse / mean(y),
                    f = f_val, df1 = df_m, df2 = df_e, p = f_p, aic = stats::AIC(fit), bic = stats::BIC(fit))

  # ---- coefficients ----
  cf <- sm$coefficients
  ci <- stats::confint(fit, level = 1 - alpha)
  vif <- rep(NA_real_, nrow(cf))
  if (degree == 1 && length(predictors) > 1) {
    vif[-1] <- diag(solve(stats::cor(mf[predictors])))
  }
  beta <- rep(NA_real_, nrow(cf))
  if (degree == 1) beta[-1] <- cf[-1, 1] * vapply(predictors, \(v) stats::sd(mf[[v]]), 0) / stats::sd(y)
  coefs <- data.frame(Term = c("(Intercept)", terms_x), Estimate = cf[, 1], SE = cf[, 2], t = cf[, 3], P = cf[, 4],
                      Lower = ci[, 1], Upper = ci[, 2], StdBeta = beta, VIF = vif, row.names = NULL, stringsAsFactors = FALSE)
  coefs$Significant <- !is.na(coefs$P) & coefs$P < alpha

  # ---- optimum of a quadratic (stationary point of the fitted curve) ----
  optimum <- NULL
  if (degree == 2) {
    b <- cf[, 1]
    xs <- -b[2] / (2 * b[3])
    rng <- range(mf[[predictors]])
    optimum <- list(x = unname(xs), y = unname(b[1] + b[2] * xs + b[3] * xs^2), type = if (b[3] < 0) "maximum" else "minimum",
                    inside = xs >= rng[1] && xs <= rng[2], range = rng)
  }
  equation <- paste0(response, " = ", .star_fmt_num(cf[1, 1]),
                     paste0(ifelse(cf[-1, 1] < 0, " - ", " + "), .star_fmt_num(abs(cf[-1, 1])), " ", terms_x, collapse = ""))

  # ---- assumptions ----
  diagnostics <- tryCatch(.star_reg_diagnostics(fit, mf, response, alpha), error = \(e) NULL)

  # ---- plain-language summary ----
  sig_terms <- coefs$Term[-1][coefs$Significant[-1]]
  summary_text <- sprintf(
    "The model is %s (F(%d, %d) = %.2f, p %s) and explains %.1f%% of the variation in %s (adjusted R-squared = %.3f).%s%s",
    if (f_p < alpha) "significant" else "not significant", df_m, df_e, f_val, .star_fmt_p(f_p), 100 * r2, response, sm$adj.r.squared,
    if (length(sig_terms)) paste0(" Significant term(s): ", paste(sig_terms, collapse = ", "), ".") else " No individual term is significant.",
    if (!is.null(optimum)) sprintf(" The fitted curve has a %s at %s = %s (%s = %s)%s.", optimum$type, predictors, .star_fmt_num(optimum$x),
                                   response, .star_fmt_num(optimum$y), if (optimum$inside) "" else ", outside the range of the data, so it is an extrapolation")
    else "")
  if (any(vif > 10, na.rm = TRUE)) {
    notes <- c(notes, sprintf("Variance inflation factor above 10 for %s: these predictors are strongly correlated with the others, so their individual estimates are unstable.",
                              paste(coefs$Term[which(vif > 10)], collapse = ", ")))
  }
  if (n < 10 * df_m) notes <- c(notes, sprintf("Only %d rows for %d predictor term(s); fewer than 10 rows per term makes the estimates unreliable.", n, df_m))
  apa <- sprintf("The regression model was %s, F(%d, %d) = %.2f, p %s, R2 = %.2f.", if (f_p < alpha) "significant" else "not significant",
                 df_m, df_e, f_val, .star_fmt_p(f_p), r2)

  structure(
    list(anova = anova, fit = fit_stats, coefficients = coefs, equation = equation, optimum = optimum, response = response,
         predictors = predictors, terms = terms_x, degree = degree, alpha = alpha, summary = summary_text, apa = apa,
         diagnostics = diagnostics, data_notes = notes, data = d0, model = fit, p_model = f_p, significant = f_p < alpha),
    class = "star_regression"
  )
}

# Residual checks for a regression: outliers, normality (shared with star_diagnostics) and constant variance
# (Breusch-Pagan, studentized, against the fitted values: lmtest::bptest).
.star_reg_diagnostics <- function(fit, mf, response, alpha, outlier_cut = 3, suspect_cut = 2) {
  ck <- .star_resid_checks(fit, mf, response, setdiff(names(mf), response), alpha, outlier_cut, suspect_cut)
  n <- ck$n
  e2 <- ck$res^2
  homogeneity <- list(test = "Breusch-Pagan", statistic = NA_real_, p = NA_real_, ok = NA, ratio = NA_real_)
  if (n >= 5 && stats::var(ck$fitted) > 0 && stats::var(e2) > 0) {
    bp <- lmtest::bptest(fit, ~ f, data = data.frame(f = ck$fitted), studentize = TRUE)
    stat <- unname(bp$statistic)
    p <- bp$p.value
    homogeneity <- list(test = "Breusch-Pagan", statistic = stat, p = p, ok = p >= alpha, ratio = NA_real_)
  }
  msg <- character()
  if (ck$n_out > 0) {
    top <- ck$outliers[ck$outliers$Flag == "outlier", ][seq_len(min(3, ck$n_out)), ]
    msg <- c(msg, sprintf("%d outlier(s) found (|studentized residual| > %g): row %s. Check for a data-entry or measurement error before deciding to remove or keep %s; fit the model with and without %s to see whether the conclusions change.",
                          ck$n_out, outlier_cut, paste0(top$Row, " (", top$Value, ")", collapse = ", "),
                          if (ck$n_out == 1) "it" else "them", if (ck$n_out == 1) "it" else "them"))
  } else {
    msg <- c(msg, sprintf("No outliers found (no |studentized residual| > %g).", outlier_cut))
  }
  if (ck$n_sus > 0) msg <- c(msg, sprintf("%d observation(s) are suspect (|studentized residual| between %g and %g); see the table.", ck$n_sus, suspect_cut, outlier_cut))
  lev <- stats::hatvalues(fit)
  hi_lev <- which(lev > 2 * length(stats::coef(fit)) / n)
  if (length(hi_lev) && ck$n_out == 0) msg <- c(msg, sprintf("%d observation(s) have high leverage (unusual predictor values); they pull the fitted model towards themselves.", length(hi_lev)))
  if (isTRUE(ck$normality$ok)) {
    msg <- c(msg, sprintf("Residuals are consistent with a normal distribution (Shapiro-Wilk p %s).", .star_fmt_p(ck$normality$p)))
  } else if (isFALSE(ck$normality$ok)) {
    msg <- c(msg, sprintf("Residuals depart from normality (Shapiro-Wilk p %s)%s. Consider a transformation (log, square root).",
                          .star_fmt_p(ck$normality$p), if (ck$n_out > 0) "; this may be caused by the outlier(s)" else ""))
  }
  if (isTRUE(homogeneity$ok)) {
    msg <- c(msg, sprintf("The spread of the residuals is constant (Breusch-Pagan p %s).", .star_fmt_p(homogeneity$p)))
  } else if (isFALSE(homogeneity$ok)) {
    msg <- c(msg, sprintf("The spread of the residuals changes with the fitted value (Breusch-Pagan p %s); standard errors and p values may be unreliable. Consider a transformation of the response.", .star_fmt_p(homogeneity$p)))
  }
  structure(
    list(outliers = ck$outliers, n_outliers = ck$n_out, n_suspect = ck$n_sus, normality = ck$normality, ks = ck$ks,
         homogeneity = homogeneity, messages = msg,
         ok = ck$n_out == 0 && !isFALSE(ck$normality$ok) && !isFALSE(ck$ks$ok) && !isFALSE(homogeneity$ok),
         residuals = data.frame(Row = ck$rn, Fitted = ck$fitted, Residual = ck$res, Studentized = ck$sres, Flag = ck$flag, stringsAsFactors = FALSE),
         response = response, alpha = alpha, outlier_cut = outlier_cut, suspect_cut = suspect_cut),
    class = "star_diagnostics"
  )
}

## Predicted values (with confidence interval of the mean) at new predictor values.
star_predict <- function(x, newdata, level = 0.95) {
  if (!inherits(x, "star_regression")) stop("'x' must be a result of star_regression().", call. = FALSE)
  nd <- as.data.frame(newdata, stringsAsFactors = FALSE)
  if (!all(x$predictors %in% names(nd))) stop("'newdata' needs the column(s): ", paste(x$predictors, collapse = ", "), call. = FALSE)
  nd <- nd[x$predictors]
  if (x$degree > 1) for (k in 2:x$degree) nd[[paste0(x$predictors, "^", k)]] <- nd[[x$predictors]]^k
  pr <- stats::predict(x$model, newdata = nd, interval = "confidence", level = level)
  out <- data.frame(newdata[x$predictors], Predicted = pr[, "fit"], Lower = pr[, "lwr"], Upper = pr[, "upr"], row.names = NULL, stringsAsFactors = FALSE)
  rng <- vapply(x$predictors, \(v) range(x$data[[v]]), c(0, 0))
  out$Extrapolated <- vapply(seq_len(nrow(nd)), \(i) any(unlist(nd[i, x$predictors]) < rng[1, ] | unlist(nd[i, x$predictors]) > rng[2, ]), NA)
  out
}

print.star_regression <- function(x, ...) {
  cat(x$summary, "\n\nModel: ", x$equation, "\n\n", sep = "")
  print(x$anova, row.names = FALSE, digits = 4, na.print = "")
  cat("\n")
  print(x$coefficients[c("Term", "Estimate", "SE", "t", "P", "Lower", "Upper")], row.names = FALSE, digits = 4)
  invisible(x)
}
