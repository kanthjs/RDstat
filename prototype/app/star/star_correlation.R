## New API (not part of the original STAR): structured results for a correlation analysis of numeric variables.
## The original BivariateCorrelationTest() prints text; this returns data frames so that an app can render
## tables, draw plots and export to Excel.
##   vars        : two or more numeric columns
##   method      : "pearson" (linear), "spearman" or "kendall" (rank based, for non-normal data or outliers)
##   alternative : "two.sided", "greater" or "less"
##   adjust      : multiple-testing correction of the p values of all pairs (see stats::p.adjust); "none" by default,
##                 as in STAR
## Each pair uses every row in which both variables are present (pairwise deletion), as STAR does.

## Numeric columns of a data frame: text such as "1,250.5" or "12 kg" is read as a number (see star_parse_number);
## rows keep their original row names so that row numbers in the results match the user's data.
.star_numeric_frame <- function(data, cols) {
  if (!is.data.frame(data)) stop("'data' must be a data frame.", call. = FALSE)
  .star_check_columns(data, cols, "The variables")
  notes <- character()
  out <- data[, cols, drop = FALSE]
  for (nm in cols) {
    raw <- out[[nm]]
    if (is.factor(raw)) raw <- as.character(raw)
    v <- star_parse_number(raw)
    if (!is.numeric(raw)) {
      shown <- !star_is_blank(raw)
      bad <- is.na(v) & shown
      if (sum(shown) == 0 || sum(!is.na(v)) == 0) stop("The variable '", nm, "' contains no numbers.", call. = FALSE)
      if (sum(bad) > 0.5 * sum(shown)) {
        stop("The variable '", nm, "' is mostly text (e.g. '", paste(utils::head(unique(as.character(raw[bad])), 3), collapse = "', '"),
             "'); choose a numeric column.", call. = FALSE)
      }
      if (any(bad)) notes <- c(notes, sprintf("Variable '%s': %d value(s) are not numbers and were treated as missing (e.g. '%s', row %d).",
                                             nm, sum(bad), as.character(raw[bad][1]), which(bad)[1]))
    }
    inf <- is.infinite(v)
    if (any(inf)) {
      v[inf] <- NA
      notes <- c(notes, sprintf("Variable '%s': %d infinite value(s) were treated as missing.", nm, sum(inf)))
    }
    out[[nm]] <- v
  }
  structure(out, notes = notes)
}

.star_corr_strength <- function(r) {
  a <- abs(r)
  ifelse(is.na(a), NA_character_, ifelse(a < 0.1, "negligible", ifelse(a < 0.3, "weak", ifelse(a < 0.5, "moderate", ifelse(a < 0.7, "strong", "very strong")))))
}

star_correlation <- function(data, vars, method = c("pearson", "spearman", "kendall"),
                             alternative = c("two.sided", "greater", "less"), alpha = 0.05,
                             adjust = c("none", "holm", "bonferroni", "BH")) {
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  adjust <- match.arg(adjust)
  if (length(vars) < 2) stop("Choose at least two variables.", call. = FALSE)
  d <- .star_numeric_frame(data, vars)
  notes <- attr(d, "notes")
  k <- length(vars)
  constant <- vars[vapply(vars, \(v) length(unique(stats::na.omit(d[[v]]))) < 2, NA)]
  if (length(constant)) stop("The variable '", constant[1], "' is constant (or has fewer than two values); it cannot be correlated.", call. = FALSE)

  r_m <- p_m <- n_m <- matrix(NA_real_, k, k, dimnames = list(vars, vars))
  ci_lo <- ci_hi <- r_m
  diag(r_m) <- 1
  for (j in seq_len(k)) n_m[j, j] <- sum(!is.na(d[[vars[j]]]))
  pairs <- utils::combn(k, 2)
  tab <- vector("list", ncol(pairs))
  for (s in seq_len(ncol(pairs))) {
    i <- pairs[1, s]; j <- pairs[2, s]
    ok <- stats::complete.cases(d[[vars[i]]], d[[vars[j]]])
    n <- sum(ok)
    ct <- if (n >= 3) tryCatch(suppressWarnings(stats::cor.test(d[[vars[i]]][ok], d[[vars[j]]][ok], method = method,
                                                                alternative = alternative, exact = FALSE, conf.level = 1 - alpha)),
                               error = \(e) NULL)
    r <- if (is.null(ct)) NA_real_ else unname(ct$estimate)
    p <- if (is.null(ct)) NA_real_ else ct$p.value
    ci <- if (!is.null(ct) && !is.null(ct$conf.int)) as.numeric(ct$conf.int) else c(NA_real_, NA_real_)
    r_m[i, j] <- r_m[j, i] <- r
    p_m[i, j] <- p_m[j, i] <- p
    n_m[i, j] <- n_m[j, i] <- n
    ci_lo[i, j] <- ci_lo[j, i] <- ci[1]
    ci_hi[i, j] <- ci_hi[j, i] <- ci[2]
    tab[[s]] <- data.frame(Variable1 = vars[i], Variable2 = vars[j], r = r, N = n, P = p, Lower = ci[1], Upper = ci[2],
                           stringsAsFactors = FALSE)
  }
  pair_tab <- do.call(rbind, tab)
  pair_tab$PAdj <- stats::p.adjust(pair_tab$P, method = if (adjust == "none") "none" else adjust)
  pair_tab$Significant <- !is.na(pair_tab$PAdj) & pair_tab$PAdj < alpha
  pair_tab$Strength <- .star_corr_strength(pair_tab$r)
  rownames(pair_tab) <- NULL
  if (adjust != "none") { # the matrix shows the p values that decide significance
    pa <- p_m
    for (s in seq_len(nrow(pair_tab))) {
      pa[pair_tab$Variable1[s], pair_tab$Variable2[s]] <- pa[pair_tab$Variable2[s], pair_tab$Variable1[s]] <- pair_tab$PAdj[s]
    }
    p_m <- pa
  }
  if (any(pair_tab$N < 10, na.rm = TRUE)) notes <- c(notes, "Some pairs have fewer than 10 observations; correlations from so few points are unreliable.")
  if (any(is.na(pair_tab$r))) notes <- c(notes, "Some correlations could not be computed (fewer than 3 complete pairs).")
  if (any(pair_tab$N < nrow(d), na.rm = TRUE)) notes <- c(notes, "Rows with a missing value are left out pair by pair, so N differs between pairs.")

  method_name <- c(pearson = "Pearson", spearman = "Spearman", kendall = "Kendall")[[method]]
  sym <- c(pearson = "r", spearman = "rho", kendall = "tau")[[method]]
  n_sig <- sum(pair_tab$Significant)
  strongest <- pair_tab[which.max(abs(pair_tab$r)), ]
  summary_text <- if (is.na(strongest$r)) "No correlation could be computed." else sprintf(
    "%s correlation of %d variable(s), %d pair(s): %d significant at alpha = %s%s. Strongest: %s and %s (%s = %.2f, %s).",
    method_name, k, nrow(pair_tab), n_sig, format(alpha), if (adjust != "none") paste0(" (", adjust, "-adjusted)") else "",
    strongest$Variable1, strongest$Variable2, sym, strongest$r, strongest$Strength)
  apa <- vapply(seq_len(nrow(pair_tab)), \(s) {
    z <- pair_tab[s, ]
    if (is.na(z$r)) return(sprintf("%s and %s: not computed.", z$Variable1, z$Variable2))
    sprintf("%s and %s were %s correlated, %s(%d) = %s, p %s.", z$Variable1, z$Variable2,
            if (z$Significant) paste(z$Strength, if (z$r < 0) "negatively" else "positively") else "not significantly",
            sym, z$N - 2, sub("^(-?)0\\.", "\\1.", formatC(z$r, format = "f", digits = 2)), .star_fmt_p(z$PAdj))
  }, "")

  structure(
    list(pairs = pair_tab, r = r_m, p = p_m, n = n_m, lower = ci_lo, upper = ci_hi, vars = vars, method = method,
         method_name = method_name, symbol = sym, alternative = alternative, alpha = alpha, adjust = adjust,
         summary = summary_text, apa = apa, data_notes = notes, data = d),
    class = "star_correlation"
  )
}

print.star_correlation <- function(x, ...) {
  cat(x$summary, "\n\n")
  m <- formatC(x$r, format = "f", digits = 3)
  m[upper.tri(m, diag = TRUE)] <- ""
  print(noquote(m[-1, -ncol(m), drop = FALSE]))
  cat("\nPairs\n")
  print(x$pairs, row.names = FALSE, digits = 4)
  invisible(x)
}
