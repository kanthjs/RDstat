## New API (not part of the original STAR): rank-based alternative to a one-factor analysis of variance, for data that
## do not meet its assumptions (normal residuals, equal variances).
##   block = NULL : Kruskal-Wallis test (CRD), groups compared on their mean ranks
##   block given  : Friedman test (RCBD, one observation per block x treatment), groups compared on their rank sums
## Multiple comparisons are those of the agricolae package (Fisher's LSD on ranks, Conover); as with the ANOVA,
## letters are given only when the overall test is significant.

star_nonparametric <- function(data, response, treatment, block = NULL, alpha = 0.05) {
  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  if (length(treatment) != 1) stop("The rank-based test needs exactly one treatment factor.", call. = FALSE)
  .star_check_columns(data, c(response, treatment, block), "Response, treatment and block")
  d <- star_prepare_data(data, response, treatment, block)
  d <- d[!is.na(d[[response]]), , drop = FALSE]
  y <- d[[response]]
  tr <- droplevels(d[[treatment]])
  if (nlevels(tr) < 2) stop("The treatment needs at least two levels.", call. = FALSE)
  if (is.null(block)) {
    k <- agricolae::kruskal(y, tr, alpha = alpha, console = FALSE)
    st <- k$statistics
    out <- data.frame(Treatment = rownames(k$means), Median = k$means$Q50, N = k$means$r, Rank = k$means$rank,
                      stringsAsFactors = FALSE)
    out$Group <- trimws(as.character(k$groups[out$Treatment, "groups"]))
    test <- list(name = "Kruskal-Wallis", statistic = unname(st$Chisq), df = unname(st$Df), p = unname(st$p.chisq),
                 rank_label = "Mean rank")
  } else {
    b <- droplevels(d[[block]])
    if (any(table(b, tr) != 1)) stop("The Friedman test needs exactly one observation of each treatment in each block.", call. = FALSE)
    f <- agricolae::friedman(b, tr, y, alpha = alpha, console = FALSE)
    st <- f$statistics
    out <- data.frame(Treatment = rownames(f$means), Median = f$means$Q50, N = f$means$r, Rank = f$means$rankSum,
                      stringsAsFactors = FALSE)
    out$Group <- trimws(as.character(f$groups[out$Treatment, "groups"]))
    test <- list(name = "Friedman", statistic = unname(st$Chisq), df = unname(st$Df), p = unname(st$p.chisq),
                 rank_label = "Rank sum")
  }
  out <- out[match(levels(tr), out$Treatment), ]
  rownames(out) <- NULL
  test$significant <- !is.na(test$p) && test$p < alpha
  if (!test$significant) out$Group <- NA_character_
  test$summary <- sprintf("%s test: chi-square = %.2f, df = %d, p %s. %s", test$name, test$statistic, test$df, .star_fmt_p(test$p),
                          if (test$significant) sprintf("The %s differ (letters compare the %s).", treatment, tolower(test$rank_label))
                          else sprintf("No significant difference among the %s.", treatment))
  structure(c(test, list(means = out, treatment = treatment, response = response, alpha = alpha)), class = "star_nonparametric")
}
