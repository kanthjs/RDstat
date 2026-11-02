## New API (not part of the original STAR): rank-based alternative to a one-factor analysis of variance, for data that
## do not meet its assumptions (normal residuals, equal variances).
##   block = NULL : Kruskal-Wallis test (CRD), groups compared on their mean ranks
##   block given  : Friedman test (RCBD, one observation per block x treatment), groups compared on their rank sums
## Multiple comparisons are those of the agricolae package (Fisher's LSD on ranks, Conover) unless posthoc names Dunn's
## test (Kruskal-Wallis only), with Holm or Bonferroni adjusted p values; as with the ANOVA, letters are given only when
## the overall test is significant.

star_nonparametric <- function(data, response, treatment, block = NULL, alpha = 0.05, posthoc = c("conover", "dunn_holm", "dunn_bonferroni")) {
  posthoc <- match.arg(posthoc)
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
                 rank_label = "Mean rank", posthoc = "conover", posthoc_label = "Conover (LSD on ranks, agricolae)")
    if (posthoc != "conover") {
      dn <- .star_dunn(y, tr, sub("^dunn_", "", posthoc), alpha)
      out$Group <- unname(dn$letters[out$Treatment])
      test$pairs <- dn$pairs; test$posthoc <- posthoc
      test$posthoc_label <- sprintf("Dunn, %s adjusted", if (dn$adjust == "holm") "Holm" else "Bonferroni")
    }
  } else {
    b <- droplevels(d[[block]])
    if (any(table(b, tr) != 1)) stop("The Friedman test needs exactly one observation of each treatment in each block.", call. = FALSE)
    f <- agricolae::friedman(b, tr, y, alpha = alpha, console = FALSE)
    st <- f$statistics
    out <- data.frame(Treatment = rownames(f$means), Median = f$means$Q50, N = f$means$r, Rank = f$means$rankSum,
                      stringsAsFactors = FALSE)
    out$Group <- trimws(as.character(f$groups[out$Treatment, "groups"]))
    test <- list(name = "Friedman", statistic = unname(st$Chisq), df = unname(st$Df), p = unname(st$p.chisq),
                 rank_label = "Rank sum", posthoc = "conover", posthoc_label = "LSD on rank sums (agricolae)")
  }
  out <- out[match(levels(tr), out$Treatment), ]
  rownames(out) <- NULL
  test$significant <- !is.na(test$p) && test$p < alpha
  if (!test$significant) out$Group <- NA_character_
  test$summary <- sprintf("%s test: chi-square = %.2f, df = %d, p %s. %s", test$name, test$statistic, test$df, .star_fmt_p(test$p),
                          if (test$significant) sprintf("The %s differ (letters: %s, comparing the %s).", treatment, test$posthoc_label, tolower(test$rank_label))
                          else sprintf("No significant difference among the %s.", treatment))
  res <- structure(c(test, list(means = out, treatment = treatment, response = response, block = block, alpha = alpha,
                                data = data.frame(Treatment = tr, y = y, stringsAsFactors = FALSE))), class = "star_nonparametric")
  res$apa <- .star_np_apa(res)
  res
}

## APA-style sentence for a Kruskal-Wallis or Friedman test
.star_np_apa <- function(x) {
  p <- if (is.na(x$p)) "p = NA" else if (x$p < 0.001) "p < .001" else paste("p =", sub("^0\\.", ".", formatC(x$p, format = "f", digits = 3)))
  sprintf("A %s test showed that %s %s among the levels of %s, χ²(%d, N = %d) = %.2f, %s.", x$name, x$response,
          if (x$significant) "differed significantly" else "did not differ significantly", x$treatment, x$df, sum(x$means$N), x$statistic, p)
}

## box plot of the observations of each treatment, with the letters of the rank comparison above the boxes
star_ggplot_rank <- function(x, main = NULL, xlab = NULL, ylab = NULL, points = TRUE, palette = NULL, base_size = 12,
                             theme = "classic", legend = "auto", shape = "circle", point_size = 1) {
  if (!inherits(x, c("star_nonparametric", "star_trimmed"))) stop("'x' must be a result of star_nonparametric() or star_trimmed().")
  th <- star_gg_theme(base_size, theme, .star_gg_legend(legend))
  d <- x$data
  lv <- levels(d$Treatment)
  m <- x$means
  m$Treatment <- factor(m$Treatment, levels = lv)
  m$top <- as.numeric(tapply(d$y, d$Treatment, max)[as.character(m$Treatment)])
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$Treatment, y = .data$y)) +
    ggplot2::geom_boxplot(fill = .star_gg_palette(1, palette), alpha = 0.35, width = 0.6, outlier.shape = if (isTRUE(points)) NA else 19)
  if (isTRUE(points)) p <- p + ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.12, height = 0, seed = 1),
                                                   shape = .star_gg_shape(shape), size = 1.8 * point_size, fill = "white", colour = "grey20", alpha = 0.85)
  if (x$significant) p <- p + ggplot2::geom_text(data = m, ggplot2::aes(x = .data$Treatment, y = .data$top, label = .data$Group),
                                                 vjust = -0.6, fontface = "bold", inherit.aes = FALSE)
  note <- if (x$significant) sprintf("Letters: %s, alpha = %s.", x$posthoc_label, format(x$alpha))
          else "No significant difference."
  p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.12))) +
    ggplot2::labs(x = .star_gg_or(xlab, x$treatment), y = .star_gg_or(ylab, x$response),
                  title = .star_gg_or(main, paste(x$response, "by", x$treatment)),
                  subtitle = paste(strwrap(sprintf("%s, p %s. Boxes: median and quartiles%s. %s", if (inherits(x, "star_trimmed")) x$name else paste(x$name, "test"), .star_fmt_p(x$p),
                                                   if (isTRUE(points)) "; circles: observations" else "", note), 70), collapse = "\n")) +
    th + .star_gg_xlab_theme(lv)
}
