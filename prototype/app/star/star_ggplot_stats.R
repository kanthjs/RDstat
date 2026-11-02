## New API (not part of the original STAR): ggplot2 plots for star_correlation() and star_regression().
## Each function returns a ggplot object, so it can be printed, modified with `+`, or saved with star_ggsave().
##   star_ggplot_correlation(x, pair)     heatmap of the correlation matrix, or the scatter plot of one pair
##   star_ggplot_regression(x)            data with the fitted line / curve and its confidence band; for several
##                                        predictors, observed against fitted values
## Both also take palette, theme and legend (see star_gg_theme()).

.star_stars <- function(p, alpha) ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < alpha, "*", ""))))

## type = "heatmap" (default) or "matrix": a scatter plot matrix with the points below the diagonal, the variable
## names on it and the coefficient above it. `pair` (two variable names) gives the scatter plot of that pair instead.
star_ggplot_correlation <- function(x, pair = NULL, main = NULL, xlab = NULL, ylab = NULL, base_size = 12,
                                    type = c("heatmap", "matrix"), palette = NULL, theme = "classic", legend = "auto", shape = "circle", heat = NULL, point_size = 1) {
  type <- match.arg(type)
  pch <- .star_gg_shape(shape)
  ac <- .star_gg_accent(palette)
  hc <- if (is.null(.star_gg_heat(heat))) c(ac[["line"]], "white", ac[["point"]]) else .star_gg_heat(heat)
  if (!inherits(x, "star_correlation")) stop("'x' must be a result of star_correlation().", call. = FALSE)
  if (is.null(pair) && type == "matrix") {
    v <- x$vars
    k <- length(v)
    g <- expand.grid(i = seq_len(k), j = seq_len(k))
    d <- x$data
    cells <- do.call(rbind, lapply(seq_len(nrow(g)), function(s) {
      vx <- v[g$j[s]]; vy <- v[g$i[s]]
      data.frame(xv = factor(vx, levels = v), yv = factor(vy, levels = v), x = d[[vx]], y = d[[vy]], kind = if (g$i[s] > g$j[s]) "lower" else if (g$i[s] < g$j[s]) "upper" else "diag")
    }))
    cells <- cells[stats::complete.cases(cells$x, cells$y), ]
    mid <- vapply(v, function(z) mean(range(d[[z]], na.rm = TRUE)), 0)
    up <- g[g$i < g$j, ]
    lab <- data.frame(xv = factor(v[up$j], levels = v), yv = factor(v[up$i], levels = v), x = mid[up$j], y = mid[up$i],
                      lab = paste0(formatC(x$r[cbind(up$i, up$j)], format = "f", digits = 2), .star_stars(x$p[cbind(up$i, up$j)], x$alpha)),
                      r = abs(x$r[cbind(up$i, up$j)]))
    dg <- data.frame(xv = factor(v, levels = v), yv = factor(v, levels = v), x = mid, y = mid, lab = v)
    p <- ggplot2::ggplot(cells, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(data = cells[cells$kind == "lower", ], shape = pch, size = 1.6 * point_size, fill = ac[["point"]], colour = "grey20", alpha = 0.8)
    if (x$method == "pearson") p <- p + ggplot2::geom_smooth(data = cells[cells$kind == "lower", ], method = "lm", formula = y ~ x, se = FALSE, colour = ac[["line"]], linewidth = 0.7)
    return(p +
      ggplot2::geom_blank(data = cells[cells$kind != "lower", ]) +
      ggplot2::geom_text(data = lab, ggplot2::aes(label = lab, size = r), fontface = "bold", show.legend = FALSE) +
      ggplot2::scale_size(range = c(3.5, 8)) +
      ggplot2::geom_text(data = dg, ggplot2::aes(label = lab), fontface = "bold", size = 5, colour = "grey25") +
      ggplot2::facet_grid(yv ~ xv, scales = "free") +
      ggplot2::labs(x = .star_gg_or(xlab, NULL), y = .star_gg_or(ylab, NULL), title = .star_gg_or(main, paste(x$method_name, "correlation: scatter plot matrix")),
                    subtitle = sprintf("Above the diagonal: %s (* p < %s, ** p < 0.01, *** p < 0.001)", x$symbol, format(x$alpha))) +
      star_gg_theme(base_size, theme, .star_gg_legend(legend)) + ggplot2::theme(strip.text = ggplot2::element_blank(), panel.border = ggplot2::element_rect(colour = "grey70", fill = NA),
                                                axis.line = ggplot2::element_blank(), panel.spacing = grid::unit(2, "pt")))
  }
  if (is.null(pair)) {
    v <- x$vars
    k <- length(v)
    g <- expand.grid(i = seq_len(k), j = seq_len(k))
    g <- g[g$i > g$j, ]                                  # lower triangle: each pair once
    g$r <- x$r[cbind(g$i, g$j)]
    g$p <- x$p[cbind(g$i, g$j)]
    g$lab <- paste0(formatC(g$r, format = "f", digits = 2), .star_stars(g$p, x$alpha))
    g$x <- factor(v[g$j], levels = v)
    g$y <- factor(v[g$i], levels = rev(v))
    return(ggplot2::ggplot(g, ggplot2::aes(x = x, y = y, fill = r)) +
      ggplot2::geom_tile(colour = "white", linewidth = 1) +
      ggplot2::geom_text(ggplot2::aes(label = lab), fontface = "bold", size = 4) +
      ggplot2::scale_fill_gradient2(low = hc[1], mid = hc[2], high = hc[3], midpoint = 0, limits = c(-1, 1),
                                    name = x$symbol) +
      ggplot2::scale_x_discrete(drop = FALSE, limits = v[-k]) +
      ggplot2::scale_y_discrete(drop = FALSE, limits = rev(v[-1])) +
      ggplot2::coord_fixed() +
      ggplot2::labs(x = .star_gg_or(xlab, NULL), y = .star_gg_or(ylab, NULL), title = .star_gg_or(main, paste(x$method_name, "correlation")),
                    subtitle = sprintf("* p < %s, ** p < 0.01, *** p < 0.001%s", format(x$alpha), if (x$adjust != "none") paste0(" (", x$adjust, "-adjusted)") else "")) +
      star_gg_theme(base_size, theme, .star_gg_legend(legend, "right")) + ggplot2::theme(axis.line = ggplot2::element_blank(),
                                                axis.ticks = ggplot2::element_blank()))
  }
  if (length(pair) != 2 || !all(pair %in% x$vars)) stop("'pair' must name two of the correlated variables.", call. = FALSE)
  d <- x$data[stats::complete.cases(x$data[pair]), pair, drop = FALSE]
  names(d) <- c("x", "y")
  row <- x$pairs[(x$pairs$Variable1 == pair[1] & x$pairs$Variable2 == pair[2]) | (x$pairs$Variable1 == pair[2] & x$pairs$Variable2 == pair[1]), ]
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y))
  if (x$method == "pearson") p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, colour = ac[["line"]], fill = ac[["line"]], alpha = 0.15, linewidth = 0.9)
  p + ggplot2::geom_point(shape = pch, size = 2.4 * point_size, fill = ac[["point"]], colour = "grey20", alpha = 0.85) +
    ggplot2::labs(x = .star_gg_or(xlab, pair[1]), y = .star_gg_or(ylab, pair[2]), title = .star_gg_or(main, paste(pair[2], "against", pair[1])),
                  subtitle = sprintf("%s = %.3f, p %s, n = %d", x$symbol, row$r, .star_fmt_p(row$PAdj), row$N)) +
    star_gg_theme(base_size, theme, .star_gg_legend(legend))
}

star_ggplot_regression <- function(x, level = 0.95, main = NULL, xlab = NULL, ylab = NULL, base_size = 12,
                                   palette = NULL, theme = "classic", legend = "auto", shape = "circle", point_size = 1) {
  ac <- .star_gg_accent(palette)
  pch <- .star_gg_shape(shape)
  th <- star_gg_theme(base_size, theme, .star_gg_legend(legend))
  if (!inherits(x, "star_regression")) stop("'x' must be a result of star_regression().", call. = FALSE)
  sub <- sprintf("%s;  R-squared = %.3f, p %s", x$equation, x$fit$r2, .star_fmt_p(x$p_model))
  if (length(x$predictors) == 1) {
    v <- x$predictors
    pts <- data.frame(x = x$data[[v]], y = x$data[[x$response]])
    grid <- data.frame(seq(min(pts$x), max(pts$x), length.out = 120)); names(grid) <- v
    pr <- star_predict(x, grid, level)
    ln <- data.frame(x = grid[[v]], y = pr$Predicted, lo = pr$Lower, hi = pr$Upper)
    p <- ggplot2::ggplot(pts, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_ribbon(data = ln, ggplot2::aes(y = y, ymin = lo, ymax = hi), fill = ac[["line"]], alpha = 0.15) +
      ggplot2::geom_line(data = ln, colour = ac[["line"]], linewidth = 1) +
      ggplot2::geom_point(shape = pch, size = 2.4 * point_size, fill = ac[["point"]], colour = "grey20", alpha = 0.85)
    if (!is.null(x$optimum) && x$optimum$inside) {
      o <- data.frame(x = x$optimum$x, y = x$optimum$y, lab = sprintf("%s at %s", x$optimum$type, .star_fmt_num(x$optimum$x)))
      p <- p + ggplot2::geom_point(data = o, shape = 23, size = 4, fill = "#F0E442", colour = "grey20") +
        ggplot2::geom_text(data = o, ggplot2::aes(label = lab), vjust = -1.2, fontface = "bold")
    }
    return(p + ggplot2::labs(x = .star_gg_or(xlab, v), y = .star_gg_or(ylab, x$response), title = .star_gg_or(main, paste(x$response, "against", v)),
                             subtitle = paste0(sub, sprintf("\nBand: %g%% confidence interval of the mean", 100 * level))) +
             th)
  }
  pts <- data.frame(x = stats::fitted(x$model), y = x$data[[x$response]])
  ggplot2::ggplot(pts, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = ac[["line"]], linewidth = 0.9) +
    ggplot2::geom_point(shape = pch, size = 2.4 * point_size, fill = ac[["point"]], colour = "grey20", alpha = 0.85) +
    ggplot2::labs(x = .star_gg_or(xlab, paste("Fitted", x$response)), y = .star_gg_or(ylab, paste("Observed", x$response)),
                  title = .star_gg_or(main, paste("Observed against fitted", x$response)),
                  subtitle = sprintf("R-squared = %.3f, p %s. Line: observed = fitted", x$fit$r2, .star_fmt_p(x$p_model))) +
    th
}
