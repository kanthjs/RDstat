## New API (not part of the original STAR): ggplot2 versions of the result plots.
## These sit next to the base-graphics star_plot_means() / star_plot_diagnostics() and do not change them.
## Each function returns a ggplot object, so it can be printed, modified with `+`, or saved with star_ggsave().
##   star_ggplot_means(x, effect, data, error, points, type)  bars (or lines) of means with error bars and letters
##   star_ggplot_diagnostics(x)                               four residual panels
## All take base_size, theme and legend (see star_gg_theme()); the means plot also takes a palette (see .star_gg_palette()).
##   star_ggsave(plot, file, width, height, dpi)              PNG / SVG / PDF export
##   star_gg_font(plot)                                       Thai-capable font when the plot has Thai text
##   star_gg_fit_width(plot, width)                           subtitle wrapped to a figure `width` inches wide

## Colours for n groups. `palette` is NULL or the name of a built-in palette (see star_gg_palettes), or a vector
## of colours (recycled). The default, Okabe-Ito, is colour-blind safe; beyond 8 levels it falls back to a generated palette.
star_gg_palettes <- c("okabe", "dark2", "set2", "npg", "aaas", "lancet", "jco", "viridis", "cividis", "mako", "blues", "ylgnbu",
                      "grey", "green")
.star_gg_palette <- function(n, palette = NULL) {
  if (length(palette) > 1 || (length(palette) == 1 && !palette %in% star_gg_palettes)) return(rep_len(palette, n))
  # a fixed list of colours up to its length, a generated palette beyond that
  fixed <- function(base, gen) if (n <= length(base)) base[seq_len(n)] else grDevices::hcl.colors(n, gen)
  # n steps of a sequential hcl palette, leaving out its palest end so the lightest group stays visible
  seq_pal <- function(name, one) if (n == 1) one else grDevices::hcl.colors(n + 1, name)[seq_len(n)]
  switch(if (is.null(palette)) "okabe" else palette,
    okabe = fixed(c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#56B4E9", "#CC79A7", "#F0E442", "#999999"), "Dark 3"),
    dark2 = fixed(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666"), "Dark 2"),
    set2 = fixed(c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"), "Set 2"),
    # journal palettes as in the ggsci package (Nature, Science, The Lancet, J Clin Oncol)
    npg = fixed(c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#DC0000", "#7E6148", "#B09C85"), "Dark 3"),
    aaas = fixed(c("#3B4992", "#EE0000", "#008B45", "#631879", "#008280", "#BB0021", "#5F559B", "#A20056", "#808180", "#1B1919"), "Dark 3"),
    lancet = fixed(c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919"), "Dark 3"),
    jco = fixed(c("#0073C2", "#EFC000", "#868686", "#CD534C", "#7AA6DC", "#003C67", "#8F7700", "#3B3B3B", "#A73030", "#4A6990"), "Dark 3"),
    viridis = if (n == 1) "#21908C" else grDevices::hcl.colors(n, "viridis"),
    cividis = if (n == 1) "#7C7B78" else grDevices::hcl.colors(n, "Cividis"),
    mako = if (n == 1) "#3D9BA3" else grDevices::hcl.colors(n + 1, "Mako")[seq_len(n)],
    blues = seq_pal("Blues 3", "#2171B5"),
    ylgnbu = seq_pal("YlGnBu", "#41B6C4"),
    grey = if (n == 1) "grey55" else grDevices::gray.colors(n, start = 0.2, end = 0.85),
    green = seq_pal("Greens 3", "#2E8B57"))
}
## Point and line colours of the scatter, regression and heatmap plots, matching the palette
.star_gg_accent <- function(palette = NULL) {
  if (length(palette) > 1 || (length(palette) == 1 && !palette %in% star_gg_palettes)) return(rep_len(palette, 2))
  switch(if (is.null(palette)) "okabe" else palette,
    okabe = c(point = "#0072B2", line = "#D55E00"), dark2 = c(point = "#1B9E77", line = "#D95F02"),
    set2 = c(point = "#8DA0CB", line = "#FC8D62"), npg = c(point = "#4DBBD5", line = "#E64B35"),
    aaas = c(point = "#3B4992", line = "#EE0000"), lancet = c(point = "#00468B", line = "#ED0000"),
    jco = c(point = "#0073C2", line = "#CD534C"), viridis = c(point = "#3B528B", line = "#5DC863"),
    cividis = c(point = "#7C7B78", line = "#00204D"), mako = c(point = "#3D9BA3", line = "#0B0405"),
    blues = c(point = "#6BAED6", line = "#08306B"), ylgnbu = c(point = "#41B6C4", line = "#081D58"),
    grey = c(point = "grey60", line = "grey10"), green = c(point = "#74C476", line = "#00441B"))
}
## Diverging colours (negative, middle, positive) of the correlation heatmap. NULL keeps the palette's own colours.
star_gg_heat <- c("rdbu", "brbg", "piyg", "vik", "berlin")
.star_gg_heat <- function(heat = NULL) {
  if (is.null(heat) || !heat %in% star_gg_heat) return(NULL)
  switch(heat,
    rdbu = c("#B2182B", "#F7F7F7", "#2166AC"), brbg = c("#8C510A", "#F5F5F5", "#01665E"), piyg = c("#C51B7D", "#F7F7F7", "#4D9221"),
    vik = rev(grDevices::hcl.colors(3, "Vik")), berlin = grDevices::hcl.colors(3, "Berlin"))
}
## Point shapes, as filled R plotting symbols (outline in the point colour's dark edge, fill in the palette colour)
star_gg_shapes <- c(circle = 21, square = 22, diamond = 23, triangle = 24, triangle_down = 25)
.star_gg_shape <- function(shape = NULL) {
  if (is.numeric(shape) && length(shape) == 1) return(shape)
  s <- if (is.null(shape) || !shape %in% names(star_gg_shapes)) "circle" else shape
  unname(star_gg_shapes[[s]])
}

## theme  : "classic" (axes only), "bw" (white with grid lines and a frame), "minimal" (grid lines, no frame) or
##          "grey" (the ggplot2 default)
## legend : "top", "right", "bottom" or "none"
## family : font family for all text, including geom_text labels; defaults to getOption("star.plot_family", "")
##          ("" = the device default). The app sets it to "Roboto" after registering the font files.
star_gg_theme <- function(base_size = 12, theme = c("classic", "bw", "minimal", "grey"), legend = "top",
                          family = getOption("star.plot_family", "")) {
  theme <- match.arg(theme)
  base <- switch(theme, classic = ggplot2::theme_classic, bw = ggplot2::theme_bw, minimal = ggplot2::theme_minimal, grey = ggplot2::theme_grey)
  base(base_size = base_size, base_family = family) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   plot.subtitle = ggplot2::element_text(colour = "grey35", size = base_size * 0.8),
                   axis.text = ggplot2::element_text(colour = "grey15"),
                   legend.position = legend, legend.title = ggplot2::element_text(face = "bold"),
                   strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_text(face = "bold"))
}
## Size (in mm, for geom_text) of text drawn inside a plot, as a share of base_size, so that letters and labels
## follow the chosen font size instead of staying at ggplot2's fixed 11 pt
.star_gg_text_size <- function(base_size, rel = 0.9) rel * base_size / ggplot2::.pt
## legend = "auto" keeps the position that suits the plot
.star_gg_legend <- function(legend, auto = "top") if (is.null(legend) || identical(legend, "auto")) auto else legend

## A user-supplied text, or the default when it is NULL / empty
.star_gg_or <- function(x, default) if (is.null(x) || is.na(x[1]) || !nzchar(trimws(x[1]))) default else x[1]

## Error bar half-width from SE and N: "se", "sd" (= SE * sqrt(N)) or "ci" (95% t interval).
.star_gg_err <- function(se, n, error) {
  switch(error, se = se, sd = se * sqrt(n), ci = stats::qt(0.975, pmax(n - 1, 1)) * se)
}
.star_gg_err_label <- c(se = "mean +/- SE", sd = "mean +/- SD", ci = "mean with 95% CI")

## Long x-axis labels: rotate when they would collide
.star_gg_xlab_theme <- function(labels) {
  if (length(labels) * max(nchar(as.character(labels))) > 40)
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1)) else ggplot2::theme()
}

## effect : NULL / a factor name -> means of that factor
##          "interaction"        -> two-factor result, x = second factor, colour = first factor, as grouped bars
##          "interaction_line"   -> the same as an interaction (profile) plot with lines
## data   : the data frame given to star_aov(); only needed for points = TRUE
star_ggplot_means <- function(x, effect = NULL, data = NULL, error = c("se", "sd", "ci"), points = !is.null(data),
                              main = NULL, xlab = NULL, ylab = NULL, palette = NULL, base_size = 12,
                              theme = "classic", legend = "auto", shape = "circle", point_size = 1) {
  error <- match.arg(error)
  pch <- .star_gg_shape(shape)
  th <- star_gg_theme(base_size, theme, .star_gg_legend(legend))
  if (!inherits(x, "star_aov")) stop("'x' must be a result of star_aov() or star_splitplot().")
  inter <- !is.null(effect) && effect %in% c("interaction", "interaction_line")
  if (inter && is.null(x$cells)) stop("An interaction plot needs a two-factor result.")
  if (!inter && !is.null(effect) && is.null(x$means[[effect]])) stop("Unknown effect '", effect, "'.")
  raw <- NULL
  if (isTRUE(points)) {
    if (is.null(data)) stop("points = TRUE needs the 'data' that was analysed.")
    raw <- star_prepare_data(data, x$response, x$factors, x$block)
    raw <- raw[!is.na(raw[[x$response]]), , drop = FALSE]
  }
  y_lab <- .star_gg_or(ylab, paste("Mean", x$response))

  if (!inter) {
    f <- if (is.null(effect)) x$factors[1] else effect
    m <- x$means[[f]]
    m$Treatment <- factor(m$Treatment, levels = m$Treatment)
    m$err <- .star_gg_err(m$SE, m$N, error)
    m$lo <- m$Mean - m$err; m$hi <- m$Mean + m$err
    has_letters <- !all(is.na(m$Group)) && isTRUE(x$effects$Significant[x$effects$Effect == f])
    m$lab <- if (has_letters) m$Group else NA_character_
    p <- ggplot2::ggplot(m, ggplot2::aes(x = Treatment, y = Mean)) +
      ggplot2::geom_col(fill = .star_gg_palette(1, palette), width = 0.7, alpha = 0.9) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.2)
    top <- m$hi
    if (!is.null(raw)) {
      pts <- data.frame(Treatment = factor(as.character(raw[[f]]), levels = levels(m$Treatment)), y = raw[[x$response]])
      p <- p + ggplot2::geom_point(data = pts, ggplot2::aes(x = Treatment, y = y), inherit.aes = FALSE,
                                   position = ggplot2::position_jitter(width = 0.12, height = 0, seed = 1),
                                   shape = pch, size = 1.8 * point_size, fill = "white", colour = "grey20", alpha = 0.85)
      top <- pmax(top, tapply(pts$y, pts$Treatment, max)[as.character(m$Treatment)])
    }
    m$ytxt <- top
    if (has_letters) p <- p + ggplot2::geom_text(data = m, ggplot2::aes(y = ytxt, label = lab), vjust = -0.5, fontface = "bold", size = .star_gg_text_size(base_size))
    note <- if (has_letters) paste("Letters:", x$tests_used[[f]], "test, alpha =", x$alpha)
            else if (isTRUE(x$interaction_significant)) "Interaction significant: compare means within levels (see interaction plot)"
            else "Effect not significant"
    return(p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
             ggplot2::labs(x = .star_gg_or(xlab, f), y = y_lab, title = .star_gg_or(main, paste("Mean", x$response, "by", f)),
                           subtitle = sprintf("Bars: %s%s. %s", .star_gg_err_label[[error]], if (!is.null(raw)) "; circles: observations" else "", note)) +
             th + .star_gg_xlab_theme(levels(m$Treatment)))
  }

  # ---- interaction ----
  f1 <- x$factors[1]; f2 <- x$factors[2]
  cl <- x$cells
  cd <- data.frame(g = factor(cl[[f1]], levels = unique(cl[[f1]])), x = factor(cl[[f2]], levels = unique(cl[[f2]])),
                   Mean = cl$Mean, stringsAsFactors = FALSE)
  cd$err <- .star_gg_err(cl$SE, cl$N, error)
  cd$lo <- cd$Mean - cd$err; cd$hi <- cd$Mean + cd$err
  sm <- x$simple[[paste(f1, "within", f2)]]
  cd$lab <- NA_character_
  if (!is.null(sm)) cd$lab <- sm$Group[match(paste(cd$g, cd$x), paste(sm[[f1]], sm[[f2]]))]
  cols <- stats::setNames(.star_gg_palette(nlevels(cd$g), palette), levels(cd$g))
  dodge <- ggplot2::position_dodge(width = 0.8)
  top <- cd$hi
  pts <- NULL
  if (!is.null(raw)) {
    pts <- data.frame(g = factor(as.character(raw[[f1]]), levels = levels(cd$g)), x = factor(as.character(raw[[f2]]), levels = levels(cd$x)),
                      y = raw[[x$response]])
    mx <- tapply(pts$y, list(pts$g, pts$x), max)
    top <- pmax(top, mx[cbind(as.character(cd$g), as.character(cd$x))])
  }
  cd$ytxt <- top
  note <- if (is.null(sm)) "Interaction not significant: no letters"
          else sprintf("Letters compare %s within each level of %s (%s test, alpha = %s)", f1, f2, attr(sm, "test"), x$alpha)
  sub <- sprintf("Bars: %s%s. %s", .star_gg_err_label[[error]], if (!is.null(pts)) "; circles: observations" else "", note)
  ttl <- .star_gg_or(main, paste("Mean", x$response, "by", f1, "and", f2))

  if (identical(effect, "interaction")) {
    p <- ggplot2::ggplot(cd, ggplot2::aes(x = x, y = Mean, fill = g)) +
      ggplot2::geom_col(position = dodge, width = 0.75, alpha = 0.9) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), position = dodge, width = 0.25) +
      ggplot2::scale_fill_manual(values = cols, name = f1)
    if (!is.null(pts))
      p <- p + ggplot2::geom_point(data = pts, ggplot2::aes(x = x, y = y, group = g), inherit.aes = FALSE,
                                   position = ggplot2::position_jitterdodge(jitter.width = 0.08, dodge.width = 0.8, seed = 1),
                                   shape = pch, size = 1.6 * point_size, fill = "white", colour = "grey20", alpha = 0.85)
    p <- p + ggplot2::geom_text(ggplot2::aes(y = ytxt, label = lab, group = g), position = dodge, vjust = -0.5, fontface = "bold", size = .star_gg_text_size(base_size))
  } else {
    p <- ggplot2::ggplot(cd, ggplot2::aes(x = x, y = Mean, colour = g, group = g)) +
      ggplot2::geom_line(linewidth = 0.8, position = ggplot2::position_dodge(width = 0.15)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.12, position = ggplot2::position_dodge(width = 0.15)) +
      ggplot2::geom_point(ggplot2::aes(fill = g), shape = pch, size = 3 * point_size, colour = "grey20", position = ggplot2::position_dodge(width = 0.15)) +
      ggplot2::geom_text(ggplot2::aes(y = ytxt, label = lab), position = ggplot2::position_dodge(width = 0.15), vjust = -0.9, fontface = "bold", size = .star_gg_text_size(base_size), show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = cols, name = f1) + ggplot2::scale_fill_manual(values = cols, name = f1)
  }
  p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = .star_gg_or(xlab, f2), y = y_lab, title = ttl, subtitle = sub) +
    th + .star_gg_xlab_theme(levels(cd$x))
}

## Residual diagnostics in one faceted figure: residuals vs fitted, normal Q-Q, scale-location, histogram.
star_ggplot_diagnostics <- function(x, base_size = 11, theme = "classic", legend = "auto") {
  dg <- if (inherits(x, "star_diagnostics")) x else x$diagnostics
  if (is.null(dg)) stop("No diagnostics available for this result.")
  r <- dg$residuals
  r <- r[is.finite(r$Studentized), , drop = FALSE]
  flag_cols <- c(ok = "#0072B2", suspect = "#E69F00", outlier = "#D55E00")
  r$status <- factor(ifelse(r$Flag == "", "ok", r$Flag), levels = names(flag_cols))
  pn <- c("Residuals vs fitted", "Normal Q-Q", "Scale-location", "Histogram of residuals")
  panel <- function(i) factor(pn[i], levels = pn)

  rf <- data.frame(panel = panel(1), x = r$Fitted, y = r$Studentized, status = r$status, Row = r$Row, Flag = r$Flag)
  qq <- stats::qqnorm(r$Studentized, plot.it = FALSE)
  qs <- data.frame(panel = panel(2), x = qq$x, y = qq$y, status = r$status, Row = r$Row, Flag = r$Flag)
  sl <- data.frame(panel = panel(3), x = r$Fitted, y = sqrt(abs(r$Studentized)), status = r$status, Row = r$Row, Flag = r$Flag)
  pts <- rbind(rf, qs, sl)

  # Q-Q reference line through the quartiles
  qy <- stats::quantile(r$Studentized, c(0.25, 0.75), names = FALSE); qx <- stats::qnorm(c(0.25, 0.75))
  slope <- diff(qy) / diff(qx); icpt <- qy[1] - slope * qx[1]
  cut <- dg$outlier_cut
  hl <- data.frame(panel = panel(1), y = c(-cut, 0, cut), kind = c("cut", "zero", "cut"))
  hist_d <- data.frame(panel = panel(4), x = r$Studentized)
  lab <- pts[pts$Flag != "" & pts$panel != pn[4], , drop = FALSE]

  p <- ggplot2::ggplot(pts, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(data = hl, ggplot2::aes(yintercept = y, linetype = kind, colour = kind), show.legend = FALSE) +
    ggplot2::scale_linetype_manual(values = c(cut = "dashed", zero = "solid")) +
    ggplot2::geom_abline(data = data.frame(panel = panel(2), s = slope, i = icpt), ggplot2::aes(slope = s, intercept = i), colour = "grey50") +
    ggplot2::geom_point(ggplot2::aes(fill = status), shape = 21, size = 2, colour = "grey20", alpha = 0.9) +
    ggplot2::geom_histogram(data = hist_d, ggplot2::aes(x = x, y = ggplot2::after_stat(density)), inherit.aes = FALSE,
                            bins = max(5, min(20, ceiling(sqrt(nrow(hist_d))))), fill = "#56B4E9", colour = "white", alpha = 0.9) +
    ggplot2::stat_function(data = data.frame(panel = panel(4)), fun = stats::dnorm, inherit.aes = FALSE, colour = "grey30", linetype = "dashed") +
    ggplot2::scale_fill_manual(values = flag_cols, drop = FALSE, name = NULL) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.15)) +
    ggplot2::facet_wrap(~panel, scales = "free", ncol = 2) +
    ggplot2::labs(x = NULL, y = NULL, title = "Residual diagnostics",
                  subtitle = sprintf("Studentized residuals. Dashed lines: +/-%s (outlier cut-off); labels: %s", cut, dg$row_label %||% "row in your data")) +
    star_gg_theme(base_size, theme, .star_gg_legend(legend))
  if (nrow(lab)) p <- p + ggplot2::geom_text(data = lab, ggplot2::aes(label = Row), vjust = -0.8, size = .star_gg_text_size(base_size, 0.75), colour = "#D55E00")
  p + ggplot2::scale_colour_manual(values = c(cut = "#D55E00", zero = "grey60"), guide = "none")
}

## Thai text. The plot font (star.plot_family, Roboto in the app) has no Thai letters, and in the browser (webR)
## there is no system font to fall back on, so Thai titles, axis labels or treatment names would come out as boxes.
## star_gg_font() switches the whole plot to the font in getOption("star.plot_family_thai") when any of its text
## (labels, legend titles, or the text in its data, such as factor levels) contains Thai; otherwise the plot is unchanged.
.star_gg_has_thai <- function(p) {
  txt <- function(d) if (is.data.frame(d)) unlist(lapply(d, function(v) if (is.factor(v)) levels(v) else if (is.character(v)) unique(v)), use.names = FALSE)
  # column names in aes(): ggplot2 >= 4.0 makes the default axis and legend titles from them only when the plot is drawn
  aes_vars <- function(m) unlist(lapply(m, function(q) tryCatch(all.vars(q), error = function(e) NULL)), use.names = FALSE)
  s <- c(unlist(p$labels, use.names = FALSE), aes_vars(p$mapping), unlist(lapply(p$layers, function(l) aes_vars(l$mapping)), use.names = FALSE),
         txt(p$data), unlist(lapply(p$layers, function(l) txt(l$data)), use.names = FALSE),
         unlist(lapply(p$scales$scales, function(sc) if (is.character(sc$name)) sc$name), use.names = FALSE))
  any(grepl("[\u0E00-\u0E7F]", as.character(s)))
}
star_gg_font <- function(p, thai = getOption("star.plot_family_thai", "")) {
  if (!inherits(p, "ggplot") || is.null(thai) || !nzchar(thai) || !.star_gg_has_thai(p)) return(p)
  p + ggplot2::theme(text = ggplot2::element_text(family = thai), geom = ggplot2::element_geom(family = thai))
}
## Subtitles (the notes under the title) wrapped to the width of the figure, so that they fit an 8.5 cm journal
## figure as well as a slide. Each line of the subtitle ("\n" in it) is wrapped on its own. The plots leave their
## subtitles unwrapped; star_ggsave() and the app's Live View call this with the figure width in inches.
star_gg_fit_width <- function(p, width) {
  sub <- tryCatch(p$labels$subtitle, error = function(e) NULL)
  if (!inherits(p, "ggplot") || !is.character(sub) || length(sub) != 1 || !nzchar(sub)) return(p)
  pt <- tryCatch(p$theme$plot.subtitle$size, error = function(e) NULL)
  if (!is.numeric(pt) || length(pt) != 1) pt <- 9.6
  # an average character is about half the font size wide; about 0.45 in goes to the plot margins
  chars <- max(20, floor((width - 0.45) * 72 / (0.52 * pt)))
  lines <- unlist(lapply(strsplit(sub, "\n", fixed = TRUE)[[1]], function(l) if (nzchar(l)) strwrap(l, chars) else ""))
  p + ggplot2::labs(subtitle = paste(lines, collapse = "\n"))
}
## The font family a plot is drawn in: its own theme's, else the app default
.star_gg_family <- function(p) {
  f <- tryCatch(p$theme$text$family, error = function(e) NULL)
  if (is.character(f) && length(f) == 1 && nzchar(f)) f else getOption("star.plot_family", "")
}

## TRUE when showtext can draw `family`: adds it to sysfonts from the files systemfonts knows for it
.star_gg_showtext <- function(family) {
  if (!requireNamespace("showtext", quietly = TRUE) || !requireNamespace("systemfonts", quietly = TRUE)) return(FALSE)
  if (family %in% sysfonts::font_families()) return(TRUE)
  tryCatch({
    path <- function(w) systemfonts::match_fonts(family, weight = w)$path
    sysfonts::font_add(family, regular = path("normal"), bold = path("bold"))
    TRUE
  }, error = function(e) FALSE)
}

## Save a ggplot to PNG / SVG / PDF (chosen by the file extension). dpi matters for PNG only.
star_ggsave <- function(plot, file, width = 8, height = 5.5, dpi = 300) {
  ext <- tolower(tools::file_ext(file))
  if (!ext %in% c("png", "svg", "pdf")) stop("Use a .png, .svg or .pdf file name.")
  # PNG goes through ragg, which finds fonts registered with systemfonts. The PDF / SVG devices do not, so a custom
  # family (star.plot_family) is drawn there with showtext; the base pdf() device fails outright on an unknown family.
  plot <- star_gg_fit_width(star_gg_font(plot), width)
  fam <- .star_gg_family(plot)
  if (ext != "png" && nzchar(fam) && .star_gg_showtext(fam)) {
    dev <- if (ext == "pdf") grDevices::cairo_pdf else grDevices::svg
    dev(file, width = width, height = height, bg = "white")
    on.exit(grDevices::dev.off())
    showtext::showtext_begin()
    print(plot)
    showtext::showtext_end()
    return(invisible(file))
  }
  dev <- if (ext == "svg" && !requireNamespace("svglite", quietly = TRUE))      # base svg() device when svglite is absent
    function(filename, width, height, ...) grDevices::svg(filename, width = width, height = height) else ext
  ggplot2::ggsave(file, plot, device = dev, width = width, height = height, dpi = dpi, units = "in", bg = "white")
  invisible(file)
}
