## New API (not part of the original STAR): structured results for a one- or two-factor analysis of variance.
## Fitted with aov() and lm(); returns data frames so that an app can render tables, draw plots and export to Excel.
##   design = "CRD"  : completely randomized design (no blocks)
##   design = "RCBD" : randomized complete block design (`block` required)
##   treatment       : one factor, or two factors (factorial layout with interaction)
## Mean separation follows STAR: with a significant interaction the simple effects are compared (each factor at
## each level of the other); otherwise the significant main effects are compared.

star_aov <- function(data, response, treatment, block = NULL, design = c("CRD", "RCBD"),
                     test = c("auto", "LSD", "HSD", "duncan", "SNK", "scheffe", "bonferroni"), alpha = 0.05) {
  design <- match.arg(design)
  test <- match.arg(test)
  factors <- treatment
  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  if (length(factors) < 1) stop("Choose at least one treatment factor.")
  if (length(factors) > 2) stop("Only one or two treatment factors are supported so far.")
  if (design == "RCBD" && (is.null(block) || !nzchar(block))) stop("An RCBD analysis needs a block column.")
  if (design == "CRD") block <- NULL
  cols <- c(response, factors, block)
  .star_check_columns(data, cols, "Response, treatment factors and block")
  d <- star_prepare_data(data, response, factors, block) # coerce / clean the chosen columns, drop unusable rows
  data_notes <- attr(d, "notes")
  counts <- table(d[, factors, drop = FALSE])
  if (any(counts == 0)) stop("Some treatment combinations have no observations; the design must be complete.")

  y <- d[[response]]
  # STAR fits `response ~ block + factors` with aov() for a CRD (any layout; rows with a missing response are left
  # out) and for an RCBD in which every block x treatment cell has the same count and no response is missing, so
  # that model is fitted directly here. An RCBD with missing plots has them estimated by least squares
  # (.star_estimate_missing) and its error df reduced, as STAR does.
  if (design == "CRD" || (!anyNA(y) && .star_is_balanced(d[c(block, factors)]))) {
    raw <- .star_anova_table(summary(stats::aov(.star_aov_formula(response, factors, block), data = d)))
    if (anyNA(y)) data_notes <- c(data_notes, sprintf("The %d row(s) with a missing response were left out of the analysis.", sum(is.na(y))))
  } else {
    est <- .star_estimate_missing(d, response, factors, block)
    raw <- .star_adjust_error_df(.star_anova_table(summary(stats::aov(.star_aov_formula(response, factors, block), data = est$data))), est$df_error)
    data_notes <- c(data_notes, sprintf("%d missing response value(s) were estimated by least squares (Yates' missing-plot method: the values predicted by lm() fitted to the observed data); the error degrees of freedom are reduced by %d.", est$n_missing, est$n_missing))
  }
  anova <- data.frame(
    Source = gsub("`", "", trimws(rownames(raw)), fixed = TRUE), DF = raw[[1]], SS = raw[[2]], MS = raw[[3]],
    F = raw[[4]], P = raw[[5]], row.names = NULL, stringsAsFactors = FALSE
  )
  err <- anova[anova$Source == "Error", ]
  if (!nrow(err) || err$DF < 1) stop("There are no error degrees of freedom: each treatment needs more than one plot (replication) to test differences.", call. = FALSE)
  if (err$SS <= 1e-10 * sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)) {
    stop("The error variance is zero: every plot equals the mean of its treatment, so F and P cannot be computed. Check that the response column holds the plot values and not treatment means repeated on each row.", call. = FALSE)
  }
  n_obs <- as.data.frame(table(d[!is.na(y), factors, drop = FALSE]), stringsAsFactors = FALSE)
  single <- n_obs[n_obs$Freq == 1, factors, drop = FALSE]
  if (design == "CRD" && nrow(single)) {
    data_notes <- c(data_notes, sprintf("%d treatment(s) have a single observation (%s); their means have no replication and the comparisons involving them are weak.",
                                        nrow(single), paste(utils::head(do.call(paste, c(single, sep = " x ")), 4), collapse = ", ")))
  }
  inter_name <- if (length(factors) == 2) paste(factors, collapse = ":") else NULL
  effect_names <- c(factors, inter_name)
  effects <- anova[match(effect_names, anova$Source), c("Source", "DF", "F", "P")]
  names(effects)[1] <- "Effect"
  effects$Significant <- !is.na(effects$P) & effects$P < alpha
  ss <- anova$SS[match(effects$Effect, anova$Source)]
  effects$PartialEta2 <- ss / (ss + err$SS)
  rownames(effects) <- NULL

  grand_mean <- mean(y, na.rm = TRUE)
  cv <- 100 * sqrt(err$MS) / grand_mean

  interaction_sig <- !is.null(inter_name) && isTRUE(effects$Significant[effects$Effect == inter_name])
  dfe <- setNames(rep(err$DF, length(factors)), factors)
  mse <- setNames(rep(err$MS, length(factors)), factors)
  mm <- .star_main_means(y, d, factors, test, dfe, mse, alpha, interaction_sig)
  means <- mm$means
  tests_used <- mm$tests

  cells <- simple <- NULL
  if (length(factors) == 2) {
    cells <- .star_cell_means(y, d, factors[1], factors[2])
    if (interaction_sig) simple <- .star_simple_effects(y, d, factors, test, dfe, mse, alpha)
  }

  diagnostics <- tryCatch(star_diagnostics(data, response, factors, block, alpha), error = \(e) NULL)

  summary_text <- .star_aov_summary(anova, effects, means, factors, response, interaction_sig, tests_used, alpha, cv)

  apa <- .star_apa(effects, err, response)

  structure(
    list(
      anova = anova, effects = effects, means = means, cells = cells, simple = simple,
      grand_mean = grand_mean, cv = cv, test = test, tests_used = tests_used,
      alpha = alpha, design = design, response = response, treatment = factors[1], factors = factors,
      block = block, p_treatment = effects$P[1], significant = effects$Significant[1],
      interaction_significant = interaction_sig, summary = summary_text, apa = apa,
      data_notes = data_notes, diagnostics = diagnostics, data = d
    ),
    class = "star_aov"
  )
}

# The ANOVA table of summary(aov()): the residual row named Error, and a Total row (df and sum of squares)
.star_anova_table <- function(s) {
  t <- as.data.frame(if (inherits(s, "summary.aov")) s[[1]] else s)
  rownames(t) <- trimws(rownames(t))
  rownames(t)[nrow(t)] <- "Error"
  t[nrow(t) + 1, 1:2] <- c(sum(t[[1]]), sum(t[[2]]))
  rownames(t)[nrow(t)] <- "Total"
  t
}

.star_aov_formula <- function(response, factors, block = NULL) {
  bt <- \(x) if (length(x)) paste0("`", x, "`")
  rhs <- paste(c(bt(block), paste(bt(factors), collapse = " * ")), collapse = " + ")
  stats::as.formula(paste(bt(response), "~", rhs))
}

# Missing responses of an RCBD, estimated by least squares (Yates 1933): the values that minimise the error sum of
# squares are the predictions of the model fitted to the observed rows, so lm() + predict() gives them directly (STAR
# reached the same values by iterating aov()). Same rules as STAR: at most one row per block x treatment cell (absent
# cells are added as missing), and no more than 10% of the cells missing. Returns the completed data, the number of
# estimates and the error df of the model fitted to the observed data, which the ANOVA of the completed data uses.
.star_estimate_missing <- function(d, response, factors, block) {
  keys <- c(block, factors)
  d0 <- d # as read, before the absent plots are added
  n_cell <- table(d[keys])
  if (any(n_cell > 1)) {
    stop("The data are unbalanced: an RCBD needs one plot per treatment in each block. ", .star_layout_problem(d, keys), call. = FALSE)
  }
  absent_only <- sum(n_cell == 0) # plots with no row at all, as opposed to a row with an empty response
  if (any(n_cell == 0)) { # add the absent plots as missing values
    absent <- as.data.frame(n_cell, stringsAsFactors = FALSE)
    absent <- absent[absent$Freq == 0, keys, drop = FALSE]
    for (k in keys) absent[[k]] <- factor(absent[[k]], levels = levels(d[[k]]))
    absent[[response]] <- NA_real_
    d <- rbind(d[c(keys, response)], absent[c(keys, response)])
  }
  miss <- is.na(d[[response]])
  if (mean(miss) > 0.1) {
    # when every "missing" plot is a row the file does not have, the layout (a mistyped label, the wrong block column) is
    # the problem the user must fix, so it comes first; empty responses are reported as missing values
    if (absent_only == sum(miss)) {
      stop("The layout is not complete: an RCBD needs every treatment in every block. ", .star_layout_problem(d0, keys),
           sprintf(" (Plots with no row are treated as missing, and at most 10%% can be estimated; here %d of %d.)", sum(miss), length(miss)), call. = FALSE)
    }
    stop(sprintf("Too many missing values: %d of %d plots (more than 10%%) have no response. Missing values can be estimated for up to 10%% of the plots.", sum(miss), length(miss)),
         if (absent_only > 0) paste0(" ", absent_only, " of them have no row in the data. ", .star_layout_problem(d0, keys)) else "", call. = FALSE)
  }
  fit <- stats::lm(.star_aov_formula(response, factors, block), data = d[!miss, , drop = FALSE])
  pred <- suppressWarnings(stats::predict(fit, newdata = d[miss, , drop = FALSE]))
  if (anyNA(coef(fit)) || anyNA(pred)) {
    stop("The missing values cannot be estimated: a treatment or block has too few observed plots.", call. = FALSE)
  }
  d[[response]][miss] <- unname(pred)
  list(data = d, n_missing = sum(miss), df_error = stats::df.residual(fit))
}

# The ANOVA table of the completed data with the error df of the observed data: error MS, F and P recomputed, and the
# total df the sum of the others (as STAR does for estimated values)
.star_adjust_error_df <- function(raw, df_error) {
  e <- trimws(rownames(raw)) == "Error"; tot <- trimws(rownames(raw)) == "Total"; eff <- !e & !tot
  raw[e, 1] <- df_error
  raw[e, 3] <- raw[e, 2] / df_error
  raw[eff, 4] <- raw[eff, 3] / raw[e, 3]
  raw[eff, 5] <- stats::pf(raw[eff, 4], raw[eff, 1], df_error, lower.tail = FALSE)
  raw[tot, 1] <- sum(raw[!tot, 1])
  raw
}

# TRUE when every combination of the columns of `cols` occurs equally often
.star_is_balanced <- function(cols) {
  counts <- table(cols)
  all(counts == counts[1])
}

# What is wrong with a layout that needs each combination of `keys` exactly once, in words the user can act on: the
# combinations with no row and those with more than one (the first few of each), a label of one column found with only
# some levels of another (a typing error, or the wrong column chosen), and labels that differ only in upper/lower case.
# "" when the layout is complete.
.star_layout_problem <- function(d, keys, max_show = 4) {
  cnt <- table(d[keys])
  if (all(cnt == 1)) return("")
  cells <- as.data.frame(cnt, stringsAsFactors = FALSE)
  show <- function(rows) {
    lab <- apply(rows[keys], 1, function(r) paste(paste(keys, "=", r), collapse = ", "))
    paste0(paste0("(", utils::head(lab, max_show), ")", collapse = "; "), if (length(lab) > max_show) sprintf("; and %d more", length(lab) - max_show) else "")
  }
  out <- character()
  absent <- cells[cells$Freq == 0, , drop = FALSE]
  if (nrow(absent)) out <- c(out, sprintf("%d combination(s) have no row: %s.", nrow(absent), show(absent)))
  extra <- cells[cells$Freq > 1, , drop = FALSE]
  if (nrow(extra)) out <- c(out, sprintf("%d combination(s) have more than one row: %s.", nrow(extra), show(extra)))
  # the likeliest cause of absent cells: a few labels of one column seen with only some levels of another (a typing
  # error; the column with the fewest such labels is named), or every label so (the wrong column chosen)
  if (nrow(absent)) {
    best <- NULL; whole <- NULL
    for (k in keys) for (o in setdiff(keys, k)) {
      seen <- rowSums(table(d[[k]], d[[o]]) > 0)
      part <- names(seen)[seen < nlevels(d[[o]])]
      if (length(part) == nlevels(d[[k]])) { if (is.null(whole)) whole <- c(k, o) }
      else if (length(part) && (is.null(best) || length(part) <= length(best$part))) best <- list(k = k, o = o, part = part)
    }
    if (!is.null(best)) {
      out <- c(out, sprintf("In %s, %s appear(s) with only some levels of %s: check these labels for a typing error.",
                            best$k, paste0("'", utils::head(best$part, max_show), "'", collapse = ", "), best$o))
    } else if (!is.null(whole)) {
      out <- c(out, sprintf("No level of %s appears with every level of %s: check that the right columns are chosen (in a complete layout every block holds every treatment).",
                            whole[1], whole[2]))
    }
  }
  for (k in keys) {
    lv <- levels(d[[k]]); low <- tolower(lv)
    dup <- unique(low[duplicated(low)])
    if (length(dup)) out <- c(out, sprintf("In %s, %s differ only in upper/lower case and are read as different labels.", k,
                                           paste(vapply(dup, function(z) paste0("'", lv[low == z], "'", collapse = " and "), ""), collapse = "; ")))
  }
  paste(out, collapse = " ")
}

.star_aov_summary <- function(anova, effects, means, factors, response, interaction_sig, tests_used, alpha, cv) {
  stat <- \(nm) .star_stat(anova, nm)
  best <- \(f) .star_best(means[[f]])
  cv_txt <- sprintf("CV = %.1f%%.", cv)
  if (length(factors) == 1) {
    f <- factors
    if (effects$Significant[1]) {
      sprintf(
        "%s has a significant effect on %s (%s at alpha = %s). The highest mean is %s. Means sharing a letter are not significantly different (%s test). %s",
        f, response, stat(f), alpha, best(f), tests_used[[f]], cv_txt
      )
    } else {
      sprintf(
        "No significant effect of %s on %s (%s at alpha = %s). %s Mean separation letters are not meaningful.",
        f, response, stat(f), alpha, cv_txt
      )
    }
  } else {
    inter <- paste(factors, collapse = ":")
    parts <- character()
    if (interaction_sig) {
      parts <- c(parts, sprintf(
        "The %s x %s interaction is significant (%s), so the effect of each factor depends on the level of the other; compare means within levels (simple effects) rather than overall means.",
        factors[1], factors[2], stat(inter)
      ))
    } else {
      parts <- c(parts, sprintf("The %s x %s interaction is not significant (%s).", factors[1], factors[2], stat(inter)))
      for (f in factors) {
        parts <- c(parts, if (effects$Significant[effects$Effect == f]) {
          sprintf("%s has a significant main effect (%s); the highest mean is %s (%s test).", f, stat(f), best(f), tests_used[[f]])
        } else {
          sprintf("%s has no significant main effect (%s).", f, stat(f))
        })
      }
    }
    paste(c(parts, cv_txt), collapse = " ")
  }
}

## APA-style sentences: F(df1, df2) = x, p = .xxx, partial eta squared = .xx (no leading zero, as APA requires)
.star_apa <- function(effects, err, response) {
  nz <- function(x, d) sub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = d))
  vapply(seq_len(nrow(effects)), function(i) {
    e <- effects[i, ]
    p <- if (is.na(e$P)) "p = NA" else if (e$P < 0.001) "p < .001" else paste("p =", nz(e$P, 3))
    lab <- if (grepl(":", e$Effect)) paste("The", sub(":", " x ", e$Effect), "interaction") else paste("The main effect of", e$Effect)
    sprintf(
      "%s on %s was %s, F(%d, %d) = %.2f, %s, partial eta squared = %s.", lab, response,
      if (isTRUE(e$Significant)) "significant" else "not significant", e$DF,
      if (is.null(e$ErrDF)) err$DF else e$ErrDF, e$F, p, nz(e$PartialEta2, 2)
    )
  }, "")
}

## Kept for the RCBD-only API used earlier.
star_aov_rcbd <- function(data, response, treatment, block,
                          test = c("auto", "LSD", "HSD", "duncan", "SNK", "scheffe", "bonferroni"), alpha = 0.05) {
  star_aov(data, response, treatment, block, design = "RCBD", test = match.arg(test), alpha = alpha)
}

print.star_aov <- function(x, ...) {
  cat(x$summary, "\n\nANOVA (", x$design, ")\n", sep = "")
  print(x$anova, row.names = FALSE, digits = 4)
  for (nm in names(x$means)) {
    cat("\nMeans of ", nm, "\n", sep = "")
    print(x$means[[nm]], row.names = FALSE, digits = 4)
  }
  if (!is.null(x$cells)) {
    cat("\nCell means\n")
    print(x$cells, row.names = FALSE, digits = 4)
  }
  for (nm in names(x$simple)) {
    cat("\nSimple effects: ", nm, "\n", sep = "")
    print(x$simple[[nm]], row.names = FALSE, digits = 4)
  }
  invisible(x)
}


## Bar plots with +/- 1 SE bars and mean-separation letters.
##   effect = NULL / a factor name : bars for the means of that factor
##   effect = "interaction"        : grouped bars (two-factor results); groups = the second factor,
##                                   bars = the first factor, letters = first factor compared within each group
star_plot_means <- function(x, effect = NULL, main = NULL, col = NULL, xlab = NULL, ylab = NULL) {
  ylab_txt <- .star_gg_or(ylab, paste("Mean", x$response))
  op <- par(mar = c(5, 5, 3, 1))
  on.exit(par(op))
  if (identical(effect, "interaction") && !is.null(x$cells)) {
    f1 <- x$factors[1]
    f2 <- x$factors[2]
    cl <- x$cells
    mat <- tapply(cl$Mean, list(cl[[f1]], cl[[f2]]), sum)
    se <- tapply(cl$SE, list(cl[[f1]], cl[[f2]]), sum)
    pal <- if (is.null(col)) c("#4C78A8", "#F58518", "#54A24B", "#E45756", "#72B7B2", "#B279A2", "#EECA3B", "#9D755D")[seq_len(nrow(mat))] else col
    top <- max(mat + se, na.rm = TRUE)
    mid <- barplot(mat,
      beside = TRUE, col = pal, border = NA, ylim = c(0, top * 1.18),
      xlab = .star_gg_or(xlab, f2), ylab = ylab_txt,
      main = .star_gg_or(main, paste("Mean", x$response, "by", f1, "and", f2)),
      legend.text = rownames(mat), args.legend = list(title = f1, x = "topright", bty = "n", cex = 0.8)
    )
    arrows(mid, mat - se, mid, mat + se, angle = 90, code = 3, length = 0.04)
    sm <- x$simple[[paste(f1, "within", f2)]]
    if (!is.null(sm)) {
      for (j in seq_len(ncol(mat))) {
        for (i in seq_len(nrow(mat))) {
          g <- sm$Group[sm[[f2]] == colnames(mat)[j] & sm[[f1]] == rownames(mat)[i]]
          text(mid[i, j], mat[i, j] + se[i, j], labels = g, pos = 3, cex = 0.9)
        }
      }
    }
    mtext(
      if (is.null(sm)) {
        "Bars: mean +/- SE. Interaction not significant: no letters"
      } else {
        sprintf("Bars: mean +/- SE. Letters compare %s within each level of %s (%s test, alpha = %s)", f1, f2, attr(sm, "test"), x$alpha)
      },
      side = 3, line = 0, cex = 0.8, col = "grey40"
    )
    return(invisible(mid))
  }
  f <- if (is.null(effect)) x$factors[1] else effect
  m <- x$means[[f]]
  if (is.null(m)) stop("Unknown effect '", f, "'.")
  top <- max(m$Mean + m$SE, na.rm = TRUE)
  has_letters <- !all(is.na(m$Group)) && isTRUE(x$effects$Significant[x$effects$Effect == f])
  # many or long level names would overlap and base R then drops some of them: turn them vertical and make room
  lab <- as.character(m$Treatment)
  vertical <- length(lab) > 4 || max(nchar(lab)) > 8
  if (vertical) par(mar = c(min(12, 3.5 + 0.4 * max(nchar(lab))), 5, 3, 1))
  mid <- barplot(m$Mean,
    names.arg = lab, las = if (vertical) 2 else 1, col = if (is.null(col)) "#4C78A8" else col, border = NA, ylim = c(0, top * 1.15),
    xlab = if (vertical) "" else .star_gg_or(xlab, f), ylab = ylab_txt,
    main = .star_gg_or(main, paste("Mean", x$response, "by", f))
  )
  if (vertical) mtext(.star_gg_or(xlab, f), side = 1, line = par("mar")[1] - 1.2)
  arrows(mid, m$Mean - m$SE, mid, m$Mean + m$SE, angle = 90, code = 3, length = 0.05)
  if (has_letters) text(mid, m$Mean + m$SE, labels = m$Group, pos = 3, cex = 1.1)
  note <- if (has_letters) {
    paste("Letters:", x$tests_used[[f]], "test, alpha =", x$alpha)
  } else if (isTRUE(x$interaction_significant)) {
    "Interaction significant: compare means within levels (see interaction plot)"
  } else {
    "Effect not significant"
  }
  mtext(sprintf("Bars: mean +/- SE. %s", note), side = 3, line = 0, cex = 0.8, col = "grey40")
  invisible(mid)
}

.star_fmt_p <- function(p) if (is.na(p)) "= NA" else if (p < 1e-4) "< 0.0001" else paste("=", formatC(p, format = "f", digits = 4))
