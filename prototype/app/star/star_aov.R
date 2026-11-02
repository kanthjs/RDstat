## New API (not part of the original STAR): structured results for a one- or two-factor analysis of variance.
## The original functions print text; these wrap them and return data frames so that an app can
## render tables, draw plots and export to Excel.
##   design = "CRD"  : completely randomized design (no blocks)
##   design = "RCBD" : randomized complete block design (`block` required)
##   treatment       : one factor, or two factors (factorial layout with interaction)
## Mean separation follows STAR: with a significant interaction the simple effects are compared (each factor at
## each level of the other); otherwise the significant main effects are compared.
## `report = FALSE` skips the classic STAR printout (the slowest step); star_report() builds it later on demand.

star_aov <- function(data, response, treatment, block = NULL, design = c("CRD", "RCBD"),
                     test = c("auto", "LSD", "HSD", "duncan", "SNK", "scheffe", "bonferroni"), alpha = 0.05,
                     report = TRUE) {
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
  # that model is fitted directly here. Otherwise STAR's own procedure is run: it estimates missing values,
  # adjusts degrees of freedom and refuses layouts it cannot analyse.
  if (design == "CRD" || (!anyNA(y) && .star_is_balanced(d[c(block, factors)]))) {
    raw <- ConstructAOVTable(summary(stats::aov(.star_aov_formula(response, factors, block), data = d)))
    if (anyNA(y)) data_notes <- c(data_notes, sprintf("The %d row(s) with a missing response were left out of the analysis.", sum(is.na(y))))
    report <- if (report) .star_aov_report(d, response, factors, block, design, test, alpha)
  } else {
    legacy <- .star_aov_legacy(d, response, factors, block, design, test, alpha)
    res <- legacy$res
    report <- legacy$report
    err_line <- grep("^ERROR:", report, value = TRUE)
    if (length(err_line) || is.null(res$aovTable) || length(res$aovTable) == 0) {
      stop(if (length(err_line)) sub("^ERROR:[[:space:]]*", "", err_line[1]) else "STAR could not fit this model",
        if (anyNA(y)) paste0(" The response has ", sum(is.na(y)), " missing value(s); remove or fill them in and try again.") else "",
        call. = FALSE
      )
    }
    if (any(grepl("estimates of the missing values", report))) data_notes <- c(data_notes, "Missing response values were estimated by STAR's missing-data procedure (RCBD); degrees of freedom are adjusted.")
    raw <- ConstructAOVTable(res$aovTable[[1]][[1]])
  }
  anova <- data.frame(
    Source = gsub("`", "", trimws(rownames(raw)), fixed = TRUE), DF = raw[[1]], SS = raw[[2]], MS = raw[[3]],
    F = raw[[4]], P = raw[[5]], row.names = NULL, stringsAsFactors = FALSE
  )
  err <- anova[anova$Source == "Error", ]
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
      interaction_significant = interaction_sig, summary = summary_text, apa = apa, report = report,
      data_notes = data_notes, diagnostics = diagnostics, data = d
    ),
    class = "star_aov"
  )
}

# STAR's own analysis (AOVTest): the fitted tables and the classic printout
.star_aov_legacy <- function(d, response, factors, block, design, test, alpha) {
  # Let STAR choose the test per effect (LSD if <= 5 levels, else Tukey) unless the user picked one
  args <- list(data = d, design = design, respvar = response, factor1 = factors, sig = alpha)
  if (design == "RCBD") args$rep1 <- block
  if (!test %in% c("auto", "bonferroni")) {
    args$pwTest <- test
    args$pwVar <- response
  }
  report <- capture.output(res <- do.call(AOVTest, args))
  list(res = res, report = report)
}

.star_aov_report <- function(d, response, factors, block, design, test, alpha) {
  tryCatch(
    .star_aov_legacy(d, response, factors, block, design, test, alpha)$report,
    error = \(e) "The classic STAR report could not be produced for this data."
  )
}

.star_aov_formula <- function(response, factors, block = NULL) {
  bt <- \(x) if (length(x)) paste0("`", x, "`")
  rhs <- paste(c(bt(block), paste(bt(factors), collapse = " * ")), collapse = " + ")
  stats::as.formula(paste(bt(response), "~", rhs))
}

# TRUE when every combination of the columns of `cols` occurs equally often
.star_is_balanced <- function(cols) {
  counts <- table(cols)
  all(counts == counts[1])
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

## Classic STAR printout of a result; built on demand for a result made with `report = FALSE`.
star_report <- function(x) {
  if (!is.null(x$report)) {
    return(x$report)
  }
  if (is.null(x$data)) {
    return(character())
  }
  if (x$design %in% c("CombRCBD", "CombCRD")) {
    return(.star_combined_report(x$data, x$response, x$treatment, x$env, x$block, sub("^Comb", "", x$design), x$test, x$alpha))
  }
  if (x$design %in% c("SplitRCBD", "SplitCRD")) {
    return(.star_split_report(x$data, x$response, x$main, x$sub, x$block, x$test, x$alpha, x$design))
  }
  .star_aov_report(x$data, x$response, x$factors, x$block, x$design, x$test, x$alpha)
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
