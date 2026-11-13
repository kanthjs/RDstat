## New API (not part of the original STAR): repeated-measures ANOVA for plots (or pots, animals) measured several times,
## e.g. plant height at 30, 60 and 90 days. The treatment is applied to whole plots; time is the repeated factor within
## each plot, so the layout is that of a split plot in time (Gomez & Gomez 1984, ch. 6.4): treatment = main plot,
## time = sub plot, plot = block (replication) x treatment.
##   design = "RepRCBD": plots in complete blocks.      model: Block + Trt*Time + Error(Block:Trt)
##   design = "RepCRD" : plots assigned at random; `block` numbers the plots of each treatment (1, 2, 3, ...).
##                                                       model: Trt*Time + Error(Rep:Trt)
## Unlike a split plot, the times are not randomised within a plot, so measurements close in time tend to be more
## alike than those far apart, and the F tests of Time and Treatment x Time assume sphericity (equal variances of all
## differences between times). Mauchly's test checks it; when it fails, the degrees of freedom of the within-plot tests
## are multiplied by the Greenhouse-Geisser epsilon, or by the Huynh-Feldt epsilon when Greenhouse-Geisser is above 0.75
## (Girden 1992). The corrected p values decide significance (and so the letters); the ANOVA table keeps the
## uncorrected ones. Mean separation is that of the split plot (star_splitplot()).

star_repeated <- function(data, response, treatment, time, block,
                          test = c("auto", "LSD", "HSD", "duncan", "SNK", "scheffe", "bonferroni"), alpha = 0.05,
                          design = c("RepRCBD", "RepCRD"), correction = c("auto", "none", "GG", "HF")) {
  test <- match.arg(test)
  design <- match.arg(design)
  correction <- match.arg(correction)
  crd <- design == "RepCRD"
  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  if (length(treatment) != 1 || length(time) != 1) stop("A repeated-measures analysis needs exactly one treatment factor and one time factor.", call. = FALSE)
  if (is.null(block) || !nzchar(block)) {
    stop(if (crd) "A repeated-measures analysis in CRD needs a plot column (the number of each plot within its treatment: 1, 2, 3, ...)."
         else "A repeated-measures analysis in RCBD needs a block (replication) column.", call. = FALSE)
  }
  .star_check_columns(data, c(response, treatment, time, block), paste("Response, treatment, time and", if (crd) "plot" else "block"))
  # times in time order (D30 before D120), not alphabetical; a factor keeps the order it was given unless that is
  # just the alphabetical order of read.csv(stringsAsFactors = TRUE)
  tf <- data[[time]]
  if (!is.factor(tf) || identical(levels(tf), sort(levels(tf)))) {
    tv <- trimws(star_squash_space(as.character(data[[time]])))
    tv[star_is_blank(tv)] <- NA
    data[[time]] <- factor(tv, levels = .star_time_order(tv))
  }
  d <- star_prepare_data(data, response, c(treatment, time), block)
  if (any(is.na(d[[response]]))) {
    stop("The response has ", sum(is.na(d[[response]])), " missing value(s); the repeated-measures analysis needs every plot measured at every time. Remove the plots with missing values or fill them in and try again.", call. = FALSE)
  }
  if (nlevels(d[[time]]) < 2) stop("The time factor needs at least two times.", call. = FALSE)
  cnt <- table(d[[block]], d[[treatment]], d[[time]])
  if (any(cnt != 1)) {
    stop("The layout is not complete: each plot (", if (crd) "plot number" else "block", " x ", treatment, ") must have exactly one value at each level of ", time, ".", call. = FALSE)
  }

  sph <- .star_sphericity(d, response, treatment, time, block, alpha, correction)
  sp_design <- sub("^Rep", "Split", design)
  r <- star_splitplot(data, response, treatment, time, block, test = test, alpha = alpha, design = sp_design,
                      within_p = if (sph$correction != "none") stats::setNames(sph$tests$P_used, sph$tests$Effect))
  # the F values of the within-plot tests, from the ANOVA table (sphericity does not change them)
  sph$tests$F <- r$anova$F[match(sph$tests$Effect, r$anova$Source)]

  r$design <- design
  r$time <- time
  r$sphericity <- sph
  r$summary <- .star_rep_summary(r)
  r$apa <- .star_rep_apa(r)
  r
}

## Order of the times: by the number in each label when every label has a different one ("D30", "60 DAP", "Week 2",
## "30 วัน"), otherwise the order in which they first appear in the file ("Before", "After").
.star_time_order <- function(v) {
  lab <- unique(trimws(star_squash_space(as.character(v[!star_is_blank(v)]))))
  num <- suppressWarnings(as.numeric(regmatches(lab, regexpr("-?[0-9]+([.][0-9]+)?", lab))))
  if (length(num) == length(lab) && !anyDuplicated(num)) lab[order(num)] else lab
}

## Mauchly's test and the Greenhouse-Geisser / Huynh-Feldt epsilons for the within-plot effects. The plots x times
## matrix is turned into orthonormal contrasts among the times; their residual SSCP after removing the treatment means
## (blocks are pooled with the plots, as in the split-plot Error(b)) gives the epsilons. Its trace is SS Error(b).
.star_sphericity <- function(d, response, treatment, time, block, alpha, correction = "auto") {
  plot_id <- interaction(d[[block]], d[[treatment]], drop = TRUE, sep = " / ")
  w <- tapply(d[[response]], list(plot_id, d[[time]]), function(v) v[1])
  trt <- d[[treatment]][match(rownames(w), as.character(plot_id))]
  p <- ncol(w); k <- p - 1
  h <- stats::contr.helmert(p)
  h <- sweep(h, 2, sqrt(colSums(h^2)), "/")
  fit <- stats::lm(w %*% h ~ trt)
  rdf <- fit$df.residual
  e <- crossprod(as.matrix(stats::residuals(fit)))
  if (k == 1) {
    gg <- 1; hf <- 1; mauchly <- list(W = NA_real_, statistic = NA_real_, df = NA_real_, p = NA_real_)
  } else {
    gg <- sum(diag(e))^2 / (k * sum(diag(e %*% e)))
    hf <- if (rdf > k * gg) min(1, ((rdf + 1) * k * gg - 2) / (k * (rdf - k * gg))) else NA_real_
    mt <- tryCatch(stats::mauchly.test(stats::lm(w ~ trt), X = ~1), error = function(err) NULL)
    mauchly <- if (is.null(mt)) list(W = NA_real_, statistic = NA_real_, df = NA_real_, p = NA_real_)
               else {
                 # chi-square approximation (Mauchly 1940), only for the report; the p value is mauchly.test()'s
                 rho <- 1 - (2 * k^2 + k + 2) / (6 * k * rdf)
                 list(W = unname(mt$statistic), statistic = -rdf * rho * log(unname(mt$statistic)), df = k * (k + 1) / 2 - 1, p = mt$p.value)
               }
  }
  if (correction == "auto") {
    correction <- if (k == 1 || is.na(mauchly$p) && gg >= 1 - 1e-12) "none"
                  else if (is.na(mauchly$p)) "GG"   # too few plots to test sphericity: be conservative
                  else if (mauchly$p >= alpha) "none"
                  else if (!is.na(hf) && gg > 0.75) "HF" else "GG"
  }
  if (correction == "HF" && is.na(hf)) correction <- "GG"
  inter <- paste(treatment, time, sep = ":")
  a <- nlevels(d[[treatment]])
  df1 <- c(k, (a - 1) * k); df2 <- rep(rdf * k, 2)
  tests <- data.frame(Effect = c(time, inter), F = NA_real_, DF1 = df1, DF2 = df2, stringsAsFactors = FALSE)
  # F from the per-plot contrasts, so that the p values can be computed here; equal to the ANOVA table's F
  fit0 <- stats::lm(w %*% h ~ 1)
  ss_time <- sum(colSums(w %*% h)^2) / nrow(w)
  ss_inter <- sum(diag(crossprod(as.matrix(stats::residuals(fit0))))) - sum(diag(e))
  mse <- sum(diag(e)) / (rdf * k)
  tests$F <- c(ss_time / k, ss_inter / ((a - 1) * k)) / mse
  pf <- function(eps) stats::pf(tests$F, df1 * eps, df2 * eps, lower.tail = FALSE)
  tests$P <- pf(1); tests$P_GG <- pf(gg); tests$P_HF <- if (is.na(hf)) NA_real_ else pf(hf)
  eps_used <- switch(correction, none = 1, GG = gg, HF = hf)
  tests$P_used <- pf(eps_used)
  list(mauchly = mauchly, gg = gg, hf = hf, correction = correction, epsilon = eps_used, tests = tests,
       n_plots = nrow(w), n_times = p, df_resid = rdf, ss_error = sum(diag(e)))
}

.star_rep_correction_name <- c(none = "none (sphericity assumed)", GG = "Greenhouse-Geisser", HF = "Huynh-Feldt")

.star_sphericity_text <- function(s, alpha) {
  m <- s$mauchly
  if (s$n_times == 2) return("With two times there is only one difference between times, so sphericity holds and no correction is needed.")
  test_txt <- if (is.na(m$p)) "Mauchly's test could not be computed (too few plots for the number of times)"
              else sprintf("Mauchly's W = %.3f, p %s", m$W, .star_fmt_p(m$p))
  eps_txt <- sprintf("Greenhouse-Geisser epsilon = %.3f, Huynh-Feldt epsilon = %s", s$gg, if (is.na(s$hf)) "NA" else sprintf("%.3f", s$hf))
  verdict <- switch(s$correction,
    none = if (is.na(m$p)) "no correction was applied." else sprintf("sphericity is not rejected at alpha = %s, so the usual F tests are used.", format(alpha)),
    GG = sprintf("%s, so the within-plot tests use the Greenhouse-Geisser correction.", if (is.na(m$p)) "the epsilons are used as a precaution" else "sphericity is rejected"),
    HF = "sphericity is rejected and Greenhouse-Geisser epsilon is above 0.75, so the within-plot tests use the Huynh-Feldt correction.")
  sprintf("%s (%s); %s", test_txt, eps_txt, verdict)
}

.star_rep_summary <- function(r) {
  trt <- r$treatment; tm <- r$time; inter <- paste(trt, tm, sep = ":")
  e <- function(nm) r$effects[r$effects$Effect == nm, ]
  st <- function(nm) { x <- e(nm); sprintf("F = %.2f, p %s", x$F, .star_fmt_p(x$P)) }
  corr <- if (r$sphericity$correction == "none") "" else paste0(", ", .star_rep_correction_name[[r$sphericity$correction]], " corrected")
  parts <- if (isTRUE(r$interaction_significant)) {
    sprintf("The %s x %s interaction is significant (%s%s): the treatments change differently over time, so compare the treatments at each time and the times within each treatment rather than the overall means.",
            trt, tm, st(inter), corr)
  } else {
    c(sprintf("The %s x %s interaction is not significant (%s%s): the treatments follow similar patterns over time.", trt, tm, st(inter), corr),
      if (isTRUE(e(trt)$Significant)) sprintf("%s has a significant effect averaged over time (%s); the highest mean is %s (%s test).", trt, st(trt), .star_best(r$means[[trt]]), r$tests_used[[trt]])
      else sprintf("%s has no significant effect averaged over time (%s).", trt, st(trt)),
      if (isTRUE(e(tm)$Significant)) sprintf("%s has a significant effect (%s%s); the highest mean is at %s (%s test).", tm, st(tm), corr, .star_best(r$means[[tm]]), r$tests_used[[tm]])
      else sprintf("%s has no significant effect (%s%s).", tm, st(tm), corr))
  }
  paste(c(parts, paste("Sphericity:", .star_sphericity_text(r$sphericity, r$alpha)),
          sprintf("CV(a) = %.1f%% (between plots), CV(b) = %.1f%% (within plots).", r$cv_a, r$cv_b)), collapse = " ")
}

.star_rep_apa <- function(r) {
  nz <- function(x, d) sub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = d))
  s <- r$sphericity
  dfs <- function(x) if (abs(x - round(x)) < 1e-8) sprintf("%d", as.integer(round(x))) else sprintf("%.2f", x)
  vapply(seq_len(nrow(r$effects)), function(i) {
    e <- r$effects[i, ]
    within <- e$Effect != r$treatment
    eps <- if (within) s$epsilon else 1
    df1 <- e$DF * eps; df2 <- e$ErrDF * eps
    p <- if (is.na(e$P)) "p = NA" else if (e$P < 0.001) "p < .001" else paste("p =", nz(e$P, 3))
    lab <- if (grepl(":", e$Effect)) paste("The", sub(":", " x ", e$Effect), "interaction") else paste("The main effect of", e$Effect)
    sprintf("%s on %s was %s, F(%s, %s) = %.2f, %s, partial eta squared = %s%s.", lab, r$response,
            if (isTRUE(e$Significant)) "significant" else "not significant", dfs(df1), dfs(df2), e$F, p, nz(e$PartialEta2, 2),
            if (within && s$correction != "none") sprintf(" (%s corrected, epsilon = %s)", .star_rep_correction_name[[s$correction]], nz(eps, 2)) else "")
  }, "")
}
