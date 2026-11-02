## New API (not part of the original STAR): structured results for a combined analysis of variance of one treatment
## factor tested in several environments (sites, seasons or years), as STAR's combAOVTest does it.
##   design = "RCBD": y ~ Env + Rep(Env) + T + T:Env. Env is tested against Rep within Env; Rep within Env, T and
##                    T:Env against the pooled error.
##   design = "CRD" : y ~ Env + T + T:Env, every effect tested against the pooled error.
## Environments are fixed effects (as in STAR). Mean separation follows STAR: with a significant T x Env interaction
## the treatments are compared within each environment and the environments within each treatment (pooled error);
## otherwise the significant main effects are compared (T with the pooled error, Env with Rep within Env in an RCBD).
## Each environment is also analysed on its own, and the error variances of the environments are compared with
## Bartlett's test (Gomez & Gomez, 1984, chapter 8): the pooled error is valid only when they are homogeneous.
## The layout must be complete; an RCBD must also be balanced (each environment x block x treatment exactly once).

star_combined <- function(data, response, treatment, env, block = NULL, design = c("RCBD", "CRD"),
                          test = c("auto", "LSD", "HSD", "duncan", "SNK", "scheffe", "bonferroni"), alpha = 0.05,
                          report = TRUE) {
  design <- match.arg(design)
  test <- match.arg(test)
  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  if (length(treatment) != 1) stop("A combined analysis needs exactly one treatment factor.", call. = FALSE)
  if (length(env) != 1 || !nzchar(env)) stop("A combined analysis needs an environment column (site, season or year).", call. = FALSE)
  if (design == "RCBD" && (is.null(block) || !nzchar(block))) stop("A combined RCBD analysis needs a block (replication) column.", call. = FALSE)
  if (design == "CRD") block <- NULL
  .star_check_columns(data, c(response, treatment, env, block), "Response, treatment, environment and block")
  factors <- c(treatment, env)
  d <- star_prepare_data(data, response, factors, block)
  data_notes <- attr(d, "notes")
  if (nlevels(d[[env]]) < 2) stop("The environment column '", env, "' needs at least two levels.", call. = FALSE)
  if (anyNA(d[[response]])) {
    data_notes <- c(data_notes, sprintf("The %d row(s) with a missing response were left out of the analysis.", sum(is.na(d[[response]]))))
    d <- d[!is.na(d[[response]]), , drop = FALSE]
  }
  if (any(table(d[c(env, treatment)]) == 0)) stop("Every treatment must appear in every environment.", call. = FALSE)
  if (design == "RCBD" && any(table(d[c(env, block, treatment)]) != 1)) {
    stop("The layout is not complete and balanced: each environment x block x treatment combination must appear exactly once.",
         if (any(table(d[c(env, block, treatment)]) == 0)) " Some are missing (a missing yield also counts); estimate or remove them and try again." else "",
         call. = FALSE)
  }
  y <- d[[response]]

  bt <- \(x) paste0("`", x, "`")
  rhs <- c(bt(env), if (design == "RCBD") paste0(bt(env), ":", bt(block)), bt(treatment), paste0(bt(treatment), ":", bt(env)))
  fit <- stats::aov(stats::as.formula(paste(bt(response), "~", paste(rhs, collapse = " + "))), data = d)
  s <- summary(fit)[[1]]
  nm <- gsub("`", "", trimws(rownames(s)), fixed = TRUE)
  get <- \(src) s[match(src, nm), ]
  rep_lab <- if (design == "RCBD") paste(block, "within", env)
  inter_name <- paste(treatment, env, sep = ":")
  src_fit <- c(env, if (design == "RCBD") paste(env, block, sep = ":"), treatment, paste(treatment, env, sep = ":"), "Residuals")
  if (anyNA(match(src_fit, nm))) {
    # aov names an interaction after the order of the factors in the data; try the other order
    src_fit <- c(env, if (design == "RCBD") paste(block, env, sep = ":"), treatment, paste(env, treatment, sep = ":"), "Residuals")
    if (design == "RCBD" && is.na(match(src_fit[2], nm))) src_fit[2] <- paste(env, block, sep = ":")
    if (anyNA(match(src_fit, nm))) stop("STAR could not fit the combined model.", call. = FALSE)
  }
  tab <- get(src_fit)
  anova <- data.frame(
    Source = c(env, rep_lab, treatment, inter_name, "Error"), DF = tab$Df, SS = tab$`Sum Sq`, MS = tab$`Mean Sq`,
    F = tab$`F value`, P = tab$`Pr(>F)`, row.names = NULL, stringsAsFactors = FALSE
  )
  anova$F[nrow(anova)] <- anova$P[nrow(anova)] <- NA_real_
  err <- anova[nrow(anova), ]
  if (design == "RCBD") {
    rep_row <- anova[anova$Source == rep_lab, ]
    anova$F[1] <- anova$MS[1] / rep_row$MS
    anova$P[1] <- stats::pf(anova$F[1], anova$DF[1], rep_row$DF, lower.tail = FALSE)
  } else {
    rep_row <- err
  }
  anova <- rbind(anova, data.frame(Source = "Total", DF = sum(anova$DF), SS = sum(anova$SS), MS = NA_real_,
                                   F = NA_real_, P = NA_real_, stringsAsFactors = FALSE))

  effects <- anova[match(c(treatment, env, inter_name), anova$Source), c("Source", "DF", "F", "P")]
  names(effects)[1] <- "Effect"
  effects$Significant <- !is.na(effects$P) & effects$P < alpha
  den_ss <- c(err$SS, rep_row$SS, err$SS)
  ss <- anova$SS[match(effects$Effect, anova$Source)]
  effects$PartialEta2 <- ss / (ss + den_ss)
  effects$ErrDF <- c(err$DF, rep_row$DF, err$DF)
  rownames(effects) <- NULL

  grand_mean <- mean(y)
  cv <- 100 * sqrt(err$MS) / grand_mean
  interaction_sig <- isTRUE(effects$Significant[3])
  dfe <- setNames(c(err$DF, rep_row$DF), factors)
  mse <- setNames(c(err$MS, rep_row$MS), factors)
  mm <- .star_main_means(y, d, factors, test, dfe, mse, alpha, interaction_sig)
  means <- mm$means
  tests_used <- mm$tests
  cells <- .star_cell_means(y, d, treatment, env)
  # simple effects use the pooled error for both factors, as STAR does
  simple <- if (interaction_sig) .star_simple_effects(y, d, factors, test, setNames(rep(err$DF, 2), factors), setNames(rep(err$MS, 2), factors), alpha)

  by_env <- .star_combined_by_env(d, response, treatment, env, block, design, alpha)
  bartlett <- .star_bartlett(by_env$ErrorMS, by_env$ErrorDF)

  diagnostics <- tryCatch({
    dd <- d
    if (design == "RCBD") dd$.block_in_env <- interaction(dd[[env]], dd[[block]], drop = TRUE)
    star_diagnostics(dd, response, factors, if (design == "RCBD") ".block_in_env", alpha)
  }, error = \(e) NULL)

  summary_text <- paste(
    .star_aov_summary(anova, effects, means, factors, response, interaction_sig, tests_used, alpha, cv),
    .star_bartlett_text(bartlett, alpha)
  )
  apa <- .star_apa(effects, err, response)
  d <- d[, c(response, treatment, env, block)]
  report <- if (report) .star_combined_report(d, response, treatment, env, block, design, test, alpha)

  structure(
    list(
      anova = anova, effects = effects, means = means, cells = cells, simple = simple,
      grand_mean = grand_mean, cv = cv, test = test, tests_used = tests_used,
      alpha = alpha, design = paste0("Comb", design), response = response, treatment = treatment, factors = factors,
      env = env, block = block, p_treatment = effects$P[1], significant = effects$Significant[1],
      interaction_significant = interaction_sig, by_env = by_env, bartlett = bartlett,
      error_terms = setNames(c("Error", if (design == "RCBD") rep_lab else "Error"), factors),
      summary = summary_text, apa = apa, report = report, data_notes = data_notes, diagnostics = diagnostics, data = d
    ),
    class = "star_aov"
  )
}

# the analysis of each environment on its own: mean, CV, error mean square and the F test of the treatments
.star_combined_by_env <- function(d, response, treatment, env, block, design, alpha) {
  rows <- lapply(levels(d[[env]]), \(lv) {
    sub <- droplevels(d[d[[env]] == lv, , drop = FALSE])
    fit <- stats::aov(.star_aov_formula(response, treatment, block), data = sub)
    s <- summary(fit)[[1]]
    i <- match(treatment, gsub("`", "", trimws(rownames(s)), fixed = TRUE))
    e <- nrow(s)
    m <- mean(sub[[response]])
    data.frame(
      Environment = lv, N = nrow(sub), Mean = m, CV = 100 * sqrt(s$`Mean Sq`[e]) / m,
      ErrorMS = s$`Mean Sq`[e], ErrorDF = s$Df[e], F = s$`F value`[i], P = s$`Pr(>F)`[i],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$Significant <- !is.na(out$P) & out$P < alpha
  names(out)[1] <- env
  out
}

# Bartlett's test of homogeneity of the error variances of the environments (from their mean squares and df)
.star_bartlett <- function(ms, df) {
  ok <- is.finite(ms) & ms > 0 & df > 0
  ms <- ms[ok]
  df <- df[ok]
  k <- length(ms)
  if (k < 2) {
    return(list(statistic = NA_real_, df = NA_real_, p = NA_real_, ratio = NA_real_))
  }
  pooled <- sum(df * ms) / sum(df)
  m <- sum(df) * log(pooled) - sum(df * log(ms))
  cc <- 1 + (sum(1 / df) - 1 / sum(df)) / (3 * (k - 1))
  chi <- m / cc
  list(statistic = chi, df = k - 1, p = stats::pchisq(chi, k - 1, lower.tail = FALSE), ratio = max(ms) / min(ms))
}

.star_bartlett_text <- function(b, alpha) {
  if (is.na(b$p)) {
    return("")
  }
  if (b$p < alpha) {
    sprintf("The error variances of the environments differ (Bartlett chi-square = %.2f, df = %d, p %s; largest / smallest = %.1f), so the pooled error may not be valid: consider analysing each environment separately, or a transformation.",
            b$statistic, b$df, .star_fmt_p(b$p), b$ratio)
  } else {
    sprintf("The error variances of the environments are homogeneous (Bartlett p %s), so they can be pooled.", .star_fmt_p(b$p))
  }
}

.star_combined_report <- function(d, response, treatment, env, block, design, test, alpha) {
  args <- list(data = d, design = design, respvar = response, factor1 = treatment, set = env, sig = alpha)
  if (design == "RCBD") args$rep1 <- block
  if (!test %in% c("auto", "bonferroni")) {
    args$pwTest <- test
    args$pwVar <- response
  }
  tryCatch(utils::capture.output(do.call(combAOVTest, args)),
           error = \(e) "The classic STAR report could not be produced for this data.")
}
