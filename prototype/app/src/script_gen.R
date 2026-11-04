# Plain R scripts that repeat an analysis outside the app. They use only base R (stats) and, where the app does,
# agricolae / lmtest, never the star_* functions, so they run on any computer with R.

r_str <- function(x) encodeString(as.character(x), quote = "\"")
r_vec <- function(x) if (length(x) == 1) r_str(x) else paste0("c(", paste(r_str(x), collapse = ", "), ")")
r_num <- function(x) format(x, digits = 15)

script_parse_number <- c(
  "parse_number <- function(x) {",
  "  if (is.numeric(x)) return(x)",
  "  s <- trimws(gsub(intToUtf8(8203), \"\", gsub(intToUtf8(160), \" \", as.character(x), fixed = TRUE), fixed = TRUE))",
  "  s[tolower(s) %in% c(\"\", \"na\", \"n/a\", \"nan\", \"null\", \"none\", \"missing\", \"-\", \"--\", \".\")] <- NA",
  "  s <- sub(\"%$\", \"\", s)",
  "  s <- sub(\"^\\\\$\\\\s*(?=[-+]?[0-9.,])\", \"\", s, perl = TRUE)",
  "  s <- sub(\"(?<=[0-9.])\\\\s*[A-Za-z/%]+$\", \"\", s, perl = TRUE)",
  "  s <- gsub(\" \", \"\", s)",
  "  has_dot <- grepl(\".\", s, fixed = TRUE); has_com <- grepl(\",\", s, fixed = TRUE)",
  "  both <- has_dot & has_com & !is.na(s); last_comma <- both & grepl(\",[^.]*$\", s)",
  "  s[last_comma] <- gsub(\",\", \".\", gsub(\".\", \"\", s[last_comma], fixed = TRUE), fixed = TRUE)",
  "  only_com <- has_com & !has_dot & !is.na(s)",
  "  thousands <- only_com & grepl(\"^[-+]?[0-9]{1,3}(,[0-9]{3})+$\", s)",
  "  s[thousands] <- gsub(\",\", \"\", s[thousands], fixed = TRUE)",
  "  s[only_com & !thousands] <- sub(\",\", \".\", s[only_com & !thousands], fixed = TRUE)",
  "  s[both & !last_comma] <- gsub(\",\", \"\", s[both & !last_comma], fixed = TRUE)",
  "  suppressWarnings(as.numeric(s))",
  "}")

script_header <- function(title, file) {
  has_file <- !is.null(file) && !is.na(file)
  xl <- has_file && grepl("[.]xlsx?$", file, ignore.case = TRUE)
  embedded <- attr(file, "data")
  read <- if (!is.null(embedded)) c("# the example data are included here, so this script needs no data file",
                                    sprintf("d <- read.csv(text = %s, check.names = FALSE, strip.white = TRUE, colClasses = \"character\")", encodeString(embedded, quote = "\"")))
          else if (xl) c("# install.packages(\"readxl\")   # needed to read Excel files", sprintf("d <- as.data.frame(readxl::read_excel(%s, col_types = \"text\"))", r_str(file)))
          else c(sprintf("f <- %s", if (has_file) r_str(file) else "\"your_file.csv\""),
                 "# the separator is found from the first line, as the app does (comma, semicolon, tab or bar)",
                 "first <- readLines(f, n = 1, warn = FALSE)",
                 "seps <- c(\",\", \";\", \"\\t\", \"|\")",
                 "sep <- seps[which.max(sapply(seps, function(s) lengths(regmatches(first, gregexpr(s, first, fixed = TRUE)))))]",
                 "# for a Thai CSV saved by Excel add fileEncoding = \"windows-874\"",
                 "d <- read.table(f, header = TRUE, sep = sep, quote = \"\\\"\", colClasses = \"character\", comment.char = \"\", strip.white = TRUE,",
                 "                na.strings = c(\"\", \"NA\"), check.names = FALSE, fill = TRUE)")
  c(sprintf("# %s", title),
    sprintf("# Made by %s %s on %s. Set the working directory to the folder that holds the data file.", app_name, version_label(), format(Sys.Date())),
    "# If the file has title lines above the column names, add skip = <number of lines> to the read function.",
    "",
    read,
    "",
    "# numbers as the app reads them: 1,250.5  12.5%  12 kg  12,5 (decimal comma) and blanks such as NA or - are handled",
    script_parse_number)
}

# the data file for a script: the name when the user uploaded it; for a built-in example the data themselves are carried
# along (attribute "data") so the script does not depend on a file that only exists inside the app
script_file <- function(uploaded, name) {
  if (!isTRUE(uploaded) && !is.null(name) && !is.na(name) && file.exists(name))
    structure(name, data = paste(readLines(name, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  else name
}

# numeric column as the app reads it: text trimmed, then converted to a number
script_num <- function(var, col) sprintf("%s <- parse_number(d[[%s]])", var, r_str(col))

script_tt <- function(r, file = NULL) {
  alt <- r$alternative; cl <- r$conf_level
  args <- sprintf("alternative = %s, conf.level = %s", r_str(alt), r_num(cl))
  head <- script_header(paste("t-test:", r$method_name), file)
  body <- switch(r$type,
    independent = c(
      script_num("y", r$response),
      sprintf("g <- gsub(\"[[:space:]]+\", \" \", trimws(as.character(d[[%s]])))", r_str(r$group)),
      sprintf("a <- y[g == %s & !is.na(y)]   # first group", r_str(r$labels[1])),
      sprintf("b <- y[g == %s & !is.na(y)]   # second group", r_str(r$labels[2])),
      "",
      "c(n_a = length(a), mean_a = mean(a), sd_a = sd(a), n_b = length(b), mean_b = mean(b), sd_b = sd(b))",
      sprintf("tt_pooled <- t.test(a, b, var.equal = TRUE, %s)    # equal variances (Student)", args),
      sprintf("tt_welch <- t.test(a, b, var.equal = FALSE, %s)   # unequal variances (Welch)", args),
      sprintf("tt <- %s   # the test used for the conclusion", if (r$var_equal) "tt_pooled" else "tt_welch"),
      "tt",
      "var_test <- var.test(a, b)   # equality of variances",
      "var_test",
      "shapiro_safe <- function(x) if (length(x) >= 3 && length(x) <= 5000 && diff(range(x)) > 0) shapiro.test(x)   # needs 3 to 5000 values that are not all equal",
      "shapiro_a <- shapiro_safe(a); shapiro_b <- shapiro_safe(b)   # normality",
      "shapiro_a; shapiro_b",
      sprintf("wilcox <- suppressWarnings(wilcox.test(a, b, conf.int = TRUE, %s))   # rank-based alternative", args),
      "wilcox",
      "mean(a) - mean(b)                    # difference in means",
      "(mean(a) - mean(b)) / sqrt(((length(a) - 1) * var(a) + (length(b) - 1) * var(b)) / (length(a) + length(b) - 2))   # Cohen's d"),
    paired = c(
      script_num("x", r$x), script_num("y", r$y),
      "ok <- complete.cases(x, y)",
      "x <- x[ok]; y <- y[ok]",
      "dif <- x - y",
      "",
      "c(n = length(dif), mean_diff = mean(dif), sd_diff = sd(dif))",
      sprintf("tt <- t.test(x, y, paired = TRUE, %s)", args),
      "tt",
      "shapiro_safe <- function(x) if (length(x) >= 3 && length(x) <= 5000 && diff(range(x)) > 0) shapiro.test(x)   # needs 3 to 5000 values that are not all equal",
      "shapiro <- shapiro_safe(dif)   # normality of the differences",
      "shapiro",
      sprintf("wilcox <- suppressWarnings(wilcox.test(x, y, paired = TRUE, conf.int = TRUE, %s))   # rank-based alternative", args),
      "wilcox",
      "mean(dif) / sd(dif)                  # Cohen's dz"),
    one = c(
      script_num("v", r$response),
      "v <- v[!is.na(v)]",
      sprintf("mu <- %s   # the value the mean is compared with", r_num(r$mu)),
      "",
      "c(n = length(v), mean = mean(v), sd = sd(v))",
      sprintf("tt <- t.test(v, mu = mu, %s)", args),
      "tt",
      "shapiro_safe <- function(x) if (length(x) >= 3 && length(x) <= 5000 && diff(range(x)) > 0) shapiro.test(x)   # needs 3 to 5000 values that are not all equal",
      "shapiro <- shapiro_safe(v)   # normality",
      "shapiro",
      sprintf("wilcox <- suppressWarnings(wilcox.test(v, mu = mu, conf.int = TRUE, %s))   # rank-based alternative", args),
      "wilcox",
      "(mean(v) - mu) / sd(v)               # Cohen's d"))
  paste(c(head, "", body), collapse = "\n")
}

script_corr <- function(r, file = NULL) {
  head <- script_header(sprintf("%s correlation of %d variables", r$method_name, length(r$vars)), file)
  body <- c(
    sprintf("vars <- %s", r_vec(r$vars)),
    "d[vars] <- lapply(d[vars], parse_number)",
    "",
    "# one test per pair of variables, using the rows where both values are present",
    "idx <- t(combn(length(vars), 2))",
    "pairs <- do.call(rbind, lapply(seq_len(nrow(idx)), function(s) {",
    "  x <- d[[vars[idx[s, 1]]]]; y <- d[[vars[idx[s, 2]]]]",
    "  ok <- complete.cases(x, y)",
    sprintf("  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = %s, alternative = %s, exact = FALSE, conf.level = %s))",
            r_str(r$method), r_str(r$alternative), r_num(1 - r$alpha)),
    "  data.frame(Variable1 = vars[idx[s, 1]], Variable2 = vars[idx[s, 2]], r = unname(ct$estimate), N = sum(ok), P = ct$p.value, stringsAsFactors = FALSE)",
    "}))",
    sprintf("pairs$PAdj <- p.adjust(pairs$P, method = %s)   # \"none\" leaves the p values as they are", r_str(r$adjust)),
    sprintf("pairs$Significant <- pairs$PAdj < %s", r_num(r$alpha)),
    "pairs",
    "",
    "# correlation matrix",
    "r_matrix <- matrix(1, length(vars), length(vars), dimnames = list(vars, vars))",
    "for (s in seq_len(nrow(pairs))) r_matrix[pairs$Variable1[s], pairs$Variable2[s]] <- r_matrix[pairs$Variable2[s], pairs$Variable1[s]] <- pairs$r[s]",
    "round(r_matrix, 3)")
  paste(c(head, "", body), collapse = "\n")
}

script_reg <- function(r, file = NULL) {
  y <- r$response; x <- r$predictors
  rhs <- if (r$degree > 1) paste(c(sprintf("`%s`", x), sprintf("I(`%s`^%d)", x, 2:r$degree)), collapse = " + ")
         else paste(sprintf("`%s`", x), collapse = " + ")
  title <- sprintf("Linear regression of %s on %s%s", y, paste(x, collapse = ", "),
                   c("", " (quadratic)", " (cubic)")[r$degree])
  head <- script_header(title, file)
  vars <- c(y, x)
  body <- c(
    sprintf("vars <- %s", r_vec(vars)),
    "d[vars] <- lapply(d[vars], parse_number)",
    "d <- d[complete.cases(d[vars]), vars]   # rows with a missing value are left out",
    "",
    sprintf("fit <- lm(`%s` ~ %s, data = d)", y, rhs),
    "summary(fit)                         # coefficients, R-squared, F test",
    "anova(fit)                           # sequential sums of squares",
    sprintf("confint(fit, level = %s)", r_num(1 - r$alpha)),
    c("AIC(fit); BIC(fit)"),
    if (r$degree == 1 && length(x) > 1) c(
      "",
      sprintf("vif <- diag(solve(cor(d[%s])))   # variance inflation factors", r_vec(x)),
      "vif",
      sprintf("beta <- coef(fit)[-1] * sapply(d[%s], sd) / sd(d[[%s]])   # standardised coefficients", r_vec(x), r_str(y)),
      "beta"),
    if (r$degree == 2) c(
      "",
      "b <- coef(fit)",
      "x_opt <- -b[2] / (2 * b[3])           # stationary point of the fitted curve",
      "y_opt <- unname(b[1] + b[2] * x_opt + b[3] * x_opt^2)",
      sprintf("c(x = unname(x_opt), y = y_opt, inside_range = x_opt >= min(d[[%s]]) && x_opt <= max(d[[%s]]))", r_str(x), r_str(x))),
    "",
    "# assumptions",
    "shapiro_safe <- function(x) if (length(x) >= 3 && length(x) <= 5000 && diff(range(x)) > 0) shapiro.test(x)   # needs 3 to 5000 values that are not all equal",
    "shapiro <- shapiro_safe(residuals(fit))   # normality of residuals",
    "shapiro",
    "# lmtest::bptest(fit)                     # constant variance (needs the lmtest package)",
    "head(sort(abs(rstudent(fit)), decreasing = TRUE))   # largest studentised residuals (outliers if above about 3)")
  paste(c(head, "", body), collapse = "\n")
}

# CRD and RCBD with one or two factors, the split plot and the analysis combined over environments. RCBD data with a missing
# response (the app estimates the missing values) return NULL, and the screen says the script is not available.
script_aov_supported <- function(r) {
  r$design %in% c("SplitCRD", "SplitRCBD", "CombCRD", "CombRCBD") || (r$design %in% c("CRD", "RCBD") && !(r$design == "RCBD" && anyNA(r$data[[r$response]])))
}

script_compare <- function(test, y, tr, alpha, dfe = "dfe", mse = "mse") {
  a <- r_num(alpha)
  switch(test,
    LSD = sprintf("agricolae::LSD.test(%s, %s, %s, %s, %s, console = FALSE)", y, tr, dfe, mse, a),
    bonferroni = sprintf("agricolae::LSD.test(%s, %s, %s, %s, %s, p.adj = \"bonferroni\", console = FALSE)", y, tr, dfe, mse, a),
    HSD = sprintf("agricolae::HSD.test(%s, %s, %s, %s, %s, unbalanced = length(unique(table(%s))) > 1, console = FALSE)", y, tr, dfe, mse, a, tr),
    duncan = sprintf("agricolae::duncan.test(%s, %s, %s, %s, %s, console = FALSE)", y, tr, dfe, mse, a),
    SNK = sprintf("agricolae::SNK.test(%s, %s, %s, %s, %s, console = FALSE)", y, tr, dfe, mse, a),
    scheffe = sprintf("agricolae::scheffe.test(%s, %s, %s, %s, Fc = NA_real_, alpha = %s, console = FALSE)", y, tr, dfe, mse, a))
}

script_aov <- function(r, file = NULL) {
  if (!script_aov_supported(r)) return(NULL)
  if (r$design %in% c("SplitCRD", "SplitRCBD")) return(script_split(r, file))
  if (r$design %in% c("CombCRD", "CombRCBD")) return(script_combined(r, file))
  f <- r$factors; blk <- r$block
  resp <- if (!is.null(r$transform)) r$original_response else r$response
  title <- sprintf("%s of %s by %s%s", r$design, resp, paste(f, collapse = " x "), if (!is.null(blk)) paste(", block", blk) else "")
  head <- script_header(title, file)
  trans <- if (!is.null(r$transform)) c(
    sprintf("y <- switch(%s, log = log(y + 1), sqrt = sqrt(y + 0.5), arcsin = asin(sqrt(y / 100)) * 180 / pi)   # transformed response", r_str(r$transform))
  )
  bt <- function(x) sprintf("`%s`", x)
  fml <- paste(c(if (!is.null(blk)) bt(blk), paste(bt(f), collapse = " * ")), collapse = " + ")
  y <- "d[[\"y\"]]"
  body <- c(
    "# mean comparisons use the agricolae package: install.packages(\"agricolae\")",
    sprintf("y <- parse_number(d[[%s]])", r_str(resp)),
    trans,
    "d$y <- y",
    sprintf("for (v in %s) d[[v]] <- factor(trimws(as.character(d[[v]])))", r_vec(c(f, blk))),
    sprintf("d <- d[complete.cases(d[c(\"y\", %s)]), ]   # rows with a missing value are left out", paste(r_str(c(f, blk)), collapse = ", ")),
    "",
    sprintf("fit <- aov(y ~ %s, data = d)", fml),
    "summary(fit)                         # analysis of variance table",
    "dfe <- df.residual(fit); mse <- deviance(fit) / dfe   # error df and mean square",
    "cv <- 100 * sqrt(mse) / mean(d$y)    # coefficient of variation (%)",
    "",
    "# mean comparisons: use them for effects that are significant",
    if (length(f) == 2) "# (with two factors, compare the main effects only when the interaction is not significant)",
    "cmp <- list()",
    unlist(lapply(f, function(v) sprintf("cmp[[%s]] <- %s", r_str(v), script_compare(r$tests_used[[v]], "d$y", sprintf("d[[%s]]", r_str(v)), r$alpha)))),
    sprintf("cmp   # means and letters; %s", "see cmp[[\"<factor>\"]]$groups"))
  if (length(f) == 2 && isTRUE(r$interaction_significant)) {
    sim <- unlist(lapply(rev(seq_along(f)), function(i) {
      cmp_f <- f[i]; at_f <- f[-i]
      c(sprintf("for (lv in levels(d[[%s]])) {", r_str(at_f)),
        sprintf("  s <- d[d[[%s]] == lv, ]", r_str(at_f)),
        sprintf("  simple[[paste(%s, \"within\", %s, \"=\", lv)]] <- %s", r_str(cmp_f), r_str(at_f),
                script_compare(r$tests_used[[cmp_f]], "s$y", sprintf("droplevels(s[[%s]])", r_str(cmp_f)), r$alpha)),
        "}")
    }))
    body <- c(body, "", "# the interaction is significant, so each factor is compared within the levels of the other", "simple <- list()", sim, "simple")
  }
  paste(c(head, "", body), collapse = "\n")
}

# Kruskal-Wallis (CRD) or Friedman (RCBD) test with the rank comparisons of agricolae, as the app runs them
script_np <- function(np, file = NULL) {
  f <- np$treatment; blk <- np$block
  head <- script_header(sprintf("%s test of %s by %s%s", np$name, np$response, f, if (!is.null(blk)) paste(", block", blk) else ""), file)
  body <- c(
    "# rank-based tests use the agricolae package: install.packages(\"agricolae\")",
    script_num("y", np$response),
    "d$y <- y",
    sprintf("for (v in %s) d[[v]] <- factor(trimws(as.character(d[[v]])))", r_vec(c(f, blk))),
    sprintf("d <- d[complete.cases(d[c(\"y\", %s)]), ]   # rows with a missing value are left out", paste(r_str(c(f, blk)), collapse = ", ")),
    "",
    if (is.null(blk)) sprintf("np <- agricolae::kruskal(d$y, d[[%s]], alpha = %s, console = FALSE)   # Kruskal-Wallis", r_str(f), r_num(np$alpha))
    else c("# Friedman: each treatment exactly once in every block; the values are ranked within each block",
           sprintf("np <- agricolae::friedman(d[[%s]], d[[%s]], d$y, alpha = %s, console = FALSE)", r_str(blk), r_str(f), r_num(np$alpha))),
    "np$statistics   # chi-square, df and p value (p.chisq)",
    "np$means        # medians (Q50) and ranks",
    if (np$posthoc == "conover") "np$groups       # letters: use them only when the test is significant"
    else c("",
           sprintf("# Dunn's test of the mean ranks, ties corrected, p values adjusted with the %s method", sub("^dunn_", "", np$posthoc)),
           sprintf("g <- d[[%s]]; r <- rank(d$y); N <- length(r); n <- table(g); rb <- tapply(r, g, mean)", r_str(f)),
           "tie <- sum(table(d$y)^3 - table(d$y)) / (12 * (N - 1))",
           "pr <- combn(levels(g), 2)",
           "z <- apply(pr, 2, function(ab) (rb[ab[1]] - rb[ab[2]]) / sqrt((N * (N + 1) / 12 - tie) * (1 / n[ab[1]] + 1 / n[ab[2]])))",
           sprintf("dunn <- data.frame(Comparison = paste(pr[1, ], \"-\", pr[2, ]), z = z, P.adj = p.adjust(2 * pnorm(-abs(z)), %s))", r_str(sub("^dunn_", "", np$posthoc))),
           "dunn   # pairs with P.adj below alpha differ; use them only when the test is significant"))
  paste(c(head, "", body), collapse = "\n")
}

# Welch's one-way ANOVA with the Games-Howell comparisons, as the app runs them (on the transformed values if any)
script_welch <- function(w, r, file = NULL) {
  f <- w$treatment
  resp <- r$original_response %||% r$response
  head <- script_header(sprintf("Welch's ANOVA of %s by %s", resp, f), file)
  trans <- if (!is.null(r$transform)) sprintf("y <- switch(%s, log = log(y + 1), sqrt = sqrt(y + 0.5), arcsin = asin(sqrt(y / 100)) * 180 / pi)   # transformed response", r_str(r$transform))
  body <- c(
    script_num("y", resp),
    trans,
    "d$y <- y",
    sprintf("d$g <- factor(trimws(as.character(d[[%s]])))", r_str(f)),
    "d <- d[complete.cases(d[c(\"y\", \"g\")]), ]   # rows with a missing value are left out",
    "",
    "welch <- oneway.test(y ~ g, data = d, var.equal = FALSE)   # Welch's ANOVA: no equal-variance assumption",
    "welch",
    "",
    "# Games-Howell comparisons (each pair with its own variances and Welch df; studentized range distribution)",
    "n <- tapply(d$y, d$g, length); m <- tapply(d$y, d$g, mean); v <- tapply(d$y, d$g, var); k <- nlevels(d$g)",
    "pr <- combn(levels(d$g), 2)",
    "gh <- do.call(rbind, lapply(seq_len(ncol(pr)), function(j) {",
    "  a <- pr[1, j]; b <- pr[2, j]; se2 <- v[a] / n[a] + v[b] / n[b]",
    "  df <- se2^2 / ((v[a] / n[a])^2 / (n[a] - 1) + (v[b] / n[b])^2 / (n[b] - 1)); t <- (m[a] - m[b]) / sqrt(se2)",
    "  data.frame(Comparison = paste(a, \"-\", b), Difference = m[a] - m[b], t = t, DF = df, P = ptukey(abs(t) * sqrt(2), k, df, lower.tail = FALSE))",
    "}))",
    "rownames(gh) <- NULL",
    "gh   # pairs with P below alpha differ; use them only when Welch's test is significant")
  paste(c(head, "", body), collapse = "\n")
}

# Split plot: whole plots are block x main-plot level; Error(a) tests the main-plot factor, Error(b) the sub-plot
# factor and the interaction. Sub-plot means use Error(b), main-plot means Error(a); with a significant interaction the
# main factor is compared within sub-plot levels with the pooled error and Satterthwaite df, as in the app.
script_split <- function(r, file = NULL) {
  main <- r$main; sub <- r$sub; blk <- r$block; crd <- r$design == "SplitCRD"
  resp <- if (!is.null(r$transform)) r$original_response else r$response
  head <- script_header(sprintf("Split plot (%s) of %s: main plot %s, sub plot %s", if (crd) "CRD" else "RCBD", resp, main, sub), file)
  trans <- if (!is.null(r$transform)) sprintf("y <- switch(%s, log = log(y + 1), sqrt = sqrt(y + 0.5), arcsin = asin(sqrt(y / 100)) * 180 / pi)   # transformed response", r_str(r$transform))
  bt <- function(x) sprintf("`%s`", x)
  fml <- if (crd) sprintf("y ~ %s * %s + Error(.wp)", bt(main), bt(sub)) else sprintf("y ~ %s + %s * %s + Error(.wp)", bt(blk), bt(main), bt(sub))
  body <- c(
    "# mean comparisons use the agricolae package: install.packages(\"agricolae\")",
    sprintf("y <- parse_number(d[[%s]])", r_str(resp)),
    trans,
    "d$y <- y",
    sprintf("for (v in %s) d[[v]] <- factor(trimws(as.character(d[[v]])))", r_vec(c(main, sub, blk))),
    sprintf("d <- d[complete.cases(d[c(\"y\", %s)]), ]", paste(r_str(c(main, sub, blk)), collapse = ", ")),
    sprintf("d$.wp <- interaction(d[[%s]], d[[%s]], drop = TRUE)   # whole plot = %s x main-plot level", r_str(blk), r_str(main), if (crd) "replication" else "block"),
    "",
    sprintf("fit <- aov(%s, data = d)", fml),
    "st <- summary(fit)",
    "st                                   # Error(a) is the first stratum, Error(b) the Within stratum",
    "a <- st[[1]][[1]]; b <- st[[2]][[1]]",
    "dfa <- a[[\"Df\"]][nrow(a)]; msa <- a[[\"Mean Sq\"]][nrow(a)]   # Error(a)",
    "dfb <- b[[\"Df\"]][nrow(b)]; msb <- b[[\"Mean Sq\"]][nrow(b)]   # Error(b)",
    "cv_a <- 100 * sqrt(msa) / mean(d$y); cv_b <- 100 * sqrt(msb) / mean(d$y)",
    sprintf("nb <- nlevels(d[[%s]])", r_str(sub)),
    "pooled_ms <- (msa + (nb - 1) * msb) / nb",
    "pooled_df <- floor((msa + (nb - 1) * msb)^2 / (msa^2 / dfa + ((nb - 1) * msb)^2 / dfb))",
    "",
    "# mean comparisons: use them for effects that are significant (compare the main effects when the interaction is not)",
    "cmp <- list()",
    sprintf("cmp[[%s]] <- %s", r_str(main), script_compare(r$tests_used[[main]], "d$y", sprintf("d[[%s]]", r_str(main)), r$alpha, "dfa", "msa")),
    sprintf("cmp[[%s]] <- %s", r_str(sub), script_compare(r$tests_used[[sub]], "d$y", sprintf("d[[%s]]", r_str(sub)), r$alpha, "dfb", "msb")),
    "cmp")
  if (isTRUE(r$interaction_significant)) {
    body <- c(body, "", "# the interaction is significant, so each factor is compared within the levels of the other", "simple <- list()",
      sprintf("for (lv in levels(d[[%s]])) {   # sub-plot factor within each main-plot level (Error(b))", r_str(main)),
      sprintf("  s <- d[d[[%s]] == lv, ]", r_str(main)),
      sprintf("  simple[[paste(%s, \"within\", %s, \"=\", lv)]] <- %s", r_str(sub), r_str(main),
              script_compare(r$tests_used[[sub]], "s$y", sprintf("droplevels(s[[%s]])", r_str(sub)), r$alpha, "dfb", "msb")),
      "}",
      sprintf("for (lv in levels(d[[%s]])) {   # main-plot factor within each sub-plot level (pooled error)", r_str(sub)),
      sprintf("  s <- d[d[[%s]] == lv, ]", r_str(sub)),
      sprintf("  simple[[paste(%s, \"within\", %s, \"=\", lv)]] <- %s", r_str(main), r_str(sub),
              script_compare(r$tests_used[[main]], "s$y", sprintf("droplevels(s[[%s]])", r_str(main)), r$alpha, "pooled_df", "pooled_ms")),
      "}", "simple")
  }
  paste(c(head, "", body), collapse = "\n")
}

# Combined analysis of one treatment factor over environments (sites, seasons, years), environments fixed as in STAR.
# RCBD: y ~ env + env:block + trt + trt:env, with the environment tested against block within environment and the other
# terms against the pooled error. CRD: y ~ env + trt + trt:env. Each environment is also analysed on its own and the
# error variances are compared with Bartlett's test.
script_combined <- function(r, file = NULL) {
  trt <- r$treatment; env <- r$env; blk <- r$block; rcbd <- r$design == "CombRCBD"
  resp <- if (!is.null(r$transform)) r$original_response else r$response
  head <- script_header(sprintf("Combined %s of %s: %s over %s", if (rcbd) "RCBD" else "CRD", resp, trt, env), file)
  trans <- if (!is.null(r$transform)) sprintf("y <- switch(%s, log = log(y + 1), sqrt = sqrt(y + 0.5), arcsin = asin(sqrt(y / 100)) * 180 / pi)   # transformed response", r_str(r$transform))
  bt <- function(x) sprintf("`%s`", x)
  fml <- if (rcbd) sprintf("y ~ %s + %s:%s + %s + %s:%s", bt(env), bt(env), bt(blk), bt(trt), bt(trt), bt(env)) else sprintf("y ~ %s + %s + %s:%s", bt(env), bt(trt), bt(trt), bt(env))
  fml_env <- if (rcbd) sprintf("y ~ %s + %s", bt(blk), bt(trt)) else sprintf("y ~ %s", bt(trt))
  a <- r_num(r$alpha)
  body <- c(
    "# mean comparisons use the agricolae package: install.packages(\"agricolae\")",
    sprintf("y <- parse_number(d[[%s]])", r_str(resp)),
    trans,
    "d$y <- y",
    sprintf("for (v in %s) d[[v]] <- factor(trimws(as.character(d[[v]])))", r_vec(c(trt, env, blk))),
    sprintf("d <- d[complete.cases(d[c(\"y\", %s)]), ]   # rows with a missing value are left out", paste(r_str(c(trt, env, blk)), collapse = ", ")),
    "",
    sprintf("fit <- aov(%s, data = d)", fml),
    "tab <- summary(fit)[[1]]; rn <- gsub(\"`\", \"\", trimws(rownames(tab)), fixed = TRUE)",
    "i_err <- nrow(tab)                   # pooled error",
    sprintf("i_env <- match(%s, rn); i_trt <- match(%s, rn); i_int <- which(rn %%in%% c(%s, %s))", r_str(env), r_str(trt),
            r_str(paste(trt, env, sep = ":")), r_str(paste(env, trt, sep = ":"))),
    if (rcbd) sprintf("i_rep <- which(rn %%in%% c(%s, %s))   # block within environment", r_str(paste(env, blk, sep = ":")), r_str(paste(blk, env, sep = ":"))),
    "dfe <- tab$Df[i_err]; mse <- tab[[\"Mean Sq\"]][i_err]",
    if (rcbd) c(
      "df_rep <- tab$Df[i_rep]; ms_rep <- tab[[\"Mean Sq\"]][i_rep]",
      "# the environment is tested against block within environment",
      "f_env <- tab[[\"Mean Sq\"]][i_env] / ms_rep",
      "p_env <- pf(f_env, tab$Df[i_env], df_rep, lower.tail = FALSE)"),
    "tab                                  # the F tests in this table use the pooled error",
    if (rcbd) "c(F = f_env, p = p_env)           # environment, against block within environment",
    "cv <- 100 * sqrt(mse) / mean(d$y)",
    "",
    "# each environment on its own, and Bartlett's test of equal error variances",
    sprintf("by_env <- do.call(rbind, lapply(levels(d[[%s]]), function(lv) {", r_str(env)),
    sprintf("  s <- droplevels(d[d[[%s]] == lv, ])", r_str(env)),
    sprintf("  t1 <- summary(aov(%s, data = s))[[1]]; k <- match(%s, gsub(\"`\", \"\", trimws(rownames(t1)), fixed = TRUE))", fml_env, r_str(trt)),
    sprintf("  data.frame(Environment = lv, ErrorMS = t1[[\"Mean Sq\"]][nrow(t1)], ErrorDF = t1$Df[nrow(t1)], F = t1[[\"F value\"]][k], P = t1[[\"Pr(>F)\"]][k], stringsAsFactors = FALSE)"),
    "}))",
    "by_env",
    "k <- nrow(by_env); pooled <- sum(by_env$ErrorDF * by_env$ErrorMS) / sum(by_env$ErrorDF)",
    "chi <- (sum(by_env$ErrorDF) * log(pooled) - sum(by_env$ErrorDF * log(by_env$ErrorMS))) / (1 + (sum(1 / by_env$ErrorDF) - 1 / sum(by_env$ErrorDF)) / (3 * (k - 1)))",
    "bartlett_p <- pchisq(chi, k - 1, lower.tail = FALSE)   # a small p means the pooled error may not be valid",
    "c(chi_square = chi, df = k - 1, p = bartlett_p)",
    "",
    "# mean comparisons: use them for effects that are significant (the main effects when the interaction is not)",
    "cmp <- list()",
    sprintf("cmp[[%s]] <- %s", r_str(trt), script_compare(r$tests_used[[trt]], "d$y", sprintf("d[[%s]]", r_str(trt)), r$alpha, "dfe", "mse")),
    sprintf("cmp[[%s]] <- %s", r_str(env), script_compare(r$tests_used[[env]], "d$y", sprintf("d[[%s]]", r_str(env)), r$alpha, if (rcbd) "df_rep" else "dfe", if (rcbd) "ms_rep" else "mse")),
    "cmp")
  if (isTRUE(r$interaction_significant)) {
    sim <- unlist(lapply(c(trt, env), function(cmp_f) {
      at_f <- setdiff(c(trt, env), cmp_f)
      c(sprintf("for (lv in levels(d[[%s]])) {", r_str(at_f)),
        sprintf("  s <- d[d[[%s]] == lv, ]", r_str(at_f)),
        sprintf("  simple[[paste(%s, \"within\", %s, \"=\", lv)]] <- %s", r_str(cmp_f), r_str(at_f),
                script_compare(r$tests_used[[cmp_f]], "s$y", sprintf("droplevels(s[[%s]])", r_str(cmp_f)), r$alpha)),
        "}")
    }))
    body <- c(body, "", "# the interaction is significant: each factor is compared within the levels of the other (pooled error)", "simple <- list()", sim, "simple")
  }
  paste(c(head, "", body), collapse = "\n")
}

# trimmed means (20%): the functions the app uses, written out so the script needs no package
script_trim_funs <- c(
  "# 20% trimmed means (Wilcox 2017): Winsorized variance, Yuen's standard error, percentile bootstrap with a fixed seed",
  "tr <- 0.2",
  "winvar <- function(x) { n <- length(x); g <- floor(tr * n); y <- sort(x); if (g > 0) { y[1:g] <- y[g + 1]; y[(n - g + 1):n] <- y[n - g] }; var(y) }",
  "h_of <- function(x) length(x) - 2 * floor(tr * length(x))",
  "d_of <- function(x) (length(x) - 1) * winvar(x) / (h_of(x) * (h_of(x) - 1))",
  "boot_ci <- function(stat, B = 2000, conf = 0.95) { set.seed(1); v <- replicate(B, stat()); unname(quantile(v, c((1 - conf) / 2, 1 - (1 - conf) / 2))) }")

# heteroscedastic ANOVA on trimmed means (t1way) with Yuen's tests, as the app runs it
script_trimmed <- function(x, r, file = NULL) {
  resp <- r$original_response %||% r$response
  head <- script_header(sprintf("Trimmed-means ANOVA of %s by %s", resp, x$treatment), file)
  trans <- if (!is.null(r$transform)) sprintf("y <- switch(%s, log = log(y + 1), sqrt = sqrt(y + 0.5), arcsin = asin(sqrt(y / 100)) * 180 / pi)   # transformed response", r_str(r$transform))
  body <- c(
    script_num("y", resp), trans, "d$y <- y",
    sprintf("d$g <- factor(trimws(as.character(d[[%s]])))", r_str(x$treatment)),
    "d <- d[complete.cases(d[c(\"y\", \"g\")]), ]   # rows with a missing value are left out",
    "", script_trim_funs, "",
    "x <- split(d$y, d$g); J <- length(x)",
    "dd <- sapply(x, d_of); h <- sapply(x, h_of); tm <- sapply(x, mean, trim = tr)",
    "w <- 1 / dd; U <- sum(w); xt <- sum(w * tm) / U; S <- sum((1 - w / U)^2 / (h - 1))",
    "F <- sum(w * (tm - xt)^2) / (J - 1) / (1 + 2 * (J - 2) / (J^2 - 1) * S)",
    "df1 <- J - 1; df2 <- 1 / (3 / (J^2 - 1) * S)",
    "p <- pf(F, df1, df2, lower.tail = FALSE)",
    "c(F = F, df1 = df1, df2 = df2, p = p)   # heteroscedastic one-way ANOVA on trimmed means (t1way)",
    "",
    "# Yuen's test for each pair, Holm adjusted",
    "pr <- combn(levels(d$g), 2)",
    "yuen <- do.call(rbind, lapply(seq_len(ncol(pr)), function(j) {",
    "  a <- x[[pr[1, j]]]; b <- x[[pr[2, j]]]; da <- d_of(a); db <- d_of(b)",
    "  t <- (mean(a, trim = tr) - mean(b, trim = tr)) / sqrt(da + db); df <- (da + db)^2 / (da^2 / (h_of(a) - 1) + db^2 / (h_of(b) - 1))",
    "  data.frame(Comparison = paste(pr[1, j], \"-\", pr[2, j]), t = t, DF = df, P = 2 * pt(-abs(t), df))",
    "}))",
    "yuen$P.adj <- p.adjust(yuen$P, \"holm\")",
    "yuen",
    "",
    sprintf("ci <- t(sapply(x, function(v) boot_ci(function() mean(sample(v, replace = TRUE), trim = tr), conf = %s)))   # bootstrap interval of each trimmed mean", r_num(1 - x$alpha)),
    "data.frame(TrimmedMean = tm, Lower = ci[, 1], Upper = ci[, 2])")
  paste(c(head, "", body), collapse = "\n")
}

# Yuen's test, or the trimmed mean of the differences (paired) or of the values (one sample), as the app runs it
script_tt_trimmed <- function(r, s, file = NULL) {
  head <- script_header(paste(s$method, "of", switch(r$type, independent = paste(r$response, "by", r$group), paired = paste(r$x, "-", r$y), one = r$response)), file)
  conf <- r_num(r$conf_level); alt <- r_str(r$alternative)
  pfun <- c("p_of <- function(t, df) switch(alt, two.sided = 2 * pt(-abs(t), df), greater = pt(t, df, lower.tail = FALSE), less = pt(t, df))",
            sprintf("alt <- %s; conf <- %s", alt, conf))
  data <- switch(r$type,
    independent = c(script_num("y", r$response), sprintf("g <- trimws(as.character(d[[%s]]))", r_str(r$group)),
                    sprintf("a <- y[g == %s & !is.na(y)]; b <- y[g == %s & !is.na(y)]", r_str(r$labels[1]), r_str(r$labels[2]))),
    paired = c(script_num("x1", r$x), script_num("x2", r$y), "ok <- !is.na(x1) & !is.na(x2)", "v <- x1[ok] - x2[ok]   # the differences", "mu <- 0"),
    one = c(script_num("v", r$response), "v <- v[!is.na(v)]", sprintf("mu <- %s", r_num(r$mu))))
  test <- if (r$type == "independent") c(
    "da <- d_of(a); db <- d_of(b); est <- mean(a, trim = tr) - mean(b, trim = tr); se <- sqrt(da + db)",
    "df <- (da + db)^2 / (da^2 / (h_of(a) - 1) + db^2 / (h_of(b) - 1)); t <- est / se",
    "boot <- boot_ci(function() mean(sample(a, replace = TRUE), trim = tr) - mean(sample(b, replace = TRUE), trim = tr), conf = conf)")
  else c(
    "est <- mean(v, trim = tr) - mu; se <- sqrt(d_of(v)); df <- h_of(v) - 1; t <- est / se",
    "boot <- boot_ci(function() mean(sample(v, replace = TRUE), trim = tr) - mu, conf = conf)")
  body <- c(data, "", script_trim_funs, pfun, "", test,
            "q <- qt(if (alt == \"two.sided\") 1 - (1 - conf) / 2 else conf, df)",
            "ci <- switch(alt, two.sided = est + c(-1, 1) * q * se, greater = c(est - q * se, Inf), less = c(-Inf, est + q * se))",
            "res <- c(difference = est, t = t, df = df, p = p_of(t, df), lower = ci[1], upper = ci[2], boot_lower = boot[1], boot_upper = boot[2])",
            "res")
  paste(c(head, "", body), collapse = "\n")
}
