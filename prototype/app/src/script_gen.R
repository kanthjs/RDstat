# Plain R scripts that repeat an analysis outside the app. They use only base R (stats) and, where the app does,
# agricolae / nortest / lmtest, never the star_* functions, so they run on any computer with R.

r_str <- function(x) encodeString(as.character(x), quote = "\"")
r_vec <- function(x) if (length(x) == 1) r_str(x) else paste0("c(", paste(r_str(x), collapse = ", "), ")")
r_num <- function(x) format(x, digits = 15)

script_header <- function(title, file) {
  read <- if (is.null(file) || is.na(file)) "# d <- read.csv(\"your_file.csv\", check.names = FALSE, strip.white = TRUE, stringsAsFactors = FALSE)"
          else if (grepl("[.]xlsx?$", file, ignore.case = TRUE)) sprintf("d <- as.data.frame(readxl::read_excel(%s))", r_str(file))
          else sprintf("d <- read.csv(%s, check.names = FALSE, strip.white = TRUE, stringsAsFactors = FALSE)", r_str(file))
  c(sprintf("# %s", title),
    sprintf("# Made by %s %s on %s. Runs with base R; set the working directory to the folder that holds the data file.", app_name, version_label(), format(Sys.Date())),
    "# If the file has title lines above the column names, add skip = <number of lines> to the read function.",
    "",
    read)
}

# numeric column as the app reads it: text trimmed, then converted to a number
script_num <- function(var, col) sprintf("%s <- suppressWarnings(as.numeric(d[[%s]]))", var, r_str(col))

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
      "shapiro_a <- shapiro.test(a); shapiro_b <- shapiro.test(b)   # normality",
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
      "shapiro <- shapiro.test(dif)   # normality of the differences",
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
      "shapiro <- shapiro.test(v)   # normality",
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
    "d[vars] <- lapply(d[vars], function(x) suppressWarnings(as.numeric(x)))",
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
