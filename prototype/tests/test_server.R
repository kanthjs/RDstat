# Headless test of the app's server logic (native R). Run from anywhere: Rscript prototype/tests/test_server.R
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
setwd(file.path(here, "..", "app"))
suppressMessages(library(shiny))
# a package the app needs but this R lacks makes the analyses that use it stop with an empty "Error:" (req() on an error), so name it
missing <- Filter(function(p) !requireNamespace(p, quietly = TRUE), c("agricolae", "lmtest", "mctest", "ggplot2", "readxl", "writexl"))
if (length(missing)) stop("Install these packages first: ", paste(missing, collapse = ", "), call. = FALSE)
app <- source("app.R")$value
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE) else cat("ok  ", msg, "\n")

testServer(app, {
  n <- 0
  run <- function(...) { n <<- n + 1; session$setInputs(..., go_aov = n) }
  session$setInputs(src = "ex", alpha = "0.05", pw = "auto")
  run_script <- function(txt) { e <- new.env(); old <- setwd(tempdir()); on.exit(setwd(old)); invisible(capture.output(eval(parse(text = txt), e))); e }   # in another folder: the script must not need the app files
  # the R code offered on the ANOVA screen, run on its own, must give the same table, means, letters and simple effects
  aov_script_check <- function(label) {
    r <- ok(); e <- run_script(aov_script())
    pv <- if (!is.null(r$by_env)) NULL else if (inherits(e$fit, "aovlist")) unlist(lapply(summary(e$fit), function(s) s[[1]][["Pr(>F)"]])) else summary(e$fit)[[1]][["Pr(>F)"]]
    same_table <- if (is.null(r$by_env)) (isTRUE(all.equal(unname(pv[!is.na(pv)]), r$anova$P[!is.na(r$anova$P)])) && (if (!is.null(e$cv_a)) isTRUE(all.equal(c(e$cv_a, e$cv_b), c(r$cv_a, r$cv_b))) else isTRUE(all.equal(e$cv, r$cv)))) else {
      ai <- function(src) r$anova$P[r$anova$Source == src]; tr <- r$treatment
      p_env <- if (!is.null(e$p_env)) unname(e$p_env) else e$tab[["Pr(>F)"]][e$i_env]
      isTRUE(all.equal(e$tab[["Pr(>F)"]][c(e$i_trt, e$i_int)], c(ai(tr), ai(paste(tr, r$env, sep = ":"))))) && isTRUE(all.equal(p_env, ai(r$env))) && isTRUE(all.equal(e$cv, r$cv)) &&
        isTRUE(all.equal(e$by_env$ErrorMS, r$by_env$ErrorMS)) && isTRUE(all.equal(e$by_env$ErrorDF, r$by_env$ErrorDF)) && isTRUE(all.equal(e$by_env$P, r$by_env$P)) && isTRUE(all.equal(unname(e$bartlett_p), r$bartlett$p))
    }
    same_means <- all(vapply(r$factors, function(f) { m <- r$means[[f]]; g <- trimws(e$cmp[[f]]$groups[m$Treatment, "groups"])
      isTRUE(all.equal(unname(e$cmp[[f]]$means[m$Treatment, 1]), m$Mean)) && (r$interaction_significant || identical(unname(g), m$Group)) }, NA))
    same_simple <- all(vapply(names(r$simple), function(nm) { sm <- r$simple[[nm]]; cf <- attr(sm, "compare"); af <- attr(sm, "at")
      all(vapply(seq_len(nrow(sm)), function(i) { x <- e$simple[[paste(cf, "within", af, "=", sm[[af]][i])]]
        identical(trimws(x$groups[sm[[cf]][i], "groups"]), sm$Group[i]) }, NA)) }, NA))
    check(same_table && same_means && same_simple, paste("ANOVA script reproduces the app:", label))
  }

  # ---- RCBD, one factor (rice yield vs nitrogen) ----
  session$setInputs(design = "RCBD", example = "rcbd1", resp = "Yield", trt = "Nitrogen", blk = "Rep")
tests <- c(auto = "HSD", LSD = "LSD", HSD = "HSD", duncan = "duncan")
  for (pw in names(tests)) { run(pw = pw); check(identical(unname(ok()$tests_used[["Nitrogen"]]), tests[[pw]]), paste("RCBD 1 factor: mean comparison", pw, "is used")) }
  check(!exists("AOVTest") && !exists("estMissData") && !exists("cv") && exists("AOVTest", envir = star_legacy, inherits = FALSE), "STAR's own code is kept in star_legacy and masks nothing")
  rep_tests <- c(LSD = "Least Significant", HSD = "Tukey", duncan = "Duncan")
  for (pw in names(rep_tests)) { run(pw = pw); check(any(grepl(rep_tests[[pw]], output$report)) && grepl("ANOVA TABLE", output$report), paste("Classic STAR report: RCBD 1 factor with", pw)) }
  run(pw = "LSD")
  check(round(ok()$anova$F[2], 2) == 8.54, "RCBD 1 factor: F = 8.54")
  lv <- output$means_plot
  check(file.exists(lv$src) || nzchar(lv$src), "Live View: means plot is drawn")
  check(grepl("^Plot: .*Nitrogen", lv$alt), paste("Live View: alt text names the plot:", lv$alt))
  check(identical(tables()$anova$`Pr(>F)`[2], "0.0022"), "RCBD 1 factor: p formatted 0.0022")
  check(identical(tables()$means$Nitrogen$Group, c("b", "a", "a", "a", "a", "a")), "RCBD 1 factor: letters")
  check(identical(tables()$means$Nitrogen$N, rep("3", 6)), "RCBD 1 factor: N as integers")
  aov_script_check("RCBD, one factor, LSD")
  html <- function(x) paste(as.character(x$html), collapse = "")
  ui <- html(output$results_ui); check(!grepl("key-stats", ui) && grepl("9.4", ui, fixed = TRUE) && regexpr("APA-style report", ui) < regexpr("Analysis of variance", ui) && grepl("result-card", ui), "RCBD 1 factor: the APA-style report comes first, then the result cards")
  check(grepl("No results yet", html(output$corr_msg)), "Correlation: empty state before the first run")

  # ---- CRD, one factor (insecticides) ----
  session$setInputs(design = "CRD", example = "crd1", resp = "GrainYield", trt = "Insecticide")
  run(pw = "HSD")
  check(is.null(ok()$block) && round(ok()$anova$F[1], 2) == 9.83, "CRD 1 factor: no block, F = 9.83")
  check(grepl("Completely Randomized Design", output$report) && grepl("Tukey", output$report), "CRD 1 factor: Classic STAR report")
  check(identical(tables()$means$Insecticide$Group, c("ab", "c", "a", "bc", "bc", "ab", "a")), "CRD 1 factor: letters match the original")
  aov_script_check("CRD, one factor, Tukey")
  session$setInputs(transform = "log"); run(pw = "duncan"); aov_script_check("CRD, log-transformed response, Duncan")
  session$setInputs(transform = "none"); run(pw = "HSD")
  check(!grepl("assumptions are not met", html(output$results_ui)) && !grepl("ข้อตกลงเบื้องต้น", html(output$results_ui)), "Results tab: no verdict on the assumptions")
  check(round(np_res()$statistic, 2) == 20.85 && np_res()$name == "Kruskal-Wallis" && !grepl("chi-square =", html(output$diag_ui)) && !grepl("chi-square =", html(output$results_ui)), "CRD 1 factor: Kruskal-Wallis computed but not shown beside the ANOVA, chi-square = 20.85 (kruskal.test)")
  # rank-based analysis chosen by the user: it replaces the ANOVA on the results screen and in the downloads
  run(aov_method = "rank")
  ui <- html(output$results_ui)
  check(aov_rank() && grepl("Kruskal-Wallis test", ui) && !grepl("Analysis of variance", ui) && !grepl("Means (publication layout)", ui, fixed = TRUE) && grepl("χ²(6, N = 28) = 20.85", ui, fixed = TRUE),
        "Rank method: Kruskal-Wallis replaces the ANOVA on the results screen")
  check(!grepl("Rank-based alternative", html(output$diag_ui)), "Rank method: no rank card on the assumptions tab")
  e <- run_script(np_script()); check(isTRUE(all.equal(e$np$statistics$p.chisq, np_try()$p)) && identical(trimws(e$np$groups[np_try()$means$Treatment, "groups"]), np_try()$means$Group),
                                      "Rank method: Kruskal-Wallis script reproduces the app (p and letters)")
  session$setInputs(aov_fmt = "png")
  check(identical(names(xlsx_sheets()), c("Rank test", "Info")) && inherits(gg_current(), "ggplot") && file.size(output$dl_aov_plot) > 3000, "Rank method: Excel sheets and box plot")
  doc <- readLines(output$dl_doc, warn = FALSE); html_part <- rawToChar(jsonlite::base64_dec(paste(doc[(grep("Content-Location: file:///C:/rdstat/report.htm", doc) + 2):(grep("^------=_NextPart_RDstat", doc)[2] - 2)], collapse = "")))
  Encoding(html_part) <- "UTF-8"
  check(grepl("Kruskal-Wallis test", html_part) && !grepl("<h2>Analysis of variance</h2>", html_part, fixed = TRUE), "Rank method: Word report gives the rank test, not the ANOVA")
  session$setInputs(transform = "log"); run(aov_method = "rank")
  crd <- read.csv("example_crd.csv", stringsAsFactors = FALSE)
  check(isTRUE(all.equal(np_try()$means$Median, as.numeric(tapply(crd$GrainYield, factor(crd$Insecticide, levels = np_try()$means$Treatment), median)))) &&
        grepl("transformation is not used", html(output$results_ui)), "Rank method: medians in the original unit when a transformation is set")
  session$setInputs(transform = "none"); run(aov_method = "anova")
  check(!aov_rank() && grepl("Analysis of variance", html(output$results_ui)), "Rank method: back to ANOVA")
  check(!is.null(aov_rec()) && grepl("Data check|Observation", html(output$results_ui)) && !grepl("Check with", html(output$results_ui)),
        "Guidance: data check under the ANOVA, with no result of another method beside it")
  # traffic light: one outlier in the insecticide trial is red, with a choice by reporting goal (ranks, or trimmed means)
  rc <- aov_rec(); ui <- html(output$results_ui)
  check(rc$light == "red" && identical(vapply(rc$options, function(o) o$id, ""), c("rank", "trimmed")) && grepl("light-red", ui) && grepl("To compare ranks or medians", ui) &&
        grepl("To compare means without relying on the normal curve", ui) && grepl("Use trimmed means", ui), "Traffic light: an outlier is red, with options by reporting goal")
  check(grepl("badge bg-danger", html(output$diag_ui)) && grepl("badge bg-success", html(output$diag_ui)), "Traffic light: assumption badges on the diagnostics tab")
  # the standard ANOVA is the default; Welch's ANOVA is suggested only when the variances differ or the group sizes are unequal
  xs <- c(-1.2, -0.6, -0.3, 0, 0.3, 0.6, 1.2, -0.9, 0.9, 0)
  eq <- data.frame(T = rep(c("a", "b", "c"), each = 10), y = c(10 + xs, 12 + xs, 14 + xs), stringsAsFactors = FALSE)
  rc <- star_recommend_aov(star_aov(eq, "y", "T", design = "CRD"))
  check(is.null(rc$switch_to) && rc$light %in% c("green", "yellow") && grepl("look similar", rc$observed) && grepl("standard ANOVA suits", rc$advice),
        "Standard ANOVA default: no switch offered when the variances look similar")
  rc <- star_recommend_aov(star_aov(eq[-(1:4), ], "y", "T", design = "CRD"))
  check(is.null(rc$switch_to), "Standard ANOVA default: unequal group sizes alone (similar variances) do not trigger a switch")
  # Welch's ANOVA with Games-Howell letters; switching method needs no new run
  session$setInputs(aov_method = "welch")
  w <- welch_try(); ref <- oneway.test(GrainYield ~ Insecticide, crd)
  check(aov_welch() && isTRUE(all.equal(w$p, ref$p.value)) && isTRUE(all.equal(w$statistic, unname(ref$statistic))) && grepl("Games-Howell comparisons", html(output$results_ui)),
        "Welch: oneway.test result on the screen")
  e <- run_script(welch_script()); check(isTRUE(all.equal(e$welch$p.value, w$p)) && isTRUE(all.equal(e$gh$P, w$pairs$P)), "Welch: script reproduces the app (Welch p and Games-Howell p)")
  check(identical(plot_obj()$tests_used[["Insecticide"]], "Games-Howell") && identical(names(xlsx_sheets()), c("Welch means", "Games-Howell", "Info")), "Welch: plot letters and Excel sheets")
  # Dunn's comparisons after Kruskal-Wallis, chosen instead of Conover's
  session$setInputs(aov_method = "rank", np_posthoc = "dunn_holm")
  np <- np_try(); e <- run_script(np_script())
  check(np$posthoc == "dunn_holm" && isTRUE(all.equal(unname(e$dunn$P.adj), np$pairs$P.adj)) && identical(np$means$Group[np$means$Treatment == "Control"], "b") && grepl("Dunn, Holm adjusted", html(output$results_ui)),
        "Dunn: Holm-adjusted comparisons, letters and script")
  session$setInputs(aov_method = "anova", np_posthoc = "conover")
  # trimmed means: with no trimming the methods are Welch's; the R code reproduces the app, bootstrap included
  check(isTRUE(all.equal(unname(unlist(.star_t1way(crd$GrainYield, factor(crd$Insecticide), 0)[c("F", "df2", "p")])), unname(c(ref$statistic, ref$parameter[2], ref$p.value)))) &&
        isTRUE(all.equal(.star_yuen(1:10 + 0, c(2, 5, 9, 11, 3, 8, 14, 6), 0)$p, t.test(1:10 + 0, c(2, 5, 9, 11, 3, 8, 14, 6))$p.value)),
        "Trimmed means: with no trimming, t1way and Yuen are Welch's ANOVA and Welch's t-test")
  session$setInputs(aov_method = "trimmed")
  x <- trimmed_try(); ui <- html(output$results_ui)
  check(aov_trimmed() && grepl("Yuen's comparisons", ui) && grepl("Bootstrap", ui) && grepl("you are using|You are using", ui), "Trimmed means: results screen, acknowledged as a recommended method")
  e <- run_script(trimmed_script())
  check(isTRUE(all.equal(unname(e$p), x$p)) && isTRUE(all.equal(unname(e$F), x$statistic)) && isTRUE(all.equal(e$yuen$P.adj, x$pairs$P.adj)) &&
        isTRUE(all.equal(unname(e$ci[, 1]), x$means$Lower)), "Trimmed means: script reproduces the app (F, p, Yuen tests, bootstrap intervals)")
  check(identical(names(xlsx_sheets()), c("Trimmed means", "Yuen tests", "Info")) && inherits(gg_current(), "ggplot"), "Trimmed means: Excel sheets and box plot")
  session$setInputs(aov_method = "anova")
  # the advice follows the data: skewed values suggest the rank test, unequal variances Welch (CRD) or a transformation (RCBD)
  set.seed(1)
  skew <- data.frame(T = rep(c("a", "b", "c"), each = 10), y = round(c(rexp(10, 1), rexp(10, 0.5), rexp(10, 0.3)), 2), stringsAsFactors = FALSE)
  rc <- star_recommend_aov(star_aov(skew, "y", "T", design = "CRD"))
  check(rc$light == "red" && identical(rc$switch_to, "rank") && grepl("Kruskal-Wallis", rc$switch_label), "Guidance: skewed residuals are red and suggest Kruskal-Wallis or trimmed means")
  x <- c(-1.2, -0.6, -0.3, 0, 0.3, 0.6, 1.2, -0.9, 0.9, 0)
  het <- data.frame(T = rep(c("a", "b", "c"), each = 10), y = c(10 + x, 12 + 4 * x, 14 + 10 * x), B = rep(1:10, 3), stringsAsFactors = FALSE)
  rc <- star_recommend_aov(star_aov(het, "y", "T", design = "CRD"))
  check(identical(rc$switch_to, "welch") && grepl("Levene", rc$observed) && grepl("Switch to Welch", rc$switch_label), "Guidance: unequal variances in CRD suggest Welch's ANOVA")
  rc <- star_recommend_aov(star_aov(het[-(1:4), ], "y", "T", design = "CRD"))
  check(identical(rc$switch_to, "welch") && grepl("numbers of observations differ", rc$observed), "Guidance: unequal variances and unequal group sizes also suggest Welch's ANOVA")

  # ---- incomplete or mistyped layouts: the message names the cells and the likely cause ----
  err_msg <- function(expr) tryCatch({ expr; "" }, error = conditionMessage)
  lay <- expand.grid(Rep = factor(1:3), N = factor(c("N0", "N1", "N2", "N3"))); lay$Y <- c(48, 52, 50, 55, 57, 54, 60, 62, 59, 58, 61, 63)
  typo <- lay; typo$N <- as.character(typo$N); typo$N[typo$N == "N2"][1:2] <- "n2"
  m <- err_msg(star_aov(typo, "Y", "N", block = "Rep", design = "RCBD"))
  check(grepl("have no row", m) && grepl("(Rep = 3, N = n2)", m, fixed = TRUE) && grepl("'n2' and 'N2' differ only in upper/lower case", m, fixed = TRUE),
        "Layout check: a treatment label typed in lower case is named, with the cells it leaves empty")
  m <- err_msg(star_aov(data.frame(Rep = factor(1:6), N = factor(rep(c("a", "b"), 3)), Y = c(5, 7, 6, 8, 5, 9)), "Y", "N", block = "Rep", design = "RCBD"))
  check(grepl("check that the right columns are chosen", m), "Layout check: a block column with one treatment per block points to the column choice")
  m <- err_msg(star_aov(rbind(lay, lay[2, ]), "Y", "N", block = "Rep", design = "RCBD"))
  check(grepl("more than one row: (Rep = 2, N = N0)", m, fixed = TRUE), "Layout check: a duplicated plot is named")
  flat <- lay; flat$Y <- rep(c(1, 2, 3, 4), each = 3)
  check(grepl("error variance is zero", err_msg(star_aov(flat, "Y", "N", block = "Rep", design = "RCBD"))), "Layout check: treatment means repeated on every row are refused")
  r <- star_aov(data.frame(T = factor(rep(c("A", "B", "C"), c(4, 4, 1))), Y = c(10, 11, 9, 12, 14, 15, 13, 16, 11)), "Y", "T", design = "CRD")
  check(any(grepl("single observation (C)", r$data_notes, fixed = TRUE)), "Layout check: a CRD treatment with one observation is noted")
  sp <- expand.grid(Rep = factor(1:3), M = factor(c("I1", "I2")), S = factor(c("V1", "V2", "V3"))); sp$Y <- seq(4, 7.4, by = 0.2)
  check(grepl("(Rep = 1, M = I2, S = V1)", err_msg(star_splitplot(sp[-4, ], "Y", "M", "S", block = "Rep", design = "SplitRCBD")), fixed = TRUE), "Layout check: split plot names the missing sub plot")
  cmb <- expand.grid(Rep = factor(1:3), V = factor(c("A", "B", "C")), Site = factor(c("S1", "S2"))); cmb$Y <- seq(3, 6.4, by = 0.2)
  m <- err_msg(star_combined(cmb[!(cmb$Site == "S2" & cmb$V == "C"), ], "Y", "V", env = "Site", block = "Rep", design = "RCBD"))
  check(grepl("(Site = S2, V = C)", m, fixed = TRUE) && grepl("In V, 'C' appear", m, fixed = TRUE), "Layout check: combined analysis names the treatment missing at a site")
  rc <- star_recommend_aov(star_aov(het, "y", "T", "B", design = "RCBD"))
  check(identical(rc$switch_to, "rank") && grepl("no Welch version for blocks", rc$advice), "Guidance: unequal variances in RCBD suggest a transformation or Friedman")
  ui <- as.character(advice_ui(rc, "anova", "rec", FALSE, "ANOVA"))
  check(grepl("Use Friedman", ui) && grepl("rec_rank", ui) && grepl("Continue with ANOVA", ui) && grepl("Advice only", ui) && !grepl("trimmed", ui), "Guidance: switch and keep buttons, marked as advice (no trimmed option for RCBD)")
  check(grepl("recommended method", as.character(advice_ui(rc, "rank", "a", FALSE, "rank"))) && grepl("chose to continue", as.character(advice_ui(rc, "anova", "a", TRUE, "ANOVA"))),
        "Guidance: acknowledges the recommended method, or the user's choice to keep theirs")

  # ---- CRD, two factors (tooth growth) ----
  session$setInputs(design = "CRD", example = "crd2", resp = "len", trt = c("supp", "dose"))
  run(pw = "auto")
  r <- ok()
  check(identical(r$anova$Source, c("supp", "dose", "supp:dose", "Error", "Total")), "CRD 2 factors: ANOVA rows")
  run(aov_method = "rank"); check(grepl("needs exactly one treatment factor", html(output$results_ui)), "Rank method: two factors give a reason, not a blank screen")
  run(aov_method = "welch"); check(aov_mode() == "anova" && grepl("Welch's ANOVA needs exactly one treatment factor", html(output$results_ui)) && grepl("Analysis of variance", html(output$results_ui)),
                                   "Welch chosen: two factors get the standard ANOVA, with the reason")
  # analyses that do not exist for the design are greyed out (disabled), the standard ANOVA stays available
  check(identical(aov_support()$ok, "anova") && grepl("exactly one treatment factor", aov_support()$why), "Analysis buttons: two factors leave only the standard ANOVA")
  session$setInputs(trt = "supp")
  check(setequal(aov_support()$ok, c("anova", "rank", "welch", "trimmed")) && is.null(aov_support()$why), "Analysis buttons: CRD with one factor has all four analyses")
  session$setInputs(design = "RCBD"); check(setequal(aov_support()$ok, c("anova", "rank")) && grepl("blocks", aov_support()$why), "Analysis buttons: RCBD has no Welch or trimmed means")
  session$setInputs(design = "SplitCRD"); check(identical(aov_support()$ok, "anova") && grepl("split-plot", aov_support()$why), "Analysis buttons: split plots have only the standard ANOVA")
  session$setInputs(design = "CRD", trt = c("supp", "dose"))
  run(aov_method = "anova")
  check(r$interaction_significant, "CRD 2 factors: interaction significant")
  check(setequal(names(tables()$simple), c("supp within dose", "dose within supp")), "CRD 2 factors: simple-effect tables")
  check(identical(names(tables()$cells), c("supp", "dose = 0.5", "dose = 1", "dose = 2")), "CRD 2 factors: wide cell means")
  check(identical(tables()$simple[["supp within dose"]][["dose = 0.5"]], c("13.23 a", "7.98 b")), "CRD 2 factors: simple effect letters (original STAR)")
  aov_script_check("CRD, two factors, significant interaction (simple effects)")
  check(!("Group" %in% names(tables()$means$supp)), "CRD 2 factors: no main-effect letters when the interaction is significant")

  # ---- RCBD, two factors (nitrogen x variety) ----
  session$setInputs(design = "RCBD", example = "rcbd2", resp = "Yield", trt = c("Nitrogen", "Variety"), blk = "Rep")
  run(pw = "auto")
  r <- ok()
  check(identical(r$anova$Source, c("Rep", "Nitrogen", "Variety", "Nitrogen:Variety", "Error", "Total")), "RCBD 2 factors: ANOVA rows")
  check(round(r$cv, 2) == 10.07, "RCBD 2 factors: CV = 10.07")
  aov_script_check("RCBD, two factors")
  check(identical(tables()$simple[["Nitrogen within Variety"]][["Variety = IR8"]][1:2], c("4,252.67 d", "5,672.00 c")), "RCBD 2 factors: Nitrogen within IR8 letters")
  check("Simple effects" %in% names(xlsx_sheets()), "RCBD 2 factors: simple effects in Excel")

  # ---- split plot in RCBD (nitrogen main plot x variety sub plot) ----
  session$setInputs(design = "SplitRCBD", example = "split1", resp = "Yield", trt_main = "Nitrogen", trt_sub = "Variety", blk = "Rep")
  run(pw = "auto")
  r <- ok()
  check(identical(r$anova$Source, c("Rep", "Nitrogen", "Error(a)", "Variety", "Nitrogen:Variety", "Error(b)", "Total")), "Split plot: ANOVA rows")
  aov_script_check("split plot (RCBD), significant interaction")
  check(round(c(r$cv_a, r$cv_b), 2)[2] == 10.79 && round(r$cv_a, 2) == 6.88, "Split plot: CV(a) = 6.88, CV(b) = 10.79")
  check(identical(tables()$simple[["Nitrogen within Variety"]][["Variety = IR8"]][1:2], c("4,252.67 d", "5,672.00 c")), "Split plot: Nitrogen within IR8 letters (original STAR)")
  check(!is.null(ok()$cv_a) && !is.null(ok()$cv_b) && grepl("CV", paste(unlist(tables()), collapse = " ")), "Split plot: CV(a) and CV(b) kept in the result and in the tables")
  check(file.size(output$dl_xlsx) > 3000, "Split plot: Excel")
  check(grepl("Split Plot in Randomized Complete Block", output$report), "Split plot: Classic STAR report")
  run(pw = "HSD"); run(pw = "auto")
  check(identical(ok(), r), "Split plot: same settings give the same (cached) result")
  session$setInputs(plot_effect = "interaction"); p0 <- output$dl_aov_plot; check(file.size(p0) > 5000, "Split plot: interaction png")

  # ---- split plot in CRD (irrigation main plot x variety sub plot, simulated example) ----
  session$setInputs(design = "SplitCRD", example = "splitcrd1", resp = "Yield", trt_main = "Irrigation", trt_sub = "Variety", blk = "Rep")
  run(pw = "LSD")
  r <- ok()
  check(identical(r$anova$Source, c("Irrigation", "Error(a)", "Variety", "Irrigation:Variety", "Error(b)", "Total")), "Split plot CRD: ANOVA rows (no block)")
  check(identical(r$anova$DF, c(2, 9, 3, 6, 27, 47)), "Split plot CRD: DF a(r-1) = 9 and a(r-1)(b-1) = 27")
  check(round(r$anova$F[1], 2) == 29.80 && round(r$cv_a, 2) == 7.56 && round(r$cv_b, 2) == 5.15, "Split plot CRD: F = 29.80, CV(a) = 7.56, CV(b) = 5.15 (original STAR)")
  check(identical(tables()$means$Irrigation$Group, c("b", "a", "c")) && identical(tables()$means$Variety$Group, c("d", "c", "b", "a")), "Split plot CRD: letters match the original STAR")
  check(grepl("Split Plot in Completely Randomized Design", output$report), "Split plot CRD: Classic STAR report")
  aov_script_check("split plot (CRD)")
  dh <- html(output$diag_ui); mh <- html(output$diag_main_ui); mp <- r$diagnostics$main_plot
  check(grepl("Sub plots", dh) && grepl("Error(b)", dh, fixed = TRUE) && nrow(r$diagnostics$residuals) == 48,
        "Split plot CRD: assumption checks of the sub plots (48 residuals)")
  check(!is.null(mp) && mp$df == 9 && nrow(mp$residuals) == 12 && grepl("Main plots", mh) && grepl("little power", mh),
        "Split plot CRD: main plots checked on Error(a) (12 main plots, 9 df), with the low-power note")
  check(!is.null(output$diag_plot_main$src), "Split plot CRD: main-plot residual plot")
  check(identical(tables()$agronomy$Irrigation[[1]][4:9], c("F test (Irrigation)", "Grand mean", "CV (%)", "SE(m)", "SED", "LSD (0.05)")),
        "Split plot CRD: publication footer rows")
  pr <- precision_of(r, "Irrigation"); ea <- r$anova[r$anova$Source == "Error(a)", ]
  check(isTRUE(all.equal(pr[["sed"]], sqrt(2 * ea$MS / 16))) && pr[["df"]] == 9, "Split plot CRD: SED of the main plot uses Error(a)")

  # ---- repeated measures over time (split plot in time with the sphericity check) ----
  session$setInputs(design = "RepRCBD", example = "rep_rcbd1", resp = "Height", trt_main = "Fertilizer", trt_sub = "Day", blk = "Block")
  run(pw = "auto")
  r <- ok(); s <- r$sphericity
  check(identical(r$anova$Source, c("Block", "Fertilizer", "Error(a)", "Day", "Fertilizer:Day", "Error(b)", "Total")) && identical(r$anova$DF, c(3, 3, 9, 3, 9, 36, 63)),
        "Repeated RCBD: split plot in time rows and DF")
  rd <- read.csv("example_rep_rcbd.csv"); wd <- reshape(rd, idvar = c("Block", "Fertilizer"), timevar = "Day", direction = "wide")
  ref <- anova(lm(as.matrix(wd[grep("^Height", names(wd))]) ~ factor(wd$Fertilizer)), X = ~1, test = "Spherical")
  mref <- mauchly.test(lm(as.matrix(wd[grep("^Height", names(wd))]) ~ factor(wd$Fertilizer)), X = ~1)
  check(isTRUE(all.equal(s$tests$F, ref$F[1:2])) && isTRUE(all.equal(s$tests$P_GG, ref$`G-G Pr`[1:2])) && isTRUE(all.equal(s$tests$P_HF, ref$`H-F Pr`[1:2])) &&
        isTRUE(all.equal(s$mauchly$p, mref$p.value)) && isTRUE(all.equal(s$tests$F, r$anova$F[r$anova$Source %in% c("Day", "Fertilizer:Day")])),
        "Repeated RCBD: F, Greenhouse-Geisser and Huynh-Feldt p values and Mauchly's test equal anova.mlm() / mauchly.test()")
  check(identical(r$means$Day$Treatment, c("D30", "D60", "D90", "D120")), "Repeated RCBD: times in time order, not alphabetical")
  check(s$correction == "none" && isTRUE(all.equal(r$effects$P, r$anova$P[match(r$effects$Effect, r$anova$Source)])), "Repeated RCBD: sphericity holds (Mauchly p = 0.09), so the usual p values are used")
  res_html <- html(output$results_ui)
  check(grepl("Sphericity and corrected tests", res_html) && grepl("Mauchly", res_html) && grepl("วัดซ้ำ|sphericity", thai_summary_aov(r)), "Repeated RCBD: sphericity card and Thai sentence")
  aov_script_check("repeated measures (RCBD)")
  e <- run_script(aov_script())
  check(isTRUE(all.equal(unname(anova(e$mfit, X = ~1, test = "Spherical")$`G-G Pr`[1:2]), s$tests$P_GG)), "Repeated RCBD: script gives the Greenhouse-Geisser p values")
  check("Sphericity" %in% names(xlsx_sheets()) && file.size(output$dl_doc) > 3000, "Repeated RCBD: Excel sheet and Word report")
  check(grepl("STAR has no repeated-measures analysis", paste(output$report, collapse = " ")),"Repeated RCBD: Classic STAR report says it is the split plot in time")
  session$setInputs(plot_effect = "interaction_line", plot_style = "gg"); check(file.size(output$dl_aov_plot) > 5000, "Repeated RCBD: profile plot (lines over time)")
  session$setInputs(plot_style = "classic")

  session$setInputs(design = "RepCRD", example = "rep_crd1", resp = "Height", trt_main = "Fertilizer", trt_sub = "Day", blk = "Pot")
  run(pw = "auto")
  r <- ok(); s <- r$sphericity
  check(identical(r$anova$DF, c(3, 16, 3, 9, 48, 79)), "Repeated CRD: DF (no block), Error(a) = a(r-1) = 16")
  check(s$correction == "GG" && s$mauchly$p < 0.05 && s$gg < 0.75 && isTRUE(all.equal(r$effects$P[2:3], s$tests$P_GG)),
        "Repeated CRD: sphericity rejected, Greenhouse-Geisser p values decide significance")
  check(grepl("Greenhouse-Geisser corrected", r$summary) && any(grepl("epsilon", r$apa)) && grepl("F(1.63, 26.08) = ", r$apa[2], fixed = TRUE),
        "Repeated CRD: summary and APA give the corrected test and degrees of freedom")
  aov_script_check("repeated measures (CRD)")
  ck <- paste(as.character(rep_layout_check(read.csv("example_rep_crd.csv"), "RepCRD", "Fertilizer", "Day", "Pot")), collapse = "")
  check(grepl("20 plots, each measured at 4 times", ck) && grepl("Error(a) has 16 df", ck, fixed = TRUE), "Repeated check: plots and Error(a) df")
  bad <- read.csv("example_rep_crd.csv"); bad$Row <- seq_len(nrow(bad))
  check(grepl("does not say which plot", paste(as.character(rep_layout_check(bad, "RepCRD", "Fertilizer", "Day", "Row")), collapse = "")), "Repeated check: a row number chosen as plot is caught")
  check(grepl("not complete", tryCatch({ star_repeated(read.csv("example_rep_crd.csv")[-1, ], "Height", "Fertilizer", "Day", "Pot", design = "RepCRD"); "" }, error = conditionMessage)),
        "Repeated CRD: a plot missing one time is refused with a message")

  # ---- publication layout and transformation ----
  session$setInputs(design = "RCBD", example = "rcbd1", resp = "Yield", trt = "Nitrogen", blk = "Rep", transform = "none"); run(pw = "LSD")
  lsd_ag <- agricolae::LSD.test(ok()$data$Yield, ok()$data$Nitrogen, ok()$anova$DF[ok()$anova$Source == "Error"], ok()$anova$MS[ok()$anova$Source == "Error"], console = FALSE)$statistics$LSD
  check(identical(tables()$agronomy$Nitrogen$Mean[nrow(tables()$agronomy$Nitrogen)], fmt_n(lsd_ag)), "Publication table: LSD equals agricolae's LSD")
  check(grepl("8.54**", tables()$agronomy$Nitrogen$Mean[7], fixed = TRUE), "Publication table: F with ** for p < 0.01")
  run(transform = "log")
  check(identical(ok()$response, "Yield_log") && identical(ok()$transform, "log"), "Transformation: analysis of log(y + 1)")
  check(isTRUE(all.equal(exp(ok()$means$Nitrogen$Mean) - 1, star_parse_number(tables()$means$Nitrogen$`Mean (original unit)`), tolerance = 1e-3)), "Transformation: back-transformed means")
  session$setInputs(plot_style = "gg", gg_points = TRUE, gg_error = "se"); check(inherits(gg_means(), "ggplot"), "Transformation: plot with observations"); session$setInputs(plot_style = "classic")
  run(transform = "arcsin"); check(inherits(res(), "star_error") && grepl("between 0 and 100", res()$message), "Transformation: arcsine refuses values above 100")
  session$setInputs(transform = "none")

  # ---- combined analysis over environments (Gomez & Gomez two seasons; values from STAR's combAOVTest) ----
  session$setInputs(design = "CombRCBD", example = "comb_rcbd1", resp = "GrainYield", trt = "NitrogenRate", env = "Season", blk = "Rep")
  run(pw = "LSD")
  r <- ok()
  check(identical(r$anova$Source, c("Season", "Rep within Season", "NitrogenRate", "NitrogenRate:Season", "Error", "Total")), "Combined RCBD: ANOVA rows")
  aov_script_check("combined RCBD over seasons")
  check(identical(round(r$anova$F[1:4], 2), c(14.25, 0.71, 10.62, 5.47)) && round(r$cv, 2) == 12.05, "Combined RCBD: F and CV match STAR")
  check(grepl("Combined Analysis of Variance", output$report), "Combined RCBD: Classic STAR report")
  ui <- html(output$results_ui)
  check(grepl("Analysis of each Season", ui) && grepl("Bartlett", ui), "Combined RCBD: each environment and Bartlett's test")
  check("Each environment" %in% readxl::excel_sheets(output$dl_xlsx), "Combined RCBD: Excel sheet for each environment")
  check(!is.null(r$diagnostics), "Combined RCBD: assumption checks")
  session$setInputs(design = "CombCRD", example = "comb_crd1", resp = "Tillers", trt = "Variety", env = "Site")
  run(pw = "LSD")
  check(identical(ok()$anova$DF, c(2, 3, 6, 36, 47)), "Combined CRD: ANOVA df")
  aov_script_check("combined CRD over sites")

  # ---- Thai summary and Word report ----
  session$setInputs(design = "SplitRCBD", example = "split1", resp = "Yield", trt_main = "Nitrogen", trt_sub = "Variety", blk = "Rep"); run(pw = "auto")
  check(grepl("ปฏิสัมพันธ์ระหว่าง Nitrogen และ Variety มีนัยสำคัญ", html(output$results_ui)), "Thai summary: significant interaction")
  session$setInputs(design = "RCBD", example = "rcbd2", resp = "Yield", trt = c("Nitrogen", "Variety"), blk = "Rep", transform = "sqrt"); run(pw = "auto")
  session$setInputs(transform = "none"); run(pw = "auto")
  doc <- output$dl_doc; txt <- readLines(doc, warn = FALSE)
  check(any(grepl("multipart/related", txt)) && any(grepl("image/png", txt)) && file.size(doc) > 20000, "Word report: single-file page with tables and the plot")
  html_part <- rawToChar(jsonlite::base64_dec(paste(txt[(which(grepl("report.htm", txt))[1] + 2):(which(grepl("^------=_NextPart_RDstat$", txt))[2] - 2)], collapse = ""))); Encoding(html_part) <- "UTF-8"
  check(grepl("Analysis of variance", html_part) && grepl("สรุปผล", html_part) && grepl("LSD (0.05)", html_part, fixed = TRUE), "Word report: ANOVA, Thai summary and the publication table")

  # ---- downloads and copy (two-factor result is selected) ----
  session$setInputs(design = "RCBD", example = "rcbd2", resp = "Yield", trt = c("Nitrogen", "Variety"), blk = "Rep"); run(pw = "auto")
  x <- output$dl_xlsx; check(file.exists(x) && file.size(x) > 3000, "xlsx download created")
  sh <- readxl::excel_sheets(x); check(identical(sh, c("ANOVA", "Publication table", "Means", "Cell means", "Simple effects", "Info")), "xlsx sheets for a two-factor result")
  session$setInputs(plot_effect = "interaction"); p1 <- output$dl_aov_plot; check(file.exists(p1) && file.size(p1) > 5000, "png (interaction plot)")
  session$setInputs(plot_effect = "Nitrogen"); p2 <- output$dl_aov_plot; check(file.exists(p2) && file.size(p2) > 5000, "png (main effect plot)")
  t <- output$dl_txt; check(length(readLines(t)) > 40, "Classic STAR report: TXT download")
  session$setInputs(cp_anova = 1, cp_means = 1, cp_report = 1)
  cat("ok   copy buttons ran\n")

  session$setInputs(design = "CRD", example = "crd1", resp = "GrainYield", trt = "Insecticide"); run(pw = "LSD")
  x1 <- output$dl_xlsx; check(identical(readxl::excel_sheets(x1), c("ANOVA", "Publication table", "Means", "Info")), "xlsx sheets for a one-factor result")

  # ---- errors are reported, not thrown ----
  run(pw = "LSD", resp = "GrainYield", trt = "GrainYield")
  check(inherits(res(), "star_error"), "bad column choice is reported as an error")

  # ---- correlation screen ----
  session$setInputs(corr_src = "ex", corr_example = "cars", corr_vars = c("mpg", "hp", "wt"), corr_method = "pearson", corr_alt = "two.sided",
                    corr_adjust = "none", corr_alpha = "0.05", go_corr = 1)
  check(round(corr_ok()$r["mpg", "wt"], 4) == -0.8677, "Correlation: r(mpg, wt) = -0.8677 (original STAR)")
  check(identical(corr_tables()$matrix$mpg, c("1", "-0.776***", "-0.868***")), "Correlation: matrix with significance stars")
  check(identical(names(corr_tables()$pairs)[c(3, 5)], c("r", "p")) && corr_tables()$pairs$p[1] == "<0.0001", "Correlation: pairs table")
  ui <- html(output$corr_results_ui); check(grepl("3 of 3", ui) && grepl("r = -0.868", ui, fixed = TRUE), "Correlation: key numbers")
  r <- corr_ok()
  run_script <- function(txt) { e <- new.env(); old <- setwd(tempdir()); on.exit(setwd(old)); invisible(capture.output(eval(parse(text = txt), e))); e }   # in another folder: the script must not need the app files
  e <- run_script(corr_script()); check(isTRUE(all.equal(e$pairs$r, r$pairs$r)) && isTRUE(all.equal(e$pairs$P, r$pairs$P)) && isTRUE(all.equal(e$pairs$PAdj, r$pairs$PAdj)) && isTRUE(all.equal(e$pairs$N, r$pairs$N)) && isTRUE(all.equal(unname(e$r_matrix), unname(r$r))), "Correlation script: Pearson reproduces the app")
  session$setInputs(corr_view = "matrix"); check(file.size(output$dl_corr_plot) > 3000, "Correlation: scatter plot matrix png")
  session$setInputs(corr_view = "heat"); p <- output$dl_corr_plot; check(file.exists(p) && file.size(p) > 3000, "Correlation: heatmap png")
  session$setInputs(corr_view = "scatter", corr_x = "mpg", corr_y = "wt"); check(file.size(output$dl_corr_plot) > 3000, "Correlation: scatter png")
  x <- output$dl_corr_xlsx; check(identical(readxl::excel_sheets(x), c("Correlation (r)", "P values", "N", "Pairs", "Info")), "Correlation: xlsx sheets")
  session$setInputs(corr_method = "spearman", go_corr = 2)
  check(!("CI lower" %in% names(corr_tables()$pairs)) && names(corr_tables()$pairs)[3] == "rho", "Correlation: Spearman has rho and no CI")
  r <- corr_ok()
  e <- run_script(corr_script()); check(isTRUE(all.equal(e$pairs$r, r$pairs$r)) && isTRUE(all.equal(e$pairs$P, r$pairs$P)) && isTRUE(all.equal(e$pairs$PAdj, r$pairs$PAdj)) && isTRUE(all.equal(e$pairs$N, r$pairs$N)) && isTRUE(all.equal(unname(e$r_matrix), unname(r$r))), "Correlation script: Spearman reproduces the app")
  session$setInputs(corr_method = "kendall", corr_adjust = "holm", corr_alt = "greater", go_corr = 4); r <- corr_ok()
  e <- run_script(corr_script()); check(isTRUE(all.equal(e$pairs$r, r$pairs$r)) && isTRUE(all.equal(e$pairs$P, r$pairs$P)) && isTRUE(all.equal(e$pairs$PAdj, r$pairs$PAdj)) && isTRUE(all.equal(e$pairs$N, r$pairs$N)) && isTRUE(all.equal(unname(e$r_matrix), unname(r$r))), "Correlation script: Kendall, Holm, one-sided reproduces the app")
  session$setInputs(corr_method = "spearman", corr_adjust = "none", corr_alt = "two.sided", go_corr = 5)
  session$setInputs(corr_vars = "mpg", go_corr = 3); check(inherits(corr_res(), "star_error"), "Correlation: one variable is reported as an error")
  session$setInputs(cp_corr_matrix = 1, cp_corr_pairs = 1, cp_corr_apa = 1)

  # ---- regression screen ----
  session$setInputs(reg_src = "ex", reg_example = "cars_multi", reg_resp = "mpg", reg_pred = c("wt", "hp"), reg_degree = "1", reg_alpha = "0.05", go_reg = 1)
  check(round(reg_ok()$anova$F[1], 2) == 69.21 && round(reg_ok()$fit$adj_r2, 4) == 0.8148, "Regression: F = 69.21, adjusted R-squared = 0.8148 (original STAR)")
  check(identical(reg_tables()$coef$Term, c("(Intercept)", "wt", "hp")) && "VIF" %in% names(reg_tables()$coef), "Regression: coefficient table with VIF")
  r <- reg_ok()
  e <- run_script(reg_script()); check(isTRUE(all.equal(unname(coef(e$fit)), unname(coef(r$model)))) && isTRUE(all.equal(unname(summary(e$fit)$fstatistic[1]), r$fit$f)) && isTRUE(all.equal(summary(e$fit)$adj.r.squared, r$fit$adj_r2)) && isTRUE(all.equal(AIC(e$fit), r$fit$aic)) && isTRUE(all.equal(unname(confint(e$fit)[, 1]), r$coefficients$Lower)) && isTRUE(all.equal(unname(e$vif), r$coefficients$VIF[-1])) && isTRUE(all.equal(unname(e$beta), r$coefficients$StdBeta[-1])), "Regression script: multiple regression reproduces the app (coefficients, F, AIC, CI, VIF, beta)")
  ui <- html(output$reg_results_ui); check(grepl("R-squared", ui) && grepl("0.827", ui, fixed = TRUE) && grepl("Parameter estimates", ui), "Regression: key numbers and result cards")
  check(file.size(output$dl_reg_plot) > 3000, "Regression: observed vs fitted png")
  x <- output$dl_reg_xlsx; check(identical(readxl::excel_sheets(x), c("Model", "ANOVA", "Coefficients", "Residuals")), "Regression: xlsx sheets")
  session$setInputs(reg_example = "seeding", reg_resp = "GrainYield", reg_pred = "SeedingRate", reg_degree = "2", go_reg = 2)
  check(!is.null(reg_ok()$optimum) && reg_ok()$optimum$inside, "Regression: quadratic finds an optimum inside the range")
  r <- reg_ok()
  e <- run_script(reg_script()); check(isTRUE(all.equal(unname(coef(e$fit)), unname(coef(r$model)))) && isTRUE(all.equal(unname(summary(e$fit)$fstatistic[1]), r$fit$f)) && isTRUE(all.equal(summary(e$fit)$adj.r.squared, r$fit$adj_r2)) && isTRUE(all.equal(AIC(e$fit), r$fit$aic)) && isTRUE(all.equal(unname(confint(e$fit)[, 1]), r$coefficients$Lower)) && isTRUE(all.equal(unname(e$x_opt), r$optimum$x)) && isTRUE(all.equal(e$y_opt, r$optimum$y)), "Regression script: quadratic reproduces the app and its optimum")
  session$setInputs(reg_degree = "3", go_reg = 21); r <- reg_ok()
  e <- run_script(reg_script()); check(isTRUE(all.equal(unname(coef(e$fit)), unname(coef(r$model)))) && isTRUE(all.equal(unname(summary(e$fit)$fstatistic[1]), r$fit$f)) && isTRUE(all.equal(summary(e$fit)$adj.r.squared, r$fit$adj_r2)) && isTRUE(all.equal(AIC(e$fit), r$fit$aic)) && isTRUE(all.equal(unname(confint(e$fit)[, 1]), r$coefficients$Lower)) , "Regression script: cubic reproduces the app")
  session$setInputs(reg_degree = "2", go_reg = 22)
  check(file.size(output$dl_reg_plot) > 3000, "Regression: curve png")
  session$setInputs(reg_newx = "50, 100, 400"); check(identical(reg_predictions()$Extrapolated, c(FALSE, FALSE, TRUE)), "Regression: predictions flag extrapolation")

  # ---- t-test screen ----
  check(grepl("No results yet", html(output$tt_msg)), "t-test: empty state before the first run")
  session$setInputs(tt_src = "ex", tt_example = "tooth", tt_type = "independent", tt_resp = "len", tt_group = "supp", tt_levels = c("OJ", "VC"),
                    tt_alt = "two.sided", tt_var = "welch", tt_alpha = "0.05", go_tt = 1)
  r <- tt_ok()
  check(round(r$test$t[2], 3) == 1.915 && round(r$test$DF[2], 2) == 55.31 && round(r$p, 4) == 0.0606 && !r$significant, "t-test: Welch t = 1.915, df = 55.31, p = 0.0606 (R t.test)")
  check(identical(tt_tables()$test$Used, c("", "yes")) && round(r$test$DF[1]) == 58, "t-test: pooled and Welch rows, Welch used")
  # the R code offered to the user, run on its own, must reproduce the numbers on the screen
  run_script <- function(txt) { e <- new.env(); old <- setwd(tempdir()); on.exit(setwd(old)); invisible(capture.output(eval(parse(text = txt), e))); e }   # in another folder: the script must not need the app files
  e <- run_script(tt_script()); check(isTRUE(all.equal(unname(e$tt_welch$statistic), r$test$t[2])) && isTRUE(all.equal(e$tt$p.value, r$p)) && isTRUE(all.equal(e$var_test$p.value, r$variance$P)) && isTRUE(all.equal(e$wilcox$p.value, r$nonparametric$P)), "t-test script: independent samples reproduce the app (Welch, F test, Wilcoxon)")
  ui <- html(output$tt_results_ui); check(grepl("Difference in means", ui) && grepl("Cohen", ui) && grepl("F test", ui), "t-test: key numbers and variance test")
  session$setInputs(tt_var = "equal", go_tt = 2); check(tt_ok()$chosen == 1 && round(tt_ok()$p, 4) == 0.0604, "t-test: pooled version when variances assumed equal")
  e <- run_script(tt_script()); check(isTRUE(all.equal(e$tt$p.value, tt_ok()$p)) && isTRUE(all.equal(unname(e$tt$statistic), tt_ok()$test$t[1])), "t-test script: pooled version when variances assumed equal")
  session$setInputs(tt_example = "insect", tt_resp = "GrainYield", tt_group = "Insecticide", tt_levels = c("Dol-Mix (2kg)", "Control"), tt_var = "welch", go_tt = 3)
  check(tt_ok()$significant && any(grepl("other 5 group", tt_ok()$data_notes)), "t-test: two of seven groups, the rest noted")
  session$setInputs(tt_example = "sleep", tt_type = "paired", tt_x = "Drug1", tt_y = "Drug2", go_tt = 4)
  r <- tt_ok(); check(round(r$test$t, 3) == -4.062 && r$test$DF == 9 && round(r$p, 4) == 0.0028, "t-test: paired t = -4.062, df = 9, p = 0.0028 (Student's sleep data)")
  check(identical(tt_tables()$desc$Measurement, c("Drug1", "Drug2", "Drug1 - Drug2")), "t-test: paired descriptives include the differences")
  e <- run_script(tt_script()); check(isTRUE(all.equal(unname(e$tt$statistic), r$test$t)) && isTRUE(all.equal(e$tt$p.value, r$p)) && isTRUE(all.equal(e$wilcox$p.value, r$nonparametric$P)), "t-test script: paired reproduces the app")
  check(isTRUE(all.equal(r$nonparametric$P, suppressWarnings(wilcox.test(read.csv("example_sleep.csv")$Drug1, read.csv("example_sleep.csv")$Drug2, paired = TRUE)$p.value))) && !grepl("Hodges-Lehmann", html(output$tt_results_ui)), "t-test: Wilcoxon signed-rank computed (wilcox.test) but not shown beside the t-test")
  check(file.size(output$dl_tt_plot) > 3000, "t-test: paired plot png")
  session$setInputs(tt_example = "rice", tt_type = "one", tt_resp = "Yield", tt_mu = 3, go_tt = 5)
  r <- tt_ok(); ref <- t.test(read.csv("example_rice.csv")$Yield, mu = 3)
  check(isTRUE(all.equal(r$test$t, unname(ref$statistic))) && isTRUE(all.equal(c(r$test$Lower, r$test$Upper), ref$conf.int[1:2] - 3)), "t-test: one sample matches t.test, CI of the difference")
  # the Wilcoxon test chosen by the user replaces the t-test
  session$setInputs(tt_method = "wilcox", go_tt = 6)
  ui <- html(output$tt_results_ui); r <- tt_ok()
  check(tt_rank() && grepl("Hodges-Lehmann", ui) && !grepl("Mean - test value", ui) && grepl("Wilcoxon signed-rank", r$nonparametric_apa) && grepl(r$nonparametric_apa, ui, fixed = TRUE),
        "Wilcoxon method: replaces the t-test on the results screen, with its APA sentence")
  check(isTRUE(all.equal(r$groups$Median[1], median(read.csv("example_rice.csv")$Yield))) && grepl("Median", ui), "Wilcoxon method: medians shown")
  session$setInputs(tt_example = "tooth", tt_type = "independent", tt_resp = "len", tt_group = "supp", tt_levels = c("OJ", "VC"), go_tt = 7)
  check(grepl("W = 575.5", tt_ok()$nonparametric_summary) && grepl("Mann-Whitney", html(output$tt_results_ui)), "Wilcoxon method: rank-sum test for two independent groups")
  session$setInputs(tt_method = "t", tt_example = "rice", tt_type = "one", tt_resp = "Yield", tt_mu = 3, go_tt = 8)
  check(!tt_rank() && grepl("Mean - test value", html(output$tt_results_ui)), "Wilcoxon method: back to the t-test")
  # trimmed means for two groups: Yuen's test, the trimmed mean of the differences, and of the values against mu
  for (case in list(list(ex = "tooth", type = "independent", resp = "len", group = "supp", levels = c("OJ", "VC")), list(ex = "sleep", type = "paired", x = "Drug1", y = "Drug2"),
                    list(ex = "rice", type = "one", resp = "Yield", mu = 3))) {
    session$setInputs(tt_example = case$ex, tt_type = case$type, tt_method = "trimmed")
    if (case$type == "independent") session$setInputs(tt_resp = case$resp, tt_group = case$group, tt_levels = case$levels)
    if (case$type == "paired") session$setInputs(tt_x = case$x, tt_y = case$y)
    if (case$type == "one") session$setInputs(tt_resp = case$resp, tt_mu = case$mu)
    session$setInputs(go_tt = 100 + which(c("tooth", "sleep", "rice") == case$ex))
    s <- tt_trim_ok(); e <- run_script(tt_trim_script())
    check(isTRUE(all.equal(unname(e$res[c("t", "df", "p", "boot_lower", "boot_upper")]), unname(unlist(s$table[c("t", "DF", "P", "BootLower", "BootUpper")])))) && grepl("Bootstrap interval", html(output$tt_results_ui)),
          paste("Trimmed means, two groups:", case$type, "script reproduces the app (t, df, p, bootstrap)"))
  }
  session$setInputs(tt_method = "t")
  check(grepl("Data check|Observation", html(output$tt_results_ui)) && !grepl("Check with", html(output$tt_results_ui)) && !is.null(tt_rec()), "Guidance: t-test screen has the data check, with no Wilcoxon result beside it")
  rc <- star_recommend_tt(star_ttest(skew[skew$T != "c", ], "independent", response = "y", group = "T"))
  check(rc$light == "red" && identical(vapply(rc$options, function(o) o$id, ""), c("wilcox", "trimmed")) && grepl("x IQR", rc$observed), "Guidance: skewed groups are red, suggest Wilcoxon or trimmed means, and point to values beyond the quartiles")
  xs <- c(-1.2, -0.6, -0.3, 0, 0.3, 0.6, 1.2, -0.9, 0.9, 0)
  two_het <- data.frame(g = rep(c("a", "b"), each = 10), y = c(10 + xs, 12 + 8 * xs)); two_eq <- data.frame(g = rep(c("a", "b"), each = 10), y = c(10 + xs, 12 + xs))
  rc <- star_recommend_tt(star_ttest(two_het, "independent", response = "y", group = "g", var_equal = TRUE))
  check(identical(rc$switch_to, "welch_var") && grepl("does not assume equal variances", rc$advice), "Pooled t-test default: Welch recommended when the variances differ")
  check(is.null(star_recommend_tt(star_ttest(two_het, "independent", response = "y", group = "g"))$switch_to) &&
        is.null(star_recommend_tt(star_ttest(two_eq, "independent", response = "y", group = "g", var_equal = TRUE))$switch_to),
        "Pooled t-test default: no switch when Welch is already used, or when the variances look similar")
  e <- run_script(tt_script()); check(isTRUE(all.equal(unname(e$tt$statistic), r$test$t)) && isTRUE(all.equal(e$tt$p.value, r$p)) && isTRUE(all.equal(e$wilcox$p.value, r$nonparametric$P)), "t-test script: one sample reproduces the app")
  check(grepl("t.test(v, mu = mu", tt_script(), fixed = TRUE) && grepl("R code to repeat", html(output$tt_results_ui)), "t-test script: shown on the results screen")
  # scripts must not depend on the app: example data are carried inside, files are read as the app reads them
  check(grepl("read.csv(text = ", tt_script(), fixed = TRUE) && !grepl("example_rice.csv", tt_script(), fixed = TRUE), "Script: a built-in example carries its data")
  e <- new.env(); eval(parse(text = script_parse_number), e)
  txt <- c("1,250.5", "12.5%", "12 kg", "12,5", "1.250,5", "", "NA", "-", "$ 7", "3")
  check(isTRUE(all.equal(e$parse_number(txt), star_parse_number(txt))), "Script: numbers are parsed as the app parses them")
  semi <- tempfile(fileext = ".csv"); writeLines(c("a;b;c", "1,5;x;3", "2,5;y;4"), semi)
  e <- new.env(); invisible(capture.output(eval(parse(text = script_header("x", semi)), e)))
  check(identical(names(e$d), c("a", "b", "c")) && identical(e$parse_number(e$d$a), c(1.5, 2.5)), "Script: a semicolon file is read with its separator and a decimal comma is parsed")
  e <- new.env(); eval(parse(text = grep("shapiro_safe <-", tt_script(), value = TRUE, fixed = TRUE)[1]), e)
  check(is.null(e$shapiro_safe(c(1, 2))) && is.null(e$shapiro_safe(c(5, 5, 5))) && !is.null(e$shapiro_safe(c(1, 2, 4))), "Script: Shapiro-Wilk is skipped where the app skips it (fewer than 3 values, all equal)")
  check(file.size(output$dl_tt_plot) > 3000, "t-test: one-sample plot png")
  x <- output$dl_tt_xlsx; check(identical(readxl::excel_sheets(x), c("Descriptives", "t-test", "Normality", "Info")), "t-test: xlsx sheets (only the test chosen)")
  check(grepl("สรุปผล", html(output$tt_results_ui)) && file.size(output$dl_tt_doc) > 10000, "t-test: Thai summary and Word report")
  session$setInputs(tt_type = "paired", tt_x = "Yield", tt_y = "Yield", go_tt = 6); check(!isTRUE(tryCatch(is.list(tt_res()), error = function(e) FALSE)) || inherits(tt_res(), "star_error"), "t-test: same column twice is refused")
  session$setInputs(cp_tt_desc = 1, cp_tt_test = 1, cp_tt_apa = 1, home_tt = 1)
  cat("ok   t-test copy buttons ran
")

  # ---- plot style and download size ----
  png_size <- function(f) { b <- readBin(f, "raw", 24); c(sum(as.integer(b[17:20]) * 256^(3:0)), sum(as.integer(b[21:24]) * 256^(3:0))) }
  session$setInputs(reg_pal = "grey", reg_theme = "bw", reg_size = "9", reg_legend = "none")
  g <- gg_reg(); check(g$theme$text$size == 9 && identical(g$theme$legend.position, "none") && inherits(g$theme$panel.border, "element_rect"), "Plot style: font size, legend and theme are applied")
  check(identical(ggplot2::layer_data(g, 3)$fill[1], "grey60"), "Plot style: greyscale points")
  for (pal in style_choices$pal) for (th in style_choices$theme) { session$setInputs(reg_pal = pal, reg_theme = th); ggplot2::ggplot_build(gg_reg()) }
  cat("ok   regression plot builds with every palette and theme\n")
  session$setInputs(reg_fmt = "png", reg_dpi = "300", reg_dim = "j1")
  check(all(abs(png_size(output$dl_reg_plot) - round(c(8.5, 6.5) / 2.54 * 300)) <= 1), "Download size: journal 1 column at 300 dpi")
  session$setInputs(reg_dim = "custom", reg_w = 10, reg_h = 5)
  check(all(abs(png_size(output$dl_reg_plot) - round(c(10, 5) / 2.54 * 300)) <= 1), "Download size: custom 10 x 5 cm")
  session$setInputs(reg_w = NA); check(all(abs(png_size(output$dl_reg_plot) - round(c(20, 14) / 2.54 * 300)) <= 1), "Download size: empty custom size falls back to standard")
  session$setInputs(reg_dim = "std", reg_pal = "okabe", reg_theme = "classic", reg_size = "12", reg_legend = "auto")
  two <- star_aov(read.csv("example_rcbd2.csv"), "Yield", c("Nitrogen", "Variety"), "Rep", design = "RCBD")
  for (pal in style_choices$pal) for (eff in c("Nitrogen", "interaction", "interaction_line"))
    ggplot2::ggplot_build(star_ggplot_means(two, eff, palette = pal, theme = "minimal", legend = "bottom"))
  cat("ok   means plot builds with every palette\n")
  session$setInputs(reg_pred = c("SeedingRate", "GrainYield"), go_reg = 3); check(inherits(reg_res(), "star_error"), "Regression: bad choice is reported as an error")
  session$setInputs(cp_reg_anova = 1, cp_reg_coef = 1, cp_reg_apa = 1)
  cat("ok   regression copy buttons ran\n")

  # ---- Home, Methods, About ----
  session$setInputs(home_aov = 1, home_corr = 1, home_reg = 1)
  check(grepl("R [0-9]+[.][0-9]+", methods_sentence("3.5.0")) && grepl("ggplot2 3.5.0", methods_sentence("3.5.0")), "Methods sentence names R and ggplot2 versions")
  check(nzchar(version_label()) && nzchar(app_name), "App name and version label exist")
  check(grepl("เอกสารอ้างอิงของซอฟต์แวร์", paste(as.character(output$methods_text$html), collapse = "")), "Methods page renders")
  mt <- paste(as.character(output$methods_text$html), collapse = "")
  check(grepl("<table", mt) && grepl("agricolae", mt) && !grepl("nortest|RDstat|cp_methods", mt), "Methods page: software table without nortest, the RDstat row or the English sentence")
  # Methods page by topic: each topic's references resolve, and hold only what that topic uses
  topic_of <- function(id) Filter(function(t) t$id == id, method_topics)[[1]]
  check(all(vapply(method_topics, function(t) all(nzchar(topic_refs(t))), NA)) && !anyDuplicated(vapply(method_topics, `[[`, "", "id")) &&
        all(vapply(method_topics, `[[`, "", "group") %in% names(method_groups)), "Methods topics: every reference key matches one reference, ids are unique, groups exist")
  ph <- topic_refs(topic_of("posthoc"))
  check(any(startsWith(ph, "Tukey JW (1949)")) && any(grepl("agricolae", ph, ignore.case = TRUE)) && !any(startsWith(ph, "Welch BL")) && any(startsWith(ph, "R Core Team")),
        "Methods topics: post hoc lists Tukey, agricolae and R, not Welch")
  tt_txt <- topic_copy_text(topic_of("ttest"))
  check(grepl("^t-test
Software: R [0-9.]+
Functions: stats::t.test", tt_txt) && grepl("Student (1908)", tt_txt, fixed = TRUE) && !grepl("agricolae", tt_txt), "Methods topics: copy text of the t-test topic")
  all_refs <- unlist(lapply(method_topics, topic_refs))
  check(!any(grepl("International Rice Research Institute", c(all_refs, package_refs()))) && !any(grepl("^STAR:", unlist(lapply(method_topics, `[[`, "software")))) &&
          any(startsWith(topic_refs(topic_of("rcbd")), "Yates F (1933)")) && any(grepl("mctest", topic_refs(topic_of("regression")))) && !length(topic_refs(topic_of("advice"))),
        "Methods topics: no STAR code cited; RCBD cites Yates, regression cites mctest; no references where none apply")
  # RCBD with missing plots: least-squares estimates (lm + predict), error df reduced by the number of estimates
  local({
    set.seed(1); dm <- expand.grid(Trt = factor(paste0("T", 1:6)), Block = factor(1:4))
    dm$y <- round(50 + as.integer(dm$Trt) * 3 + as.integer(dm$Block) * 2 + rnorm(24, 0, 3), 1); dm$y[c(3, 17)] <- NA
    r <- star_aov(dm, "y", "Trt", "Block", design = "RCBD")
    full <- dm; obs <- !is.na(dm$y); full$y[!obs] <- predict(lm(y ~ Block + Trt, dm[obs, ]), dm[!obs, ])
    a <- anova(lm(y ~ Block + Trt, full)); e <- r$anova[r$anova$Source == "Error", ]
    check(e$DF == 13 && isTRUE(all.equal(e$SS, a["Residuals", "Sum Sq"])) && isTRUE(all.equal(r$anova$SS[2], a["Trt", "Sum Sq"])) &&
            abs(r$anova$F[2] - 12.83) < 0.02 && any(grepl("least squares", r$data_notes)),
          "RCBD missing plots: estimated by lm() + predict(), error df 13, F close to STAR's 12.83")
    too_many <- dm; too_many$y[1:4] <- NA
    check(inherits(tryCatch(star_aov(too_many, "y", "Trt", "Block", design = "RCBD"), error = identity), "error"), "RCBD missing plots: more than 10% missing is refused, as in STAR")
  })
  mt <- paste(as.character(output$methods_topics$html), collapse = "")
  check(grepl("accordion", mt) && grepl("cp_topic_posthoc", mt) && grepl(method_groups[["alt"]], mt, fixed = TRUE), "Methods topics: the page shows the topics in groups with copy buttons")
  session$setInputs(cp_topic_posthoc = 1, cp_topic_ttest = 1)

  # ---- mean comparison comes from agricolae: letters must agree with the critical difference ----
  d <- read.csv("example_crd.csv"); d$Insecticide <- factor(d$Insecticide)
  fit <- aov(GrainYield ~ Insecticide, d); dfe <- df.residual(fit); mse <- deviance(fit) / dfe
  mm <- tapply(d$GrainYield, d$Insecticide, mean); cdiff <- sqrt(qf(0.99, 6, dfe) * 6 * 2 * mse / 4)
  sc <- .star_compare_means(d$GrainYield, d$Insecticide, "scheffe", dfe, mse, 0.01)
  share <- function(a, b) length(intersect(strsplit(a, "")[[1]], strsplit(b, "")[[1]])) > 0
  bad <- 0; for (i in 1:6) for (j in (i + 1):7) if (share(sc$Group[i], sc$Group[j]) != (abs(mm[i] - mm[j]) <= cdiff)) bad <- bad + 1
  check(bad == 0, "Scheffe letters follow the critical difference at alpha = 0.01")
  check(!identical(sc$Group, .star_compare_means(d$GrainYield, d$Insecticide, "scheffe", dfe, mse, 0.05)$Group), "Scheffe uses the chosen alpha")
  du <- d[-c(3, 7), ]; du$Insecticide <- droplevels(du$Insecticide)
  check(identical(.star_compare_means(du$GrainYield, du$Insecticide, "LSD", dfe, mse, 0.05)$N, c(4L, 4L, 4L, 4L, 4L, 3L, 3L)), "Group sizes are the real counts when the data are unbalanced")
  check(length(package_refs()) >= 7 && all(nzchar(package_refs())), "References are built for the packages used")
  session$setInputs(cp_mrefs = 1)
  check(length(method_refs) >= 20 && all(grepl("[0-9]{4}", method_refs)), "Method references are listed")
  check(any(grepl("Error(Block:A)", paste(as.character(lapply(method_topics, `[[`, "body")), collapse = ""), fixed = TRUE)), "Methods page describes the split-plot model")

  # ---- missing letters are explained, and Excel agrees with the screen ----
  session$setInputs(src = "ex", design = "CRD", example = "crd2", resp = "len", trt = c("supp", "dose")); run(pw = "auto")
  check(grepl("No letters", html(output$results_ui)) && grepl("interaction is significant", html(output$results_ui)), "Letters: note explains why main-effect letters are missing")
  m <- readxl::read_excel(output$dl_xlsx, "Means"); check(all(is.na(m$Group) | m$Group == ""), "Letters: Excel has no letters the screen does not show")
  ns <- list(factors = "Variety", alpha = 0.05, interaction_significant = FALSE, effects = data.frame(Effect = "Variety", P = 0.2345, Significant = FALSE))
  note <- paste(as.character(no_letters_note(ns, "Variety")), collapse = "")
  check(grepl("not significant (p = 0.2345", note, fixed = TRUE) && grepl("protected test", note), "Letters: note for a non-significant F test gives the p value")

  # ---- stale results are flagged with a Run again button ----
  session$setInputs(alpha = "0.01")
  check(aov_stale() && grepl("Run again", html(output$aov_msg)) && grepl("go_aov", html(output$aov_msg)), "Stale: changed settings give a Run again button")
  run(); check(!aov_stale(), "Stale: running again clears the flag")
  session$setInputs(alpha = "0.05")

  # ---- wide-format files ----
  w <- data.frame(Variety = c("A", "B", "C"), `Rep 1` = c(5, 6, 7), `Rep 2` = c(5.5, 6.2, 7.1), `Rep 3` = c(4.9, 6.4, 7.3), check.names = FALSE)
  check(identical(wide_rep_cols(w), c("Rep 1", "Rep 2", "Rep 3")) && identical(wide_rep_cols(read.csv("example_rcbd.csv")), character()), "Wide: replication columns are recognised, long files are left alone")
  check(identical(wide_rep_cols(setNames(w, c("พันธุ์", "ซ้ำที่ 1", "ซ้ำที่ 2", "ซ้ำที่ 3"))), c("ซ้ำที่ 1", "ซ้ำที่ 2", "ซ้ำที่ 3")), "Wide: Thai column names are recognised")
  lg <- wide_to_long(w, wide_rep_cols(w))
  check(identical(names(lg), c("Variety", "Rep", "Value")) && nrow(lg) == 9 && identical(lg$Rep, rep(c("1", "2", "3"), each = 3)) && lg$Value[5] == 6.2, "Wide: converted to one row per plot")
  f <- tempfile(fileext = ".csv"); write.csv(w, f, row.names = FALSE)
  session$setInputs(design = "RCBD", src = "up", file = list(datapath = f, name = "wide.csv", size = file.size(f), type = "text/csv"))
  check(grepl("wide format", html(output$wide_note)) && ncol(dat()) == 4, "Wide: upload is flagged and not converted until asked")
  session$setInputs(wide_convert = 1)
  check(nrow(dat()) == 9 && grepl("Converted", html(output$wide_note)), "Wide: converted on request")
  session$setInputs(resp = "Value", trt = "Variety", blk = "Rep"); run(pw = "LSD")
  check(!inherits(res(), "star_error") && ok()$anova$DF[ok()$anova$Source == "Error"] == 4, "Wide: converted data analysed as RCBD (error df 4)")
  session$setInputs(wide_undo = 1); check(ncol(dat()) == 4, "Wide: conversion can be undone")
  check(is.null(output$read_note) || !grepl("Skipped", html(output$read_note)), "Read note: none for a plain file")
  # a title line above the column names and summary rows below the data, as typed in Excel
  f2 <- tempfile(fileext = ".csv"); writeLines(c("Rice yield trial 2025", readLines(f), "\"Mean\",5.9,6,6.2"), f2)
  session$setInputs(file = list(datapath = f2, name = "titled.csv", size = file.size(f2), type = "text/csv"))
  check(nrow(dat()) == 3 && identical(names(dat())[1], "Variety") && grepl("Rice yield trial 2025", html(output$read_note)) && grepl("Mean", html(output$read_note)),
        "Read note: title line and summary row are skipped and shown")
  # an old Excel 97-2003 .xls file (first sheet: mtcars); numbers read as typed, not with 17 digits
  fx <- readxl::readxl_example("datasets.xls")
  session$setInputs(file = list(datapath = fx, name = "datasets.xls", size = file.size(fx), type = "application/vnd.ms-excel"))
  check(nrow(dat()) == 32 && identical(dat()$drat[1], "3.9") && identical(dat()$wt[1], "2.62"), "Upload: .xls file is read, numbers as typed")
  # the same in .xlsx, where the file stores 2.3199999999999998 for 2.32
  fx <- readxl::readxl_example("datasets.xlsx")
  session$setInputs(file = list(datapath = fx, name = "datasets.xlsx", size = file.size(fx), type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
  check(nrow(dat()) == 32 && identical(dat()$wt[3], "2.32") && identical(dat()$drat[3], "3.85"), "Upload: .xlsx file is read, numbers as typed")
  # the t test, correlation and regression screens take .xls files too (the same reader)
  fx <- readxl::readxl_example("datasets.xls")
  xls <- list(datapath = fx, name = "datasets.xls", size = file.size(fx), type = "application/vnd.ms-excel")
  session$setInputs(tt_src = "up", tt_file = xls, corr_src = "up", corr_file = xls, reg_src = "up", reg_file = xls)
  check(all(vapply(list(tt_dat(), corr_dat(), reg_dat()), function(d) nrow(d) == 32 && identical(d$wt[1], "2.62"), NA)),
        "Upload: .xls file is read on the t test, correlation and regression screens")
  session$setInputs(tt_src = "ex", corr_src = "ex", reg_src = "ex")
  # first choices of the variables for an upload come from the column names, not their order
  g <- guess_roles(setNames(data.frame(c("A", "A", "B", "B"), c(1, 2, 1, 2), c(5, 6, 4, 5)), c("พันธุ์", "ซ้ำ", "ผลผลิต")), "ผลผลิต")
  check(identical(g$blk, "ซ้ำ") && identical(g$trt[1], "พันธุ์"), "Roles: Thai variety first, ซ้ำ second gives ซ้ำ as the block")
  sp <- read.csv("example_split.csv"); g <- guess_roles(sp, "Yield")
  check(identical(g$blk, "Rep") && identical(g$trt[1:2], c("Nitrogen", "Variety")), "Roles: split plot file gives Rep as block and the factors in file order (main plot first)")
  cb <- read.csv("example_comb_rcbd.csv"); g <- guess_roles(cb, "GrainYield")
  check(identical(g$env, "Season") && identical(g$blk, "Rep") && identical(g$trt[1], "NitrogenRate"), "Roles: combined trial gives Season as environment")
  g <- guess_roles(data.frame(Treatment = rep(c("a", "b", "c"), 4), Height = c(5.1, 6.2, 4.3, 5.5, 6.1, 4.9, 5.2, 6.6, 4.1, 5.8, 6.3, 4.4), Yield = 1:12 + 0.5), "Yield")
  check(identical(g$trt[1], "Treatment") && !"Height" %in% g$trt, "Roles: another measured trait is not offered as a treatment")
  g <- guess_roles(data.frame(x = rep(1:3, 2), z = rep(c("a", "b"), each = 3), y = c(1.1, 2, 3.3, 4.2, 5, 6.7)), "y")
  check(identical(g$trt[1], "z") && identical(g$blk, "x"), "Roles: with no recognised names the block is the remaining label column")
  g <- guess_roles(data.frame(A = rep(c("a1", "a2"), each = 6), B = rep(c("b1", "b2", "b3"), 4), Rep = rep(1:2, 6), Y = 1:12 + 0.5), "Y")
  check(identical(g$blk, "Rep") && identical(g$trt[1:2], c("A", "B")), "Roles: factor B is a treatment, not the block, when there is a factor A")
  f4 <- tempfile(fileext = ".csv"); writeLines(c("พันธุ์,ซ้ำ,ผลผลิต", "กข41,1,5557", "กข41,2,5706", "กข47,1,4652", "กข47,2,4965"), f4)
  session$setInputs(design = "RCBD", src = "up", file = list(datapath = f4, name = "thai.csv", size = file.size(f4), type = "text/csv"))
  vp <- html(output$var_pickers)
  check(grepl("<option value=\"ซ้ำ\" selected>", sub(".*id=\"blk\"", "", vp)) && grepl("<option value=\"พันธุ์\" selected>", sub(".*id=\"trt\"(.*)id=\"blk\".*", "\\1", vp)),
        "Roles: shown as the first choices on the screen")
  # variety names typed once per group (merged cells in Excel, saved as CSV): every plot is kept and analysed
  f3 <- tempfile(fileext = ".csv")
  writeLines(c("Variety,Rep,Yield", "RD41,1,5557", ",2,5706", ",3,5410", "RD47,1,4652", ",2,4965", ",3,4810", "RD49,1,5120", ",2,5302", ",3,5050"), f3)
  session$setInputs(design = "RCBD", file = list(datapath = f3, name = "merged.csv", size = file.size(f3), type = "text/csv"))
  check(identical(dat()$Variety, rep(c("RD41", "RD47", "RD49"), each = 3)) && grepl("typed once", html(output$read_note)), "Merged labels: repeated on every row and shown")
  session$setInputs(resp = "Yield", trt = "Variety", blk = "Rep"); run(pw = "LSD")
  check(!inherits(res(), "star_error") && ok()$anova$DF[ok()$anova$Source == "Error"] == 4, "Merged labels: all 9 plots analysed as RCBD (error df 4)")
  # a title merged over a block of cells (A1:C2) is not repeated into every cell of it
  cells <- data.frame(a = c("Rice trial 2026", NA, "Trt", "A", "A", "B", "B"), b = c(NA, NA, "Rep", "1", "2", "1", "2"),
                      c = c(NA, NA, "Y", "4.5", "4.7", "5.1", "5.3"), stringsAsFactors = FALSE)
  rd <- star_table_from_cells(star_fill_merged(cells, cbind(r1 = 1L, c1 = 1L, r2 = 2L, c2 = 3L))$cells)
  check(identical(names(rd), c("Trt", "Rep", "Y")) && nrow(rd) == 4, "Merged title: block merged over several columns is read as a title")
  # a note typed beside one plot does not move the header down or drop plots
  f5 <- tempfile(fileext = ".csv"); writeLines(c("Trt,Rep,Y", "A,1,4.5", "A,2,4.7,missing plot", "B,1,5.1", "B,2,5.3"), f5)
  rd <- star_read_table(f5)
  check(identical(names(rd), c("Trt", "Rep", "Y")) && nrow(rd) == 4 && any(grepl("missing plot", attr(rd, "notes"))), "Read: a note beside one plot is left out and the header kept")
  session$setInputs(src = "ex")

  # ---- split plot: the block / replication column is checked before running ----
  sp <- read.csv("example_splitcrd.csv")
  ok_crd <- paste(as.character(split_layout_check(sp, "SplitCRD", "Irrigation", "Variety", "Rep")), collapse = "")
  check(grepl("Error(a) has 9 df", ok_crd, fixed = TRUE) && grepl("6 df", ok_crd), "Split check: CRD shows Error(a) df 9 and the RCBD alternative (6)")
  sp$Plot <- as.integer(interaction(sp$Rep, sp$Irrigation, drop = TRUE))
  check(grepl("numbers the main plots across the whole trial", paste(as.character(split_layout_check(sp, "SplitRCBD", "Irrigation", "Variety", "Plot")), collapse = "")), "Split check: a main-plot number chosen as block is caught")
  sp$Row <- seq_len(nrow(sp))
  check(grepl("numbers the sub plots", paste(as.character(split_layout_check(sp, "SplitCRD", "Irrigation", "Variety", "Row")), collapse = "")), "Split check: a sub-plot number is caught")
  rb <- paste(as.character(split_layout_check(read.csv("example_split.csv"), "SplitRCBD", "Nitrogen", "Variety", "Rep")), collapse = "")
  check(grepl("Error(a) has 10 df", rb, fixed = TRUE), "Split check: RCBD example gives Error(a) df (3-1)(6-1) = 10")
  session$setInputs(design = "SplitRCBD", example = "split1", trt_main = "Nitrogen", trt_sub = "Variety", blk = "Rep")
  check(grepl("Error(a) has", html(output$split_check), fixed = TRUE), "Split check: shown on the screen")
})
