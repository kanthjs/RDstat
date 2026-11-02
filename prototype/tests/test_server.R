# Headless test of the app's server logic (native R). Run from anywhere: Rscript prototype/tests/test_server.R
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
setwd(file.path(here, "..", "app"))
suppressMessages(library(shiny))
app <- source("app.R")$value
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE) else cat("ok  ", msg, "\n")

testServer(app, {
  n <- 0
  run <- function(...) { n <<- n + 1; session$setInputs(..., go_aov = n) }
  session$setInputs(src = "ex", alpha = "0.05", pw = "auto")

  # ---- RCBD, one factor (rice yield vs nitrogen) ----
  session$setInputs(design = "RCBD", example = "rcbd1", resp = "Yield", trt = "Nitrogen", blk = "Rep")
tests <- c(auto = "Tukeys", LSD = "Least Significant", HSD = "Tukeys", duncan = "Duncan")
  for (pw in names(tests)) { run(pw = pw); check(any(grepl(tests[[pw]], report_text())), paste("RCBD 1 factor: mean comparison", pw, "is used")) }
  run(pw = "LSD")
  check(round(ok()$anova$F[2], 2) == 8.54, "RCBD 1 factor: F = 8.54")
  check(identical(tables()$anova$`Pr(>F)`[2], "0.0022"), "RCBD 1 factor: p formatted 0.0022")
  check(identical(tables()$means$Nitrogen$Group, c("b", "a", "a", "a", "a", "a")), "RCBD 1 factor: letters")
  check(identical(tables()$means$Nitrogen$N, rep("3", 6)), "RCBD 1 factor: N as integers")
  html <- function(x) paste(as.character(x$html), collapse = "")
  ui <- html(output$results_ui); check(grepl("Significant effects", ui) && grepl("1 of 1", ui) && grepl("9.4%", ui, fixed = TRUE) && grepl("Highest mean of Nitrogen", ui) && grepl("result-card", ui), "RCBD 1 factor: key numbers and result cards")
  check(grepl("No results yet", html(output$corr_msg)), "Correlation: empty state before the first run")

  # ---- CRD, one factor (insecticides) ----
  session$setInputs(design = "CRD", example = "crd1", resp = "GrainYield", trt = "Insecticide")
  run(pw = "HSD")
  check(is.null(ok()$block) && round(ok()$anova$F[1], 2) == 9.83, "CRD 1 factor: no block, F = 9.83")
  check(identical(tables()$means$Insecticide$Group, c("ab", "c", "a", "bc", "bc", "ab", "a")), "CRD 1 factor: letters match the original")
  check(!grepl("assumptions are not met", html(output$results_ui)) && !grepl("ข้อตกลงเบื้องต้น", html(output$results_ui)), "Results tab: no verdict on the assumptions")
  check(round(np_res()$statistic, 2) == 20.85 && np_res()$name == "Kruskal-Wallis" && grepl("Rank-based alternative", html(output$diag_ui)), "CRD 1 factor: Kruskal-Wallis chi-square = 20.85 (kruskal.test)")

  # ---- CRD, two factors (tooth growth) ----
  session$setInputs(design = "CRD", example = "crd2", resp = "len", trt = c("supp", "dose"))
  run(pw = "auto")
  r <- ok()
  check(identical(r$anova$Source, c("supp", "dose", "supp:dose", "Error", "Total")), "CRD 2 factors: ANOVA rows")
  check(r$interaction_significant, "CRD 2 factors: interaction significant")
  check(setequal(names(tables()$simple), c("supp within dose", "dose within supp")), "CRD 2 factors: simple-effect tables")
  check(identical(names(tables()$cells), c("supp", "dose = 0.5", "dose = 1", "dose = 2")), "CRD 2 factors: wide cell means")
  check(identical(tables()$simple[["supp within dose"]][["dose = 0.5"]], c("13.23 a", "7.98 b")), "CRD 2 factors: simple effect letters (original STAR)")
  check(!("Group" %in% names(tables()$means$supp)), "CRD 2 factors: no main-effect letters when the interaction is significant")

  # ---- RCBD, two factors (nitrogen x variety) ----
  session$setInputs(design = "RCBD", example = "rcbd2", resp = "Yield", trt = c("Nitrogen", "Variety"), blk = "Rep")
  run(pw = "auto")
  r <- ok()
  check(identical(r$anova$Source, c("Rep", "Nitrogen", "Variety", "Nitrogen:Variety", "Error", "Total")), "RCBD 2 factors: ANOVA rows")
  check(round(r$cv, 2) == 10.07, "RCBD 2 factors: CV = 10.07")
  check(identical(tables()$simple[["Nitrogen within Variety"]][["Variety = IR8"]][1:2], c("4,252.67 d", "5,672.00 c")), "RCBD 2 factors: Nitrogen within IR8 letters")
  check("Simple effects" %in% names(xlsx_sheets()), "RCBD 2 factors: simple effects in Excel")

  # ---- split plot in RCBD (nitrogen main plot x variety sub plot) ----
  session$setInputs(design = "SplitRCBD", example = "split1", resp = "Yield", trt_main = "Nitrogen", trt_sub = "Variety", blk = "Rep")
  run(pw = "auto")
  r <- ok()
  check(identical(r$anova$Source, c("Rep", "Nitrogen", "Error(a)", "Variety", "Nitrogen:Variety", "Error(b)", "Total")), "Split plot: ANOVA rows")
  check(round(c(r$cv_a, r$cv_b), 2)[2] == 10.79 && round(r$cv_a, 2) == 6.88, "Split plot: CV(a) = 6.88, CV(b) = 10.79")
  check(identical(tables()$simple[["Nitrogen within Variety"]][["Variety = IR8"]][1:2], c("4,252.67 d", "5,672.00 c")), "Split plot: Nitrogen within IR8 letters (original STAR)")
  check(grepl("Error (a) / Error (b)", html(output$results_ui), fixed = TRUE), "Split plot: CV(a) and CV(b) in the key numbers")
  check(grepl("Highest cell mean", html(output$results_ui)), "Significant interaction: highest cell mean instead of an overall mean")
  check(length(output$report) > 0 && file.size(output$dl_xlsx) > 3000, "Split plot: report and Excel")
  check(is.null(r$report) && grepl("Split Plot in Randomized Complete Block", output$report), "Split plot: classic report built on demand")
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
  check(grepl("Split Plot in Completely Randomized Design", output$report), "Split plot CRD: classic STAR report")
  check(grepl("not available for split-plot", html(output$diag_ui)), "Split plot CRD: no assumption checks yet")
  check(identical(tables()$agronomy$Irrigation[[1]][4:9], c("F test (Irrigation)", "Grand mean", "CV (%)", "SE(m)", "SED", "LSD (0.05)")),
        "Split plot CRD: publication footer rows")
  pr <- precision_of(r, "Irrigation"); ea <- r$anova[r$anova$Source == "Error(a)", ]
  check(isTRUE(all.equal(pr[["sed"]], sqrt(2 * ea$MS / 16))) && pr[["df"]] == 9, "Split plot CRD: SED of the main plot uses Error(a)")

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
  check(identical(round(r$anova$F[1:4], 2), c(14.25, 0.71, 10.62, 5.47)) && round(r$cv, 2) == 12.05, "Combined RCBD: F and CV match STAR")
  ui <- html(output$results_ui)
  check(grepl("Analysis of each Season", ui) && grepl("Bartlett", ui), "Combined RCBD: each environment and Bartlett's test")
  check(grepl("Combined Analysis of Variance", output$report), "Combined RCBD: classic STAR report")
  check("Each environment" %in% readxl::excel_sheets(output$dl_xlsx), "Combined RCBD: Excel sheet for each environment")
  check(!is.null(r$diagnostics), "Combined RCBD: assumption checks")
  session$setInputs(design = "CombCRD", example = "comb_crd1", resp = "Tillers", trt = "Variety", env = "Site")
  run(pw = "LSD")
  check(identical(ok()$anova$DF, c(2, 3, 6, 36, 47)), "Combined CRD: ANOVA df")

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
  t <- output$dl_txt; check(length(readLines(t)) > 40, "txt report download")
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
  session$setInputs(corr_view = "matrix"); check(file.size(output$dl_corr_plot) > 3000, "Correlation: scatter plot matrix png")
  session$setInputs(corr_view = "heat"); p <- output$dl_corr_plot; check(file.exists(p) && file.size(p) > 3000, "Correlation: heatmap png")
  session$setInputs(corr_view = "scatter", corr_x = "mpg", corr_y = "wt"); check(file.size(output$dl_corr_plot) > 3000, "Correlation: scatter png")
  x <- output$dl_corr_xlsx; check(identical(readxl::excel_sheets(x), c("Correlation (r)", "P values", "N", "Pairs", "Info")), "Correlation: xlsx sheets")
  session$setInputs(corr_method = "spearman", go_corr = 2)
  check(!("CI lower" %in% names(corr_tables()$pairs)) && names(corr_tables()$pairs)[3] == "rho", "Correlation: Spearman has rho and no CI")
  session$setInputs(corr_vars = "mpg", go_corr = 3); check(inherits(corr_res(), "star_error"), "Correlation: one variable is reported as an error")
  session$setInputs(cp_corr_matrix = 1, cp_corr_pairs = 1, cp_corr_apa = 1)

  # ---- regression screen ----
  session$setInputs(reg_src = "ex", reg_example = "cars_multi", reg_resp = "mpg", reg_pred = c("wt", "hp"), reg_degree = "1", reg_alpha = "0.05", go_reg = 1)
  check(round(reg_ok()$anova$F[1], 2) == 69.21 && round(reg_ok()$fit$adj_r2, 4) == 0.8148, "Regression: F = 69.21, adjusted R-squared = 0.8148 (original STAR)")
  check(identical(reg_tables()$coef$Term, c("(Intercept)", "wt", "hp")) && "VIF" %in% names(reg_tables()$coef), "Regression: coefficient table with VIF")
  ui <- html(output$reg_results_ui); check(grepl("R-squared", ui) && grepl("0.827", ui, fixed = TRUE) && grepl("Parameter estimates", ui), "Regression: key numbers and result cards")
  check(file.size(output$dl_reg_plot) > 3000, "Regression: observed vs fitted png")
  x <- output$dl_reg_xlsx; check(identical(readxl::excel_sheets(x), c("Model", "ANOVA", "Coefficients", "Residuals")), "Regression: xlsx sheets")
  session$setInputs(reg_example = "seeding", reg_resp = "GrainYield", reg_pred = "SeedingRate", reg_degree = "2", go_reg = 2)
  check(!is.null(reg_ok()$optimum) && reg_ok()$optimum$inside, "Regression: quadratic finds an optimum inside the range")
  check(file.size(output$dl_reg_plot) > 3000, "Regression: curve png")
  session$setInputs(reg_newx = "50, 100, 400"); check(identical(reg_predictions()$Extrapolated, c(FALSE, FALSE, TRUE)), "Regression: predictions flag extrapolation")

  # ---- t-test screen ----
  check(grepl("No results yet", html(output$tt_msg)), "t-test: empty state before the first run")
  session$setInputs(tt_src = "ex", tt_example = "tooth", tt_type = "independent", tt_resp = "len", tt_group = "supp", tt_levels = c("OJ", "VC"),
                    tt_alt = "two.sided", tt_var = "welch", tt_alpha = "0.05", go_tt = 1)
  r <- tt_ok()
  check(round(r$test$t[2], 3) == 1.915 && round(r$test$DF[2], 2) == 55.31 && round(r$p, 4) == 0.0606 && !r$significant, "t-test: Welch t = 1.915, df = 55.31, p = 0.0606 (R t.test)")
  check(identical(tt_tables()$test$Used, c("", "yes")) && round(r$test$DF[1]) == 58, "t-test: pooled and Welch rows, Welch used")
  ui <- html(output$tt_results_ui); check(grepl("Difference in means", ui) && grepl("Cohen", ui) && grepl("F test", ui), "t-test: key numbers and variance test")
  session$setInputs(tt_var = "equal", go_tt = 2); check(tt_ok()$chosen == 1 && round(tt_ok()$p, 4) == 0.0604, "t-test: pooled version when variances assumed equal")
  session$setInputs(tt_example = "insect", tt_resp = "GrainYield", tt_group = "Insecticide", tt_levels = c("Dol-Mix (2kg)", "Control"), tt_var = "welch", go_tt = 3)
  check(tt_ok()$significant && any(grepl("other 5 group", tt_ok()$data_notes)), "t-test: two of seven groups, the rest noted")
  session$setInputs(tt_example = "sleep", tt_type = "paired", tt_x = "Drug1", tt_y = "Drug2", go_tt = 4)
  r <- tt_ok(); check(round(r$test$t, 3) == -4.062 && r$test$DF == 9 && round(r$p, 4) == 0.0028, "t-test: paired t = -4.062, df = 9, p = 0.0028 (Student's sleep data)")
  check(identical(tt_tables()$desc$Measurement, c("Drug1", "Drug2", "Drug1 - Drug2")), "t-test: paired descriptives include the differences")
  check(isTRUE(all.equal(r$nonparametric$P, suppressWarnings(wilcox.test(read.csv("example_sleep.csv")$Drug1, read.csv("example_sleep.csv")$Drug2, paired = TRUE)$p.value))) && grepl("Rank-based", html(output$tt_results_ui)), "t-test: Wilcoxon signed-rank alternative (wilcox.test)")
  check(file.size(output$dl_tt_plot) > 3000, "t-test: paired plot png")
  session$setInputs(tt_example = "rice", tt_type = "one", tt_resp = "Yield", tt_mu = 3, go_tt = 5)
  r <- tt_ok(); ref <- t.test(read.csv("example_rice.csv")$Yield, mu = 3)
  check(isTRUE(all.equal(r$test$t, unname(ref$statistic))) && isTRUE(all.equal(c(r$test$Lower, r$test$Upper), ref$conf.int[1:2] - 3)), "t-test: one sample matches t.test, CI of the difference")
  check(file.size(output$dl_tt_plot) > 3000, "t-test: one-sample plot png")
  x <- output$dl_tt_xlsx; check(identical(readxl::excel_sheets(x), c("Descriptives", "t-test", "Wilcoxon", "Normality", "Info")), "t-test: xlsx sheets")
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
  two <- star_aov(read.csv("example_rcbd2.csv"), "Yield", c("Nitrogen", "Variety"), "Rep", design = "RCBD", report = FALSE)
  for (pal in style_choices$pal) for (eff in c("Nitrogen", "interaction", "interaction_line"))
    ggplot2::ggplot_build(star_ggplot_means(two, eff, palette = pal, theme = "minimal", legend = "bottom"))
  cat("ok   means plot builds with every palette\n")
  session$setInputs(reg_pred = c("SeedingRate", "GrainYield"), go_reg = 3); check(inherits(reg_res(), "star_error"), "Regression: bad choice is reported as an error")
  session$setInputs(cp_reg_anova = 1, cp_reg_coef = 1, cp_reg_apa = 1)
  cat("ok   regression copy buttons ran\n")

  # ---- Home, Methods, About ----
  session$setInputs(home_aov = 1, home_corr = 1, home_reg = 1, cp_methods = 1)
  check(grepl("R [0-9]+[.][0-9]+", methods_sentence("3.5.0")) && grepl("ggplot2 3.5.0", methods_sentence("3.5.0")), "Methods sentence names R and ggplot2 versions")
  check(nzchar(version_label()) && nzchar(app_name), "App name and version label exist")
  check(grepl("ผลแต่ละส่วนคำนวณจากอะไร", paste(as.character(output$methods_text$html), collapse = "")), "Methods page renders")

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
  # the classic STAR report and the tables agree on the Scheffe letters
  rep_sc <- star_aov(d, "GrainYield", "Insecticide", NULL, design = "CRD", test = "scheffe", alpha = 0.05, report = TRUE)
  rl <- grep("^(Azodin|Control|DDT|Dimercon|Dol-Mix)", rep_sc$report, value = TRUE)
  check(identical(trimws(sub("^.*[0-9] +", "", rl)), rep_sc$means$Insecticide$Group), "Classic report and tables give the same Scheffe letters")
  session$setInputs(cp_mrefs = 1)
  check(length(method_refs) >= 20 && all(grepl("[0-9]{4}", method_refs)), "Method references are listed")
  check(any(grepl("Error(Block:A)", paste(as.character(methods_sections), collapse = ""), fixed = TRUE)), "Methods page describes the split-plot model")

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
