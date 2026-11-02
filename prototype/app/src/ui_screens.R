corr_ui <- sidebarLayout(
  sidebarPanel(
    side_step(1, "Data", data_source_ui("corr", corr_examples)),
    side_step(2, "Variables and method",
      uiOutput("corr_pickers"),
      selectInput("corr_method", "Method", c("Pearson (linear)" = "pearson", "Spearman (ranks)" = "spearman", "Kendall (ranks)" = "kendall")),
      help_box("What does this mean?", p("Correlation measures how strongly two variables move together, from -1 to +1. Pearson suits straight-line relationships of measured values; Spearman and Kendall use ranks and suit curved or skewed data. A small p value (below the significance level) means the correlation is unlikely to be zero. It does not show cause and effect."))),
    advanced_box(
      selectInput("corr_alt", "Alternative", c("Two-sided" = "two.sided", "Positive (greater)" = "greater", "Negative (less)" = "less")),
      selectInput("corr_adjust", "Adjust p values for many pairs", c("None (as in STAR)" = "none", "Holm" = "holm", "Bonferroni" = "bonferroni", "Benjamini-Hochberg" = "BH")),
      selectInput("corr_alpha", "Significance level", c("0.05" = 0.05, "0.01" = 0.01, "0.10" = 0.10))),
    run_bar("go_corr", "Run correlation")),
  mainPanel(
    uiOutput("corr_msg"),
    div(id = "corr_out", tabsetPanel(
      tabPanel("Results", br(), stale_dim(uiOutput("corr_results_ui")), br(), stale_dim(conditionalPanel("input.go_corr > 0", downloadButton("dl_corr_xlsx", "Download Excel (.xlsx)")))),
      tabPanel("Data", br(), uiOutput("corr_data_ui")),
      tabPanel("Plot", br(), stale_dim(uiOutput("corr_view_picker")), plot_labels_ui("corr"), plot_style_ui("corr"), plot_live_ui("corr_plot", "corr"), plot_export_ui("corr"))))))

reg_ui <- sidebarLayout(
  sidebarPanel(
    side_step(1, "Data", data_source_ui("reg", reg_examples)),
    side_step(2, "Variables and model",
      uiOutput("reg_pickers"),
      conditionalPanel("!input.reg_pred || input.reg_pred.length <= 1",
        radioButtons("reg_degree", "Shape of the relationship", c("Straight line" = "1", "Quadratic (curve with a peak)" = "2", "Cubic" = "3"))),
      help_box("What does this mean?", p("Regression describes how the response (y) changes with the predictor(s) (x). R-squared is the share of the variation in y explained by the model (1 = all). A coefficient with p below the significance level contributes to the model. Predictions outside the range of your data are extrapolations and unreliable."))),
    advanced_box(selectInput("reg_alpha", "Significance level", c("0.05" = 0.05, "0.01" = 0.01, "0.10" = 0.10))),
    run_bar("go_reg", "Run regression")),
  mainPanel(
    uiOutput("reg_msg"),
    div(id = "reg_out", tabsetPanel(
      tabPanel("Results", br(), stale_dim(uiOutput("reg_results_ui")), br(), stale_dim(conditionalPanel("input.go_reg > 0", downloadButton("dl_reg_xlsx", "Download Excel (.xlsx)")))),
      tabPanel("Data", br(), uiOutput("reg_data_ui")),
      tabPanel("Plot", br(), plot_labels_ui("reg"), plot_style_ui("reg"), plot_live_ui("reg_plot", "reg"), plot_export_ui("reg")),
      tabPanel("Outliers & assumptions", br(), stale_dim(uiOutput("reg_diag_ui")), stale_dim(plotOutput("reg_diag_plot", height = "440px"))),
      tabPanel("Predict", br(), stale_dim(uiOutput("reg_predict_ui")))))))

tt_examples <- c("Tooth growth: orange juice vs vitamin C (two groups)" = "tooth", "Insecticides: choose two of seven (two groups)" = "insect",
                 "Sleep gain: drug 1 vs drug 2 on the same patients (paired)" = "sleep", "Rice yield vs a target of 3 (one sample)" = "rice")
tt_setup <- list(tooth = list(file = "example_crd2.csv", type = "independent", resp = "len", group = "supp"),
                 insect = list(file = "example_crd.csv", type = "independent", resp = "GrainYield", group = "Insecticide", levels = c("Dol-Mix (2kg)", "Control")),
                 sleep = list(file = "example_sleep.csv", type = "paired", x = "Drug1", y = "Drug2"),
                 rice = list(file = "example_rice.csv", type = "one", resp = "Yield", mu = 3))

tt_ui <- sidebarLayout(
  sidebarPanel(
    side_step(1, "Data", data_source_ui("tt", tt_examples, long_format_help())),
    side_step(2, "Kind of comparison",
      radioButtons("tt_type", NULL, c("Two independent groups" = "independent", "Paired: the same units measured twice" = "paired",
                                      "One sample against a value" = "one")),
      help_box("Which one?",
        p(strong("Two independent groups:"), "different plots, plants or animals in each group (e.g. variety A on some plots, variety B on others). The data have a value column and a group column."),
        p(strong("Paired:"), "each unit was measured twice or under both treatments (e.g. before and after, left and right half of a leaf). The data have one row per unit and the two measurements in two columns."),
        p(strong("One sample:"), "is the mean different from a known or target value?")),
      radioButtons("tt_method", "Test", c("t-test: compare means" = "t", "Wilcoxon: rank-based" = "wilcox",
                                          "Trimmed means (20%): Yuen's test, bootstrap CI" = "trimmed")),
      help_box("t-test or Wilcoxon?",
        p(strong("t-test"), " compares means and assumes the values (or the differences, when paired) are roughly normal. With 30 or more per group it is robust."),
        p(strong("Trimmed means"), " compare means after setting aside the lowest and highest 20% of the values, so skewed data and a few extreme values do not drive the result; Yuen's test for two groups, the trimmed mean of the differences for paired data. A bootstrap interval is given beside the t-based one."),
        p(strong("Wilcoxon"), " compares ranks, so it does not need normal data: the rank-sum (Mann-Whitney) test for two independent groups, the signed-rank test for paired data and for one sample. It reports medians and the Hodges-Lehmann estimate instead of means."),
        p("Choose the test before looking at the results, not the one that gives the smaller p value."))),
    side_step(3, "Variables", uiOutput("tt_pickers")),
    advanced_box(
      selectInput("tt_alt", "Alternative", c("Two-sided (different)" = "two.sided", "Greater: first > second (or > test value)" = "greater",
                                             "Less: first < second (or < test value)" = "less")),
      conditionalPanel("input.tt_type == 'independent'",
        radioButtons("tt_var", "Variances of the two groups", c("Assumed equal (pooled, Student): the usual t-test" = "equal", "Not assumed equal (Welch)" = "welch"))),
      selectInput("tt_alpha", "Significance level", c("0.05" = 0.05, "0.01" = 0.01, "0.10" = 0.10))),
    run_bar("go_tt", "Run test")),
  mainPanel(
    uiOutput("tt_msg"),
    div(id = "tt_out", tabsetPanel(
      tabPanel("Results", br(), stale_dim(uiOutput("tt_results_ui")), br(), stale_dim(conditionalPanel("input.go_tt > 0", downloadButton("dl_tt_xlsx", "Download Excel (.xlsx)"), downloadButton("dl_tt_doc", "Download report (Word)")))),
      tabPanel("Data", br(), uiOutput("tt_data_ui")),
      tabPanel("Plot", br(), plot_labels_ui("tt"), plot_style_ui("tt"), plot_live_ui("tt_plot", "tt"), plot_export_ui("tt"))))))
