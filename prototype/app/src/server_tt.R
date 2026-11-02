  # ---- t-test ----
  tt_dat <- use_data("tt", tt_setup)
  output$tt_data_ui <- renderUI(data_preview_ui(tt_dat()))
  observeEvent(input$tt_example, { s <- tt_setup[[input$tt_example]]; if (!is.null(s) && input$tt_src == "ex") updateRadioButtons(session, "tt_type", selected = s$type) })
  output$tt_pickers <- renderUI({
    d <- tt_dat(); nm <- names(d); num <- numeric_cols(d); s <- example_of("tt", tt_setup)
    if (!is.null(s) && !identical(s$type, input$tt_type)) s <- NULL   # the example is for another kind of test
    switch(input$tt_type,
      independent = {
        resp <- if (!is.null(s)) s$resp else tail(num, 1)
        tagList(selectInput("tt_resp", "Response (numeric)", num, selected = resp),
                selectInput("tt_group", "Group column", setdiff(nm, resp), selected = if (!is.null(s)) s$group else setdiff(nm, num)[1]),
                uiOutput("tt_level_picker"))
      },
      paired = tagList(
        selectInput("tt_x", "First measurement", num, selected = if (!is.null(s)) s$x else num[1]),
        selectInput("tt_y", "Second measurement", num, selected = if (!is.null(s)) s$y else num[min(2, length(num))]),
        helpText("One row per unit; the test is on the differences first - second.")),
      one = tagList(
        selectInput("tt_resp", "Variable (numeric)", num, selected = if (!is.null(s)) s$resp else tail(num, 1)),
        numericInput("tt_mu", "Test value (the mean is compared with this)", if (!is.null(s)) s$mu else 0)))
  })
  output$tt_level_picker <- renderUI({
    req(input$tt_group, input$tt_group %in% names(tt_dat()))
    g <- tt_dat()[[input$tt_group]]
    lv <- star_level_order(unique(star_squash_space(as.character(g[!star_is_blank(g)]))))
    s <- example_of("tt", tt_setup)
    sel <- if (!is.null(s) && identical(s$group, input$tt_group) && !is.null(s$levels)) s$levels else head(lv, 2)
    tagList(selectizeInput("tt_levels", "Groups to compare (first, second)", lv, selected = sel, multiple = TRUE, options = list(maxItems = 2)),
            if (length(lv) > 2) helpText(sprintf("This column has %d groups; the t-test compares two. For all of them use Compare treatments.", length(lv))))
  })
  observeEvent(input$tt_resp, {   # the response cannot also be the group column
    if (identical(input$tt_type, "independent") && identical(input$tt_group, input$tt_resp))
      updateSelectInput(session, "tt_group", selected = setdiff(names(tt_dat()), input$tt_resp)[1])
  }, ignoreInit = TRUE)
  tt_res <- reactive({
    alpha <- as.numeric(input$tt_alpha); d <- tt_dat()
    tryCatch(switch(input$tt_type,
      independent = { req(input$tt_resp, input$tt_group)
        validate(need(length(input$tt_levels) == 2, "Choose two groups to compare."))
        star_ttest(d, "independent", response = input$tt_resp, group = input$tt_group, levels = input$tt_levels,
                   var_equal = identical(input$tt_var, "equal"), alternative = input$tt_alt, alpha = alpha) },
      paired = { req(input$tt_x, input$tt_y)
        validate(need(input$tt_x != input$tt_y, "Choose two different columns for the two measurements."))
        star_ttest(d, "paired", x = input$tt_x, y = input$tt_y, alternative = input$tt_alt, alpha = alpha) },
      one = { req(input$tt_resp)
        validate(need(is.numeric(input$tt_mu) && !is.na(input$tt_mu), "Enter the test value."))
        star_ttest(d, "one", response = input$tt_resp, mu = input$tt_mu, alternative = input$tt_alt, alpha = alpha) }),
      error = function(e) if (inherits(e, "shiny.silent.error") || inherits(e, "validation")) stop(e) else as_error(e))
  }) |> bindEvent(input$go_tt)
  tt_ok <- reactive({ r <- tt_res(); req(!inherits(r, "star_error")); r })
  tt_stale <- stale_flag("go_tt", reactive(list(input$tt_src, input$tt_example, input$tt_file$name, input$tt_type, input$tt_resp, input$tt_group,
                                                input$tt_levels, input$tt_x, input$tt_y, input$tt_mu, input$tt_alt, input$tt_var, input$tt_alpha)))
  # the test shown follows the Test buttons at once (both tests are computed on every run)
  tt_rank <- reactive(identical(input$tt_method, "wilcox"))
  tt_trimmed <- reactive(identical(input$tt_method, "trimmed"))
  tt_trim <- reactive(tryCatch(star_tt_trimmed(tt_ok()), error = as_error))
  tt_trim_ok <- reactive({ s <- tt_trim(); req(!inherits(s, "star_error")); s })
  # the summary, APA sentence, Thai summary and name of the test chosen, for the screen and the downloads
  tt_main <- reactive({
    r <- tt_ok()
    if (tt_trimmed()) { s <- tt_trim_ok(); list(summary = s$summary, apa = s$apa, thai = thai_summary_tt_trim(r, s), name = s$method) }
    else if (tt_rank()) list(summary = r$nonparametric_summary, apa = r$nonparametric_apa, thai = thai_summary_tt_np(r), name = r$nonparametric$Method)
    else list(summary = r$summary, apa = r$apa, thai = thai_summary_tt(r), name = r$method_name)
  })
  # which test suits the data (advice only), and whether the user chose to keep theirs
  tt_rec <- reactive(tryCatch(star_recommend_tt(tt_ok()), error = function(e) NULL))
  tt_kept <- reactiveVal(FALSE)
  observeEvent(input$go_tt, tt_kept(FALSE))
  observeEvent(input$tt_rec_stay, tt_kept(TRUE))
  for (id in c("wilcox", "trimmed")) local({ m <- id; observeEvent(input[[paste0("tt_rec_", m)]], updateRadioButtons(session, "tt_method", selected = m)) })
  observeEvent(input$tt_rec_welch_var, updateRadioButtons(session, "tt_var", selected = "welch"))   # needs a new run: the screen says so
  tt_current <- reactive(if (tt_trimmed()) "trimmed" else if (tt_rank()) "wilcox" else if (isTRUE(tt_ok()$var_equal)) "pooled" else "welch_var")
  tt_advice <- reactive(advice_ui(tt_rec(), tt_current(), "tt_rec", tt_kept(),
                                  if (tt_trimmed()) "the trimmed-means test" else if (tt_rank()) "the Wilcoxon test" else if (isTRUE(tt_ok()$var_equal)) "the pooled t-test" else "Welch's t-test"))
  tt_trim_display <- function(r, s) { z <- s$table; k <- tt_digits(r)
    out <- data.frame(Method = z$Method, t = fmt_n(z$t, 3), DF = fmt_n(z$DF), p = fmt_p(z$P), Difference = fmt_n(z$Estimate, k), SE = fmt_n(z$SE, k),
                      Lower = fmt_n(z$Lower, k), Upper = fmt_n(z$Upper, k), `Bootstrap lower` = fmt_n(z$BootLower, k), `Bootstrap upper` = fmt_n(z$BootUpper, k),
                      check.names = FALSE, stringsAsFactors = FALSE)
    names(out)[7:8] <- paste0(c("Lower ", "Upper "), format(100 * r$conf_level), "% CI")
    if (r$alternative != "two.sided") names(out)[4] <- paste0("p (one-sided, ", r$alternative, ")")
    out }
  # the results screen when trimmed means are chosen
  tt_trim_ui <- function(r, tb) {
    s <- tt_trim()
    if (inherits(s, "star_error")) return(tagList(tt_advice(), error_box(paste("The trimmed-means test could not be run:", s$message))))
    z <- s$table; k <- tt_digits(r)
    tagList(
      tt_advice(),
      notes_ui(r$data_notes),
      div(class = if (s$significant) "alert alert-success" else "alert alert-info", s$summary),
      key_stats(
        stat("p value", fmt_p(z$P), if (s$significant) paste("significant at", format(r$alpha)) else paste("not significant at", format(r$alpha))),
        stat(if (r$type == "independent") "Difference in trimmed means" else "Trimmed mean difference", fmt_n(z$Estimate, k),
             sprintf("%g%% CI %s to %s", 100 * r$conf_level, fmt_n(z$Lower, k), fmt_n(z$Upper, k))),
        stat("Bootstrap interval", sprintf("%s to %s", fmt_n(z$BootLower, k), fmt_n(z$BootUpper, k)), sprintf("percentile, %d resamples", s$B)),
        stat("Trimmed means", paste(fmt_n(s$trimmed_means, k), collapse = " / "), paste(r$groups$Group[!is.na(r$groups$N)], collapse = " / "))),
      if (s$n_min < 10) div(class = "alert alert-light small", icon("circle-info"), sprintf(" With %d values%s, 20%% trimming sets aside few or no values and the bootstrap interval is rough.", s$n_min, if (r$type == "independent") " in the smaller group" else "")),
      thai_card(thai_summary_tt_trim(r, s), "cp_tt_th"),
      result_card("Descriptive statistics", html_table(tb$desc), copy = "cp_tt_desc", copy_label = "Copy table"),
      result_card(s$method, html_table(tt_trim_display(r, s)),
        p(class = "text-muted small mb-0", "Winsorized variances; no normal distribution or equal variances assumed. The bootstrap interval uses a fixed seed, so the same data give the same interval."),
        copy = "cp_tt_trim", copy_label = "Copy table"),
      result_card("APA-style report", p(s$apa), copy = "cp_tt_apa"),
      code_card(tt_trim_script(), "cp_tt_trim_script"),
      p(class = "text-muted small", icon("circle-info"), " The t-test is not shown because trimmed means are chosen under Kind of comparison."))
  }
  output$tt_msg <- renderUI({
    if (!isTRUE(input$go_tt > 0)) return(run_hint("Run test"))
    r <- tt_res()
    tagList(if (tt_stale()) stale_note("go_tt"), if (inherits(r, "star_error")) error_box(r$message))
  })
  tt_tables <- reactive({ r <- tt_ok(); list(desc = tt_desc_display(r), test = tt_test_display(r), normal = tt_normal_display(r), nonpar = tt_nonpar_display(r)) })

  # the results screen when the Wilcoxon test is chosen: it takes the place of the t-test
  tt_rank_ui <- function(r, tb) {
    w <- r$nonparametric; k <- tt_digits(r); sig <- !is.na(w$P) && w$P < r$alpha
    g <- r$groups[!is.na(r$groups$N), ]   # the groups or measurements, not the row of their difference
    tagList(
      notes_ui(r$data_notes),
      tt_advice(),
      div(class = if (sig) "alert alert-success" else "alert alert-info", r$nonparametric_summary),
      key_stats(
        stat("p value", fmt_p(w$P), if (sig) paste("significant at", format(r$alpha)) else paste("not significant at", format(r$alpha))),
        stat(if (r$type == "independent") "Hodges-Lehmann shift" else "Hodges-Lehmann estimate", fmt_n(w$Estimate, k),
             if (is.na(w$Lower)) "no interval" else sprintf("%g%% CI %s to %s", 100 * r$conf_level, fmt_n(w$Lower, k), fmt_n(w$Upper, k))),
        stat("Statistic", sub(" = .*", "", w$Statistic), sub(".* = ", "", w$Statistic)),
        stat("Medians", paste(fmt_n(g$Median, k), collapse = " / "), paste(g$Group, collapse = " / "))),
      thai_card(thai_summary_tt_np(r), "cp_tt_th"),
      result_card("Descriptive statistics", html_table(tb$desc), copy = "cp_tt_desc", copy_label = "Copy table"),
      result_card(w$Method, html_table(tb$nonpar),
        p(class = "text-muted small mb-0", "Estimate = Hodges-Lehmann ", if (r$type == "independent") "shift between the groups" else "median difference",
          "; with tied values (or zero differences), or 50 or more values, the p value and the interval use a normal approximation."),
        copy = "cp_tt_np", copy_label = "Copy table"),
      result_card("APA-style report", p(r$nonparametric_apa), copy = "cp_tt_apa"),
      code_card(tt_script(), "cp_tt_script"),
      p(class = "text-muted small", icon("circle-info"), " The t-test is not shown because the Wilcoxon test is chosen under Kind of comparison."))
  }
  output$tt_results_ui <- renderUI({
    r <- tt_ok(); tb <- tt_tables()
    if (tt_trimmed()) return(tt_trim_ui(r, tb))
    if (tt_rank()) return(tt_rank_ui(r, tb))
    tr <- r$test[r$chosen, ]; k <- tt_digits(r)
    nrm <- r$normality; bad_norm <- nrm$Data[!is.na(nrm$P) & nrm$P < r$alpha]
    small <- min(nrm$N) < 30
    tagList(
      notes_ui(r$data_notes),
      tt_advice(),
      div(class = if (r$significant) "alert alert-success" else "alert alert-info", r$summary),
      key_stats(
        stat("p value", fmt_p(r$p), if (r$significant) paste("significant at", format(r$alpha)) else paste("not significant at", format(r$alpha))),
        stat(switch(r$type, independent = "Difference in means", paired = "Mean difference", one = "Mean - test value"), fmt_n(tr$Difference, k),
             sprintf("%g%% CI %s to %s", 100 * r$conf_level, fmt_n(tr$Lower, k), fmt_n(tr$Upper, k))),
        stat("t statistic", sprintf("%.2f", tr$t), paste("df =", .star_tt_df(tr$DF))),
        stat(if (r$type == "paired") "Cohen's dz" else "Cohen's d", sprintf("%.2f", r$effect_size), .star_tt_effect_word(r$effect_size))),
      thai_card(thai_summary_tt(r), "cp_tt_th"),
      result_card("Descriptive statistics", html_table(tb$desc),
        if (r$type == "paired") p(class = "text-muted small mb-0", sprintf("Correlation between the two measurements: r = %.3f.", r$pair_r)),
        copy = "cp_tt_desc", copy_label = "Copy table"),
      result_card("t-test", html_table(tb$test),
        if (r$type == "independent") p(class = "text-muted small mb-0", sprintf("Both versions are shown, as in STAR; the conclusion uses the %s test (change it under Advanced options). Welch's test does not need equal variances and is the safer default.",
                                                                  if (r$var_equal) "pooled (equal variances)" else "Welch")),
        copy = "cp_tt_test", copy_label = "Copy table"),
      result_card("Assumption checks", html_table(tb$normal),
        if (!is.null(r$variance)) p(sprintf("Equality of variances, F test: F(%d, %d) = %.2f, p %s. %s", r$variance$DF1, r$variance$DF2, r$variance$F, .star_fmt_p(r$variance$P),
                                            "For information only: Welch's test is the default whatever this result, because choosing the test from a variance test distorts its error rate.")),
        p(class = "text-muted small mb-0",
          if (length(bad_norm) && small) "The data depart from a normal distribution and the sample is small, so the t-test may be unreliable; check the plot for outliers, or choose the Wilcoxon test under Kind of comparison."
          else if (length(bad_norm)) "The data depart somewhat from a normal distribution, but with 30 or more observations per group the t-test is usually still reliable."
          else "The normality tests are only a guide: with few observations they have little power, so also look at the plot.")),
      result_card("APA-style report", p(r$apa), copy = "cp_tt_apa"),
      code_card(tt_script(), "cp_tt_script"))
  })
  gg_tt <- reactive(apply_plot_customizations(do.call(star_ggplot_ttest, c(list(tt_ok(), main = input$tt_title, xlab = input$tt_xlab, ylab = input$tt_ylab), plot_style("tt"))), "tt"))
  output$tt_plot <- renderPlot(print(gg_tt()),
    width = function() live_plot_dims("tt")$width,
    height = function() live_plot_dims("tt")$height
  )
  output$dl_tt_plot <- plot_download("tt", gg_tt)
  tt_script <- reactive(script_tt(tt_ok(), tt_file_name()))
  tt_trim_script <- reactive(script_tt_trimmed(tt_ok(), tt_trim_ok(), tt_file_name()))
  tt_file_name <- reactive(script_file(input$tt_src == "up", if (input$tt_src == "up") input$tt_file$name else tt_setup[[input$tt_example]]$file))
  copy_button("cp_tt_th", "สรุปผล", function() tt_main()$thai)
  output$dl_tt_doc <- downloadHandler("rdstat_ttest_report.doc", function(file) {
    r <- tt_ok(); tb <- tt_tables()
    png <- tryCatch({ f <- tempfile(fileext = ".png"); star_ggsave(gg_tt(), f, width = 6.5, height = 4.5, dpi = 200); f }, error = function(e) NULL)
    blocks <- c(w_p(paste("Data:", tt_file_name() %||% "-", "| Date:", format(Sys.Date()), "|", app_name, version_label()), "small"),
      w_h2("สรุปผล"), w_p(tt_main()$thai), w_p(tt_main()$summary),
      w_h2("Descriptive statistics"), word_table(tb$desc),
      if (tt_trimmed()) c(w_h2(tt_trim_ok()$method), word_table(tt_trim_display(r, tt_trim_ok())))
      else if (tt_rank()) c(w_h2(r$nonparametric$Method), word_table(tb$nonpar)) else c(w_h2("t-test"), word_table(tb$test)),
      w_h2("Assumption checks"), word_table(tb$normal),
      if (!is.null(r$variance)) w_p(sprintf("Equality of variances, F test: F(%d, %d) = %.2f, p %s.", r$variance$DF1, r$variance$DF2, r$variance$F, .star_fmt_p(r$variance$P))),
      w_h2("APA-style report"), w_p(tt_main()$apa),
      w_h2("Methods and references"), w_p(methods_sentence(gg_version())), paste0("<ul>", paste0("<li>", htmltools::htmlEscape(package_refs()), "</li>", collapse = ""), "</ul>"))
    word_doc(file, paste(app_name, "-", tt_main()$name), blocks, png)
  })
  output$dl_tt_xlsx <- downloadHandler("rdstat_ttest.xlsx", function(file) {
    r <- tt_ok()
    # only the test chosen, as on the screen
    sheets <- c(list(Descriptives = r$groups), if (tt_trimmed()) list(`Trimmed means` = tt_trim_ok()$table) else if (tt_rank()) list(Wilcoxon = r$nonparametric) else list(`t-test` = r$test),
                list(Normality = r$normality))
    if (!is.null(r$variance)) sheets$Variances <- r$variance
    vars <- switch(r$type, independent = sprintf("%s by %s (%s vs %s)", r$response, r$group, r$labels[1], r$labels[2]),
                   paired = paste(r$x, "-", r$y), one = sprintf("%s vs %s", r$response, format(r$mu)))
    sheets$Info <- data.frame(Item = c("App version", "Test", "Variables", "Alternative", "Alpha", "Effect size (Cohen's d)", "Summary", "Data checks"),
                              Value = c(version_label(), tt_main()$name, vars, r$alternative, r$alpha, round(r$effect_size, 4), tt_main()$summary,
                                        if (length(r$data_notes)) paste(r$data_notes, collapse = " | ") else "none"))
    writexl::write_xlsx(sheets, file)
  })
  copy_button("cp_tt_desc", "Descriptive statistics", function() tsv(tt_tables()$desc))
  copy_button("cp_tt_test", "t-test table", function() tsv(tt_tables()$test))
  copy_button("cp_tt_np", "Wilcoxon table", function() tsv(tt_tables()$nonpar))
  copy_button("cp_tt_apa", "APA text", function() tt_main()$apa)
  copy_button("cp_tt_trim", "Trimmed-means table", function() tsv(tt_trim_display(tt_ok(), tt_trim_ok())))
  copy_button("cp_tt_trim_script", "R code", function() tt_trim_script())
  copy_button("cp_tt_script", "R code", function() tt_script())

