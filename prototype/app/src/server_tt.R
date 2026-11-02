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
            if (length(lv) > 2) helpText(sprintf("This column has %d groups; the t-test compares two. For all of them use Analysis of variance.", length(lv))))
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
  output$tt_msg <- renderUI({
    if (!isTRUE(input$go_tt > 0)) return(run_hint("Run t-test"))
    r <- tt_res()
    tagList(if (tt_stale()) stale_note("go_tt"), if (inherits(r, "star_error")) error_box(r$message))
  })
  tt_tables <- reactive({ r <- tt_ok(); list(desc = tt_desc_display(r), test = tt_test_display(r), normal = tt_normal_display(r), nonpar = tt_nonpar_display(r)) })

  output$tt_results_ui <- renderUI({
    r <- tt_ok(); tb <- tt_tables(); tr <- r$test[r$chosen, ]; k <- tt_digits(r)
    nrm <- r$normality; bad_norm <- nrm$Data[!is.na(nrm$P) & nrm$P < r$alpha]
    small <- min(nrm$N) < 30
    tagList(
      notes_ui(r$data_notes),
      div(class = if (r$significant) "alert alert-success" else "alert alert-info", r$summary),
      key_stats(
        stat("p value", fmt_p(r$p), if (r$significant) paste("significant at", format(r$alpha)) else paste("not significant at", format(r$alpha))),
        stat(switch(r$type, independent = "Difference in means", paired = "Mean difference", one = "Mean - test value"), fmt_n(tr$Difference, k),
             sprintf("%g%% CI %s to %s", 100 * r$conf_level, fmt_n(tr$Lower, k), fmt_n(tr$Upper, k))),
        stat("t statistic", sprintf("%.2f", tr$t), paste("df =", .star_tt_df(tr$DF))),
        stat(if (r$type == "paired") "Cohen's dz" else "Cohen's d", sprintf("%.2f", r$effect_size), .star_tt_effect_word(r$effect_size))),
      result_card("สรุปผล (ภาษาไทย)", p(lang = "th", thai_summary_tt(r)), copy = "cp_tt_th", copy_label = "คัดลอก"),
      result_card("Descriptive statistics", html_table(tb$desc),
        if (r$type == "paired") p(class = "text-muted small mb-0", sprintf("Correlation between the two measurements: r = %.3f.", r$pair_r)),
        copy = "cp_tt_desc", copy_label = "Copy table"),
      result_card("t-test", html_table(tb$test),
        if (r$type == "independent") p(class = "text-muted small mb-0", sprintf("Both versions are shown, as in STAR; the conclusion uses the %s test (change it under Advanced options). Welch's test does not need equal variances and is the safer default.",
                                                                  if (r$var_equal) "pooled (equal variances)" else "Welch")),
        copy = "cp_tt_test", copy_label = "Copy table"),
      result_card("Assumption checks", html_table(tb$normal),
        if (!is.null(r$variance)) p(sprintf("Equality of variances, F test: F(%d, %d) = %.2f, p %s. %s", r$variance$DF1, r$variance$DF2, r$variance$F, .star_fmt_p(r$variance$P),
                                            if (r$variance$P < r$alpha) "The variances differ, so use the Welch test." else "No evidence that the variances differ.")),
        p(class = "text-muted small mb-0",
          if (length(bad_norm) && small) "The data depart from a normal distribution and the sample is small, so the t-test may be unreliable; check the plot for outliers, or use the rank-based (Wilcoxon) test below or a transformation."
          else if (length(bad_norm)) "The data depart somewhat from a normal distribution, but with 30 or more observations per group the t-test is usually still reliable."
          else "The normality tests are only a guide: with few observations they have little power, so also look at the plot.")),
      result_card("Rank-based alternative (nonparametric)", html_table(tb$nonpar),
        div(class = paste("small mb-0", if (length(bad_norm) && small) "alert alert-warning" else "text-muted"),
            "Does not assume a normal distribution. ", if (length(bad_norm) && small) strong("Report this test, because the data are not normal and the sample is small. "),
            "Estimate = Hodges-Lehmann ", if (r$type == "independent") "shift between the groups" else "median difference", "; ",
            "with tied values (or zero differences) the p value and the interval use a normal approximation."),
        copy = "cp_tt_np", copy_label = "Copy table"),
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
  tt_file_name <- reactive(script_file(input$tt_src == "up", if (input$tt_src == "up") input$tt_file$name else tt_setup[[input$tt_example]]$file))
  copy_button("cp_tt_th", "สรุปผล", function() thai_summary_tt(tt_ok()))
  output$dl_tt_doc <- downloadHandler("rdstat_ttest_report.doc", function(file) {
    r <- tt_ok(); tb <- tt_tables()
    png <- tryCatch({ f <- tempfile(fileext = ".png"); star_ggsave(gg_tt(), f, width = 6.5, height = 4.5, dpi = 200); f }, error = function(e) NULL)
    blocks <- c(w_p(paste("Data:", tt_file_name() %||% "-", "| Date:", format(Sys.Date()), "|", app_name, version_label()), "small"),
      w_h2("สรุปผล"), w_p(thai_summary_tt(r)), w_p(r$summary),
      w_h2("Descriptive statistics"), word_table(tb$desc),
      w_h2("t-test"), word_table(tb$test),
      w_h2("Assumption checks"), word_table(tb$normal),
      if (!is.null(r$variance)) w_p(sprintf("Equality of variances, F test: F(%d, %d) = %.2f, p %s.", r$variance$DF1, r$variance$DF2, r$variance$F, .star_fmt_p(r$variance$P))),
      w_h2("Rank-based alternative"), word_table(tb$nonpar),
      w_h2("APA-style report"), w_p(r$apa),
      w_h2("Methods and references"), w_p(methods_sentence(gg_version())), paste0("<ul>", paste0("<li>", htmltools::htmlEscape(package_refs()), "</li>", collapse = ""), "</ul>"))
    word_doc(file, paste(app_name, "-", r$method_name), blocks, png)
  })
  output$dl_tt_xlsx <- downloadHandler("rdstat_ttest.xlsx", function(file) {
    r <- tt_ok()
    sheets <- list(Descriptives = r$groups, `t-test` = r$test, Wilcoxon = r$nonparametric, Normality = r$normality)
    if (!is.null(r$variance)) sheets$Variances <- r$variance
    vars <- switch(r$type, independent = sprintf("%s by %s (%s vs %s)", r$response, r$group, r$labels[1], r$labels[2]),
                   paired = paste(r$x, "-", r$y), one = sprintf("%s vs %s", r$response, format(r$mu)))
    sheets$Info <- data.frame(Item = c("App version", "Test", "Variables", "Alternative", "Alpha", "Effect size (Cohen's d)", "Summary", "Data checks"),
                              Value = c(version_label(), r$method_name, vars, r$alternative, r$alpha, round(r$effect_size, 4), r$summary,
                                        if (length(r$data_notes)) paste(r$data_notes, collapse = " | ") else "none"))
    writexl::write_xlsx(sheets, file)
  })
  copy_button("cp_tt_desc", "Descriptive statistics", function() tsv(tt_tables()$desc))
  copy_button("cp_tt_test", "t-test table", function() tsv(tt_tables()$test))
  copy_button("cp_tt_np", "Wilcoxon table", function() tsv(tt_tables()$nonpar))
  copy_button("cp_tt_apa", "APA text", function() tt_ok()$apa)
  copy_button("cp_tt_script", "R code", function() tt_script())

