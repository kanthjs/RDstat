  # ---- correlation ----
  corr_dat <- use_data("corr", corr_setup)
  output$corr_data_ui <- renderUI(data_preview_ui(corr_dat()))
  output$corr_pickers <- renderUI({
    nm <- numeric_cols(corr_dat()); s <- example_of("corr", corr_setup)
    selectizeInput("corr_vars", "Variables (two or more)", nm, multiple = TRUE, selected = if (!is.null(s)) intersect(s$vars, nm) else head(nm, 6))
  })
  corr_res <- reactive({
    req(input$corr_vars)
    tryCatch(star_correlation(corr_dat(), input$corr_vars, input$corr_method, input$corr_alt, as.numeric(input$corr_alpha), input$corr_adjust), error = as_error)
  }) |> bindEvent(input$go_corr)
  corr_ok <- reactive({ r <- corr_res(); req(!inherits(r, "star_error")); r })
  corr_stale <- stale_flag("go_corr", reactive(list(input$corr_src, input$corr_example, input$corr_file$name, input$corr_vars, input$corr_method,
                                                    input$corr_alt, input$corr_adjust, input$corr_alpha)))
  output$corr_msg <- renderUI({
    if (!isTRUE(input$go_corr > 0)) return(run_hint("Run correlation"))
    r <- corr_res()
    tagList(if (corr_stale()) stale_note("go_corr"), if (inherits(r, "star_error")) error_box(r$message))
  })
  corr_tables <- reactive({ r <- corr_ok(); list(matrix = corr_matrix_display(r), pairs = corr_pairs_display(r)) })

  output$corr_results_ui <- renderUI({
    r <- corr_ok(); tb <- corr_tables()
    best <- r$pairs[which.max(abs(r$pairs$r)), ]
    tagList(
      notes_ui(r$data_notes),
      div(class = if (any(r$pairs$Significant)) "alert alert-success" else "alert alert-info", r$summary),
      key_stats(
        stat("Significant pairs", sprintf("%d of %d", sum(r$pairs$Significant), nrow(r$pairs)), paste("alpha =", format(r$alpha))),
        stat("Strongest pair", sprintf("%s = %.3f", r$symbol, best$r), paste(best$Variable1, "and", best$Variable2)),
        stat("Method", r$method_name, if (r$adjust != "none") paste(r$adjust, "adjusted p values")),
        stat("Observations per pair", if (min(r$pairs$N) == max(r$pairs$N)) min(r$pairs$N) else paste(min(r$pairs$N), "to", max(r$pairs$N)))),
      result_card("Correlation matrix", html_table(tb$matrix),
        p(class = "text-muted small mb-0", sprintf("%s coefficient. * p < %s, ** p < 0.01, *** p < 0.001%s.", r$method_name, format(r$alpha),
                                                  if (r$adjust != "none") paste0(" (", r$adjust, "-adjusted)") else "")),
        copy = "cp_corr_matrix", copy_label = "Copy table"),
      result_card("All pairs", html_table(tb$pairs), copy = "cp_corr_pairs", copy_label = "Copy table"),
      thai_card(thai_summary_corr(r), "cp_corr_th"),
      result_card("APA-style report", lapply(r$apa, p), copy = "cp_corr_apa"),
      code_card(corr_script(), "cp_corr_script"))
  })
  output$corr_view_picker <- renderUI({
    v <- corr_ok()$vars
    tagList(selectInput("corr_view", "Plot", c("Scatter plot matrix (all pairs)" = "matrix", "Heatmap of all pairs" = "heat", "Scatter plot of two variables" = "scatter")),
            conditionalPanel("input.corr_view == 'scatter'", fluidRow(
              column(6, selectInput("corr_x", "X axis", v, selected = v[1])),
              column(6, selectInput("corr_y", "Y axis", v, selected = v[2])))))
  })
  gg_corr <- reactive({
    r <- corr_ok(); v <- input$corr_view %||% "matrix"
    x <- input$corr_x; y <- input$corr_y
    pair <- if (v == "scatter" && !is.null(x) && !is.null(y) && x %in% r$vars && y %in% r$vars) {
      validate(need(x != y, "Choose two different variables for the scatter plot."))
      c(x, y)
    }
    apply_plot_customizations(do.call(star_ggplot_correlation, c(list(r, pair, main = input$corr_title, xlab = input$corr_xlab, ylab = input$corr_ylab,
                                            type = if (v == "heat") "heatmap" else "matrix", heat = input$corr_heat), plot_style("corr"))), "corr")
  })
  output$corr_plot <- render_live_plot("corr", gg_corr)
  output$dl_corr_plot <- plot_download("corr", gg_corr)
  output$dl_corr_xlsx <- downloadHandler("rdstat_correlation.xlsx", function(file) {
    r <- corr_ok(); tb <- corr_tables()
    mat <- function(m) data.frame(Variable = r$vars, m, check.names = FALSE, row.names = NULL)
    writexl::write_xlsx(list(`Correlation (r)` = mat(r$r), `P values` = mat(r$p), N = mat(r$n), Pairs = r$pairs,
                              Info = data.frame(Item = c("App version", "Method", "Alternative", "Alpha", "P adjustment", "Summary", "Data checks"),
                                                Value = c(version_label(), r$method_name, r$alternative, r$alpha, r$adjust, r$summary,
                                                          if (length(r$data_notes)) paste(r$data_notes, collapse = " | ") else "none"))), file)
  })
  copy_button("cp_corr_matrix", "Correlation matrix", function() tsv(corr_tables()$matrix))
  copy_button("cp_corr_pairs", "Pairs table", function() tsv(corr_tables()$pairs))
  copy_button("cp_corr_apa", "APA text", function() paste(corr_ok()$apa, collapse = " "))
  copy_button("cp_corr_th", "สรุปผล", function() thai_summary_corr(corr_ok()))
  corr_file_name <- reactive(script_file(input$corr_src == "up", if (input$corr_src == "up") input$corr_file$name else corr_setup[[input$corr_example]]$file))
  corr_script <- reactive(script_corr(corr_ok(), corr_file_name()))
  copy_button("cp_corr_script", "R code", function() corr_script())

