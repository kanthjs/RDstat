  # ---- regression ----
  reg_dat <- use_data("reg", reg_setup)
  output$reg_data_ui <- renderUI(data_preview_ui(reg_dat()))
  output$reg_pickers <- renderUI({
    nm <- numeric_cols(reg_dat()); s <- example_of("reg", reg_setup)
    resp <- if (!is.null(s)) s$resp else tail(nm, 1)
    tagList(selectInput("reg_resp", "Response variable (y)", nm, selected = resp),
            selectizeInput("reg_pred", "Predictor(s) (x): one or more", setdiff(nm, resp), multiple = TRUE,
                           selected = if (!is.null(s)) s$pred else head(setdiff(nm, resp), 1)))
  })
  observeEvent(input$reg_resp, {   # the response cannot also be a predictor
    nm <- numeric_cols(reg_dat()); keep <- setdiff(input$reg_pred, input$reg_resp)
    updateSelectizeInput(session, "reg_pred", choices = setdiff(nm, input$reg_resp), selected = keep)
  }, ignoreInit = TRUE)
  observeEvent(input$reg_example, { s <- reg_setup[[input$reg_example]]; if (!is.null(s)) updateRadioButtons(session, "reg_degree", selected = s$degree) })
  reg_res <- reactive({
    req(input$reg_resp, input$reg_pred)
    tryCatch(star_regression(reg_dat(), input$reg_resp, input$reg_pred, degree = if (length(input$reg_pred) == 1) as.integer(input$reg_degree) else 1L, alpha = as.numeric(input$reg_alpha)), error = as_error)
  }) |> bindEvent(input$go_reg)
  reg_ok <- reactive({ r <- reg_res(); req(!inherits(r, "star_error")); r })
  reg_stale <- stale_flag("go_reg", reactive(list(input$reg_src, input$reg_example, input$reg_file$name, input$reg_resp, input$reg_pred,
                                                  input$reg_degree, input$reg_alpha)))
  output$reg_msg <- renderUI({
    if (!isTRUE(input$go_reg > 0)) return(run_hint("Run regression"))
    r <- reg_res()
    tagList(if (reg_stale()) stale_note("go_reg"), if (inherits(r, "star_error")) error_box(r$message))
  })
  reg_tables <- reactive({ r <- reg_ok(); list(fit = reg_fit_display(r), anova = reg_anova_display(r), coef = reg_coef_display(r)) })

  output$reg_results_ui <- renderUI({
    r <- reg_ok(); tb <- reg_tables()
    tagList(
      notes_ui(r$data_notes),
      div(class = if (r$significant) "alert alert-success" else "alert alert-info", r$summary),
      key_stats(
        stat("R-squared", sprintf("%.3f", r$fit$r2), sprintf("adjusted %.3f", r$fit$adj_r2)),
        stat("Model p value", fmt_p(r$p_model), sprintf("F(%d, %d) = %.2f", r$fit$df1, r$fit$df2, r$fit$f)),
        stat("RMSE", fmt_sig(r$fit$rmse), sprintf("CV = %.1f%%", r$fit$cv)),
        stat("Observations", r$fit$n)),
      result_card("Fitted model", div(class = "equation", r$equation),
        if (!is.null(r$optimum)) p(class = "text-muted small mb-0", sprintf("Stationary point of the curve: %s = %s, fitted %s = %s (a %s%s).", r$predictors, fmt_sig(r$optimum$x), r$response,
                                                                      fmt_sig(r$optimum$y), r$optimum$type, if (r$optimum$inside) "" else "; outside the range of the data"))),
      result_card("Model fit", html_table(tb$fit)),
      result_card("Analysis of variance", html_table(tb$anova), copy = "cp_reg_anova", copy_label = "Copy table"),
      result_card("Parameter estimates", html_table(tb$coef), copy = "cp_reg_coef", copy_label = "Copy table"),
      thai_card(thai_summary_reg(r), "cp_reg_th"),
      result_card("APA-style report", p(r$apa), copy = "cp_reg_apa"),
      code_card(reg_script(), "cp_reg_script"))
  })
  gg_reg <- reactive(apply_plot_customizations(do.call(star_ggplot_regression, c(list(reg_ok(), main = input$reg_title, xlab = input$reg_xlab, ylab = input$reg_ylab), plot_style("reg"))), "reg"))
  output$reg_plot <- render_live_plot("reg", gg_reg)
  output$dl_reg_plot <- plot_download("reg", gg_reg)
  output$reg_diag_ui <- renderUI({
    dg <- reg_ok()$diagnostics
    if (is.null(dg)) return(div(class = "alert alert-warning", "Assumption checks could not be computed for this data."))
    tagList(
      diag_summary_cards(dg),
      div(class = if (dg$ok) "alert alert-success" else "alert alert-warning", tags$ul(lapply(dg$messages, tags$li))),
      if (nrow(dg$outliers)) {
        o <- dg$outliers
        o[] <- lapply(o, function(v) if (is.numeric(v)) fmt_n(v, 3) else as.character(v))
        tagList(h4("Flagged observations"), p(class = "text-muted", "Row = row number in your data. |Studentized residual| > 3 = outlier, 2 to 3 = suspect."), html_table(o))
      })
  })
  output$reg_diag_plot <- renderPlot({
    dg <- reg_ok()$diagnostics; req(dg)
    print(gg_diag(dg, "reg"))
  })
  reg_predictions <- reactive({
    r <- reg_ok(); req(length(r$predictors) == 1, nzchar(trimws(input$reg_newx %||% "")))
    xs <- star_parse_number(trimws(strsplit(input$reg_newx, "[;,[:space:]]+")[[1]]))
    validate(need(length(xs) > 0 && !anyNA(xs), "Enter numbers separated by commas, for example: 40, 60, 80"))
    star_predict(r, setNames(data.frame(xs), r$predictors))
  })
  output$reg_predict_ui <- renderUI({
    r <- reg_ok()
    if (length(r$predictors) != 1) return(div(class = "alert alert-info", "Prediction is offered for a model with one predictor."))
    tagList(textInput("reg_newx", sprintf("Values of %s (separate with commas)", r$predictors), placeholder = "e.g. 40, 60, 80"),
            tableOutput("reg_pred_tbl"), p(class = "text-muted", "Predicted = fitted mean response; the interval is the confidence interval of that mean. Values outside the range of your data are extrapolations and are marked."))
  })
  output$reg_pred_tbl <- renderTable({
    z <- reg_predictions()
    data.frame(z[[1]], fmt_sig(z$Predicted), fmt_sig(z$Lower), fmt_sig(z$Upper), ifelse(z$Extrapolated, "yes", ""), check.names = FALSE, stringsAsFactors = FALSE) |>
      setNames(c(names(z)[1], "Predicted", "Lower CI", "Upper CI", "Extrapolated"))
  }, align = "r")
  output$dl_reg_xlsx <- downloadHandler("rdstat_regression.xlsx", function(file) {
    r <- reg_ok(); dg <- r$diagnostics
    sheets <- list(Model = data.frame(Item = c("App version", "Response", "Predictor(s)", "Shape", "Equation", "Summary", "Data checks", names(unlist(r$fit))),
                                      Value = c(version_label(), r$response, paste(r$predictors, collapse = ", "), c("line", "quadratic", "cubic")[r$degree], r$equation, r$summary,
                                                if (length(r$data_notes)) paste(r$data_notes, collapse = " | ") else "none", unlist(r$fit))),
                   ANOVA = r$anova, Coefficients = r$coefficients)
    if (!is.null(dg)) sheets$Residuals <- dg$residuals
    writexl::write_xlsx(sheets, file)
  })
  copy_button("cp_reg_anova", "ANOVA table", function() tsv(reg_tables()$anova))
  copy_button("cp_reg_coef", "Parameter estimates", function() tsv(reg_tables()$coef))
  copy_button("cp_reg_apa", "APA text", function() reg_ok()$apa)
  copy_button("cp_reg_th", "สรุปผล", function() thai_summary_reg(reg_ok()))
  reg_file_name <- reactive(script_file(input$reg_src == "up", if (input$reg_src == "up") input$reg_file$name else reg_setup[[input$reg_example]]$file))
  reg_script <- reactive(script_reg(reg_ok(), reg_file_name()))
  copy_button("cp_reg_script", "R code", function() reg_script())

