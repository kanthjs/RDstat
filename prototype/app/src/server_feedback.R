  # ---- user feedback and diagnostic reporting ----
  diagnostic_info <- reactive({
    nav_tab <- input$nav %||% "Home"
    v_info <- version_label()
    r_ver <- paste(R.version$major, R.version$minor, sep = ".")
    os_info <- paste(Sys.info()[c("sysname", "release")], collapse = " ")
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    screen_details <- switch(nav_tab,
      "Analysis of variance" = {
        err_msg <- if (isTRUE(input$go_aov > 0)) {
          tryCatch({
            r <- res()
            if (inherits(r, "star_error")) r$message else "None"
          }, error = function(e) conditionMessage(e))
        } else "None (analysis not yet run)"
        paste0(
          "- Screen: ANOVA\n",
          "- Design: ", input$design %||% "-", "\n",
          "- Data source: ", input$src %||% "-", (if (identical(input$src, "ex")) paste0(" (", input$example, ")") else if (!is.null(input$file$name)) paste0(" (", input$file$name, ")") else ""), "\n",
          "- Response: ", input$resp %||% "-", "\n",
          "- Factors: ", paste(input$factors %||% input$trt %||% "-", collapse = ", "), "\n",
          "- Post-hoc: ", input$posthoc %||% input$pw %||% "-", " (alpha = ", input$alpha %||% "0.05", ")\n",
          "- Error message: ", err_msg
        )
      },
      "t-test" = {
        err_msg <- if (isTRUE(input$go_tt > 0)) {
          tryCatch({
            r <- tt_res()
            if (inherits(r, "star_error")) r$message else "None"
          }, error = function(e) conditionMessage(e))
        } else "None (analysis not yet run)"
        paste0(
          "- Screen: t-test\n",
          "- Type: ", input$tt_type %||% "-", "\n",
          "- Variable 1: ", input$tt_v1 %||% "-", "\n",
          "- Variable 2 / Group: ", input$tt_v2 %||% "-", "\n",
          "- Alternative: ", input$tt_alt %||% "-", " (alpha = ", input$tt_alpha %||% "0.05", ")\n",
          "- Error message: ", err_msg
        )
      },
      "Correlation" = {
        err_msg <- if (isTRUE(input$go_corr > 0)) {
          tryCatch({
            r <- corr_res()
            if (inherits(r, "star_error")) r$message else "None"
          }, error = function(e) conditionMessage(e))
        } else "None (analysis not yet run)"
        paste0(
          "- Screen: Correlation\n",
          "- Variables: ", paste(input$corr_vars %||% "-", collapse = ", "), "\n",
          "- Method: ", input$corr_method %||% "-", "\n",
          "- Error message: ", err_msg
        )
      },
      "Regression" = {
        err_msg <- if (isTRUE(input$go_reg > 0)) {
          tryCatch({
            r <- reg_res()
            if (inherits(r, "star_error")) r$message else "None"
          }, error = function(e) conditionMessage(e))
        } else "None (analysis not yet run)"
        paste0(
          "- Screen: Regression\n",
          "- Response: ", input$reg_resp %||% "-", "\n",
          "- Predictor(s): ", paste(input$reg_pred %||% "-", collapse = ", "), "\n",
          "- Degree: ", input$reg_degree %||% "1", "\n",
          "- Error message: ", err_msg
        )
      },
      paste0("- Active tab: ", nav_tab)
    )

    paste(
      "=== RDstat Diagnostic Report ===",
      paste0("App Version: ", v_info),
      paste0("R Version: ", r_ver),
      paste0("System: ", os_info),
      paste0("Timestamp: ", ts),
      "--- Parameters & Status ---",
      screen_details,
      "================================",
      sep = "\n"
    )
  })

  show_feedback_modal <- function() {
    diag <- diagnostic_info()
    showModal(modalDialog(
      title = tagList(icon("comment-dots", class = "text-success me-2"), "ส่งข้อเสนอแนะและแจ้งปัญหา RDstat"),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton("cp_diag_modal", tagList(icon("copy"), " คัดลอกข้อมูลวินิจฉัย"), class = "btn btn-outline-secondary"),
        modalButton("ปิดหน้าต่าง")
      ),
      div(
        p("ทีมพัฒนายินดีรับฟังความคิดเห็น ปัญหาการใช้งาน หรือข้อเสนอแนะฟีเจอร์เพิ่มเติม เพื่อพัฒนา RDstat ให้ตอบโจทย์นักวิจัยข้าวมากที่สุด"),
        div(class = "d-grid gap-2 mb-4",
          tags$a(
            href = feedback_form_url,
            target = "_blank",
            rel = "noopener noreferrer",
            class = "btn btn-success btn-lg fw-bold shadow-sm d-flex align-items-center justify-content-center gap-2",
            icon("arrow-up-right-from-square"),
            " เปิดแบบฟอร์ม Google Form เพื่อส่งข้อเสนอแนะ"
          )
        ),
        tags$hr(),
        tags$h6(class = "fw-bold text-secondary mb-2",
          icon("stethoscope", class = "me-1"), " ข้อมูลวินิจฉัยระบบ (System Diagnostics)"
        ),
        p(class = "small text-muted mb-2",
          "หากท่านพบปัญหาหรือข้อผิดพลาด สามารถกดปุ่ม ", strong("คัดลอกข้อมูลวินิจฉัย"),
          " ด้านล่าง แล้วนำไปวาง (Paste) ลงในแบบฟอร์ม เพื่อช่วยให้ทีมงานตรวจสอบสาเหตุได้รวดเร็วขึ้น:"
        ),
        tags$textarea(
          id = "diag_text_box",
          class = "form-control font-monospace small bg-light mb-2",
          rows = 7,
          readonly = "readonly",
          style = "font-size: 12px; resize: vertical;",
          diag
        ),
        p(class = "small text-muted",
          "หมายเหตุ: ข้อมูลนี้มีเพียงชื่อคอลัมน์ ค่าพารามิเตอร์ และข้อความเตือนของระบบ ",
          strong("ไม่มีการส่งเนื้อหาข้อมูลดิบของท่านออกไปภายนอก")
        )
      )
    ))
  }

  observeEvent(input$open_feedback, show_feedback_modal())
  observeEvent(input$btn_about_feedback, show_feedback_modal())
  observeEvent(input$cp_diag_modal, {
    session$sendCustomMessage("copy_text", list(what = "ข้อมูลวินิจฉัย", text = diagnostic_info()))
  })
  observeEvent(input$cp_diagnostic, {
    session$sendCustomMessage("copy_text", list(what = "ข้อมูลวินิจฉัย", text = diagnostic_info()))
  })
