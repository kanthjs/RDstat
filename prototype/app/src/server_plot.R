  # ---- plot style shared by every plot ----
  for (k in names(style_defaults)) for (s in style_screens) local({
    k <- k; s <- s; id <- paste0(s, "_", k)
    observeEvent(input[[id]], for (o in setdiff(style_screens, s)) {
      oid <- paste0(o, "_", k)
      if (!identical(input[[oid]], input[[id]])) updateSelectInput(session, oid, selected = input[[id]])
    }, ignoreInit = TRUE)
  })
  plot_style <- function(p) {
    get <- function(k) input[[paste0(p, "_", k)]] %||% style_defaults[[k]]
    list(palette = get("pal"), theme = get("theme"), base_size = as.numeric(get("size")), legend = get("legend"))
  }
  # width and height in inches of a downloaded plot
  export_dims <- function(p) {
    d <- input[[paste0(p, "_dim")]] %||% "std"
    cm <- suppressWarnings(as.numeric(if (d == "custom") c(input[[paste0(p, "_w")]], input[[paste0(p, "_h")]]) else export_sizes[[d]]))
    if (length(cm) != 2 || anyNA(cm)) cm <- export_sizes$std
    pmin(pmax(cm, 3), 60) / 2.54
  }
  # Proportional width and height in pixels for the on-screen Live View (WYSIWYG)
  live_plot_dims <- function(p) {
    wh <- export_dims(p) # in inches
    ratio <- wh[1] / wh[2]
    d <- input[[paste0(p, "_dim")]] %||% "std"
    base_w <- if (d == "j1") 480 else if (d == "slide") 700 else 640
    h <- round(base_w / ratio)
    list(width = base_w, height = max(h, 240))
  }
  live_badge_render <- function(p) {
    renderUI({
      d <- input[[paste0(p, "_dim")]] %||% "std"
      wh_cm <- export_dims(p) * 2.54
      ratio <- round(wh_cm[1] / wh_cm[2], 2)
      lbl <- switch(d,
        std = sprintf("Standard 20 × 14 ซม. (%s:1)", ratio),
        j1 = sprintf("Journal 1 คอลัมน์ 8.5 × 6.5 ซม. (%s:1)", ratio),
        j2 = sprintf("Journal 2 คอลัมน์ 17.5 × 12 ซม. (%s:1)", ratio),
        slide = sprintf("Slide 16:9 25 × 14 ซม. (%s:1)", ratio),
        custom = sprintf("กำหนดเอง %.1f × %.1f ซม. (%s:1)", wh_cm[1], wh_cm[2], ratio)
      )
      tags$span(class = "badge bg-light text-success border border-success-subtle px-2 py-1",
        title = "ขนาดและสัดส่วนที่จะได้เมื่อบันทึกภาพ",
        tags$i(class = "fa fa-ruler-combined me-1"), lbl
      )
    })
  }
  output$aov_live_badge <- live_badge_render("aov")
  output$tt_live_badge <- live_badge_render("tt")
  output$corr_live_badge <- live_badge_render("corr")
  output$reg_live_badge <- live_badge_render("reg")
  apply_plot_customizations <- function(p_obj, label_p, style_p = label_p) {
    if (!inherits(p_obj, "ggplot")) return(p_obj)
    show_ttl <- input[[paste0(label_p, "_show_title")]]
    raw_ttl <- input[[paste0(label_p, "_title")]]
    if (identical(show_ttl, FALSE) || (!is.null(raw_ttl) && tolower(trimws(raw_ttl)) == "none")) {
      p_obj <- p_obj + ggplot2::labs(title = NULL)
    }
    show_sub <- input[[paste0(label_p, "_show_sub")]]
    if (identical(show_sub, FALSE)) {
      p_obj <- p_obj + ggplot2::labs(subtitle = NULL)
    }
    ang <- input[[paste0(style_p, "_xangle")]] %||% "auto"
    if (ang %in% c("0", "45", "90")) {
      deg <- as.numeric(ang)
      p_obj <- p_obj + ggplot2::theme(axis.text.x = ggplot2::element_text(
        angle = deg,
        hjust = if (deg == 0) 0.5 else 1,
        vjust = if (deg == 90) 0.5 else 1
      ))
    }
    p_obj
  }
  gg_version <- function() tryCatch(as.character(utils::packageVersion("ggplot2")), error = function(e) "")
  output$methods_text <- renderUI({
    sentence <- methods_sentence(gg_version())
    tagList(
      div(class = "well well-sm", sentence), actionButton("cp_methods", "คัดลอกประโยค"),
      h4("เอกสารอ้างอิงของซอฟต์แวร์"), tags$ul(lapply(package_refs(), tags$li)), actionButton("cp_refs", "คัดลอกเอกสารอ้างอิง"),
      h4("ผลแต่ละส่วนคำนวณจากอะไร"),
      html_table(data.frame(`ผลลัพธ์` = c("ตาราง ANOVA (CRD, RCBD, split plot)", "อักษรเปรียบเทียบค่าเฉลี่ย", "การทดสอบที (t-test)", "การวิเคราะห์ร่วมหลายสถานที่ และ Bartlett's test", "การทดสอบแบบไม่อิงพารามิเตอร์", "SE(m), SED, LSD", "สหสัมพันธ์และค่า p", "การถดถอย ตาราง ANOVA และสัมประสิทธิ์", "การตรวจสอบการแจกแจงปกติ ความแปรปรวน และค่าผิดปกติ", "กราฟ"),
                            `คำนวณโดย` = c("R stats::aov", "agricolae (LSD.test, HSD.test, duncan.test)", "R stats::t.test, stats::var.test, stats::shapiro.test", "R stats::aov, Bartlett's test computed from the error mean squares", "agricolae (kruskal, friedman), R stats::wilcox.test", "error mean square and df from the ANOVA, stats::qt", "R stats::cor.test", "R stats::lm", "R stats::shapiro.test, nortest::lillie.test, lmtest::bptest, studentized residuals from stats", "ggplot2 and R graphics"), check.names = FALSE, stringsAsFactors = FALSE)),
      p(class = "text-muted", sprintf("รุ่นที่ใช้ในการใช้งานครั้งนี้: %s, ggplot2 %s",R.version.string, gg_version())))
  })
  observeEvent(input$cp_refs, session$sendCustomMessage("copy_text", list(what = "References", text = paste(package_refs(), collapse = "\n\n"))))
observeEvent(input$cp_mrefs, session$sendCustomMessage("copy_text", list(what = "Method references", text = paste(method_refs, collapse = "

"))))
  observeEvent(input$cp_methods, session$sendCustomMessage("copy_text", list(what = "Methods sentence", text = methods_sentence(gg_version()))))
