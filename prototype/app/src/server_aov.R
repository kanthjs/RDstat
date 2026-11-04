  # TRUE when the controls (as summarised by sig()) differ from what they were when the button was last pressed
  stale_flag <- function(button, sig) {
    ran <- reactiveVal(NULL)
    observeEvent(input[[button]], ran(sig()))
    stale <- reactive(!is.null(ran()) && !identical(ran(), sig()))
    # grey out and lock that screen's results (see stale_js); its outputs are inside <screen>_out
    observe(session$sendCustomMessage("stale", list(id = paste0(sub("^go_", "", button), "_out"), on = stale())))
    stale
  }
  # explicit cache: the default application cache is not set up when the app runs in the browser (webR)
  star_cache <- cachem::cache_mem(max_size = 100 * 1024^2)
  output$example_picker <- renderUI(selectInput("example", "Example", examples[[input$design]]))
  setup <- reactive(if (input$src == "ex" && !is.null(input$example) && input$example %in% names(example_setup)) example_setup[[input$example]])

  output$template_label <- renderText({
    switch(input$design %||% "CRD",
           CRD = "CRD",
           RCBD = "RCBD",
           SplitCRD = "Split-plot CRD",
           SplitRCBD = "Split-plot RCBD",
           CombRCBD = "RCBD over sites",
           CombCRD = "CRD over sites",
           "CRD")
  })
  output$dl_template <- downloadHandler(
    filename = function() sprintf("rdstat_template_%s.xlsx", tolower(input$design %||% "crd")),
    content = function(file) {
      des <- input$design %||% "CRD"
      tbl <- switch(des,
        CRD = data.frame(
          Treatment = rep(c("Variety_A", "Variety_B", "Variety_C", "Variety_D"), each = 4),
          Replication = rep(1:4, times = 4),
          Yield = rep(NA_real_, 16),
          stringsAsFactors = FALSE
        ),
        RCBD = data.frame(
          Block = rep(1:4, times = 4),
          Treatment = rep(c("Variety_A", "Variety_B", "Variety_C", "Variety_D"), each = 4),
          Yield = rep(NA_real_, 16),
          stringsAsFactors = FALSE
        ),
        SplitCRD = data.frame(
          MainPlot = rep(c("Main_A", "Main_B"), each = 12),
          Replication = rep(rep(1:4, each = 3), times = 2),
          SubPlot = rep(c("Sub_1", "Sub_2", "Sub_3"), times = 8),
          Yield = rep(NA_real_, 24),
          stringsAsFactors = FALSE
        ),
        SplitRCBD = data.frame(
          Block = rep(1:4, each = 6),
          MainPlot = rep(rep(c("Main_A", "Main_B"), each = 3), times = 4),
          SubPlot = rep(c("Sub_1", "Sub_2", "Sub_3"), times = 8),
          Yield = rep(NA_real_, 24),
          stringsAsFactors = FALSE
        ),
        CombRCBD = data.frame(
          Site = rep(c("Site_1", "Site_2", "Site_3"), each = 16),
          Block = rep(rep(1:4, each = 4), times = 3),
          Treatment = rep(c("Variety_A", "Variety_B", "Variety_C", "Variety_D"), times = 12),
          Yield = rep(NA_real_, 48),
          stringsAsFactors = FALSE
        ),
        CombCRD = data.frame(
          Site = rep(c("Site_1", "Site_2", "Site_3"), each = 16),
          Treatment = rep(rep(c("Variety_A", "Variety_B", "Variety_C", "Variety_D"), each = 4), times = 3),
          Replication = rep(1:4, times = 12),
          Yield = rep(NA_real_, 48),
          stringsAsFactors = FALSE
        )
      )
      writexl::write_xlsx(list(Template = tbl), file)
    }
  )

  raw_dat <- reactive({
    if (input$src == "ex") { s <- setup(); req(s); return(read.csv(s$file)) }
    req(input$file)
    # every column is read as text so that the engine can check and convert it (decimal commas, units, "n/a", ...)
    tryCatch(star_read_table(input$file$datapath, input$file$name),
             error = function(e) { showNotification(paste("Could not read the file:", conditionMessage(e)), type = "error"); req(FALSE) })
  })
  # a wide-format upload (one column per replication) is analysed only after the user agrees to convert it to long format
  to_long <- reactiveVal(FALSE)
  observeEvent(input$file, to_long(FALSE))
  observeEvent(input$wide_convert, to_long(TRUE))
  observeEvent(input$wide_undo, to_long(FALSE))
  wide_cols <- reactive(if (input$src == "up") wide_rep_cols(raw_dat()) else character())
  dat <- reactive(if (to_long() && length(wide_cols())) wide_to_long(raw_dat(), wide_cols()) else raw_dat())
  output$wide_note <- renderUI({
    cols <- wide_cols(); if (!length(cols)) return(NULL)
    if (to_long()) return(div(class = "alert alert-success small", icon("circle-check"), " ",
      sprintf("Converted to long format: %d rows, with the replication in column Rep and the values in column Value (see the Data tab).", nrow(dat())),
      actionLink("wide_undo", "Undo")))
    div(class = "alert alert-warning small", icon("triangle-exclamation"), " ", strong("This file looks like wide format: "),
        sprintf("the columns %s each hold one replication. The analysis needs one row per plot (long format).", paste(cols, collapse = ", ")),
        div(class = "mt-2", actionButton("wide_convert", "Convert to long format", class = "btn-warning btn-sm")))
  })
  output$read_note <- renderUI(if (input$src == "up") read_note_ui(raw_dat()))
  output$data_ui <- renderUI(data_preview_ui(dat()))
  output$var_pickers <- renderUI({
    d <- dat(); nm <- names(d); s <- setup()
    num <- numeric_cols(d)
    resp_choices <- if (length(num)) num else nm
    resp_sel <- if (!is.null(s) && s$resp %in% resp_choices) s$resp else tail(resp_choices, 1)
    # an example brings its own choices; for an uploaded file they are guessed from the column names
    g <- if (is.null(s)) guess_roles(d, resp_sel) else list(blk = s$blk, env = s$env, trt = s$trt)
    tagList(
      selectInput("resp", "Response variable (numeric)", resp_choices, selected = resp_sel),
      if (is_split(input$design)) tagList(
        selectInput("trt_main", "Main-plot factor", nm, selected = g$trt[1]),
        selectInput("trt_sub", "Sub-plot factor", nm, selected = if (length(g$trt) > 1) g$trt[2] else nm[min(3, length(nm))]))
      else if (is_comb(input$design)) tagList(
        selectInput("trt", "Treatment factor (one)", nm, selected = g$trt[1]),
        selectInput("env", "Environment: site, season or year", nm, selected = g$env))
      else selectizeInput("trt", "Treatment factor(s): one or two", nm, multiple = TRUE,
                     selected = if (is.null(s)) g$trt[1] else s$trt, options = list(maxItems = 2)),
      if (input$design == "CombRCBD") tagList(selectInput("blk", "Block / replication within each environment", nm, selected = g$blk),
                                              helpText("Blocks are numbered within each environment (block 1 at one site is not block 1 at another); the analysis nests them in the environment.")),
      if (input$design == "SplitCRD") tagList(selectInput("blk", "Replication: main-plot number within each main-plot level", nm, selected = g$blk),
                                              helpText("1, 2, 3, ... for the main plots of each level of the main-plot factor. Not a block: in CRD the main plots are not grouped.")),
      if (input$design == "RCBD") selectInput("blk", "Block / replication", nm, selected = g$blk),
      if (input$design == "SplitRCBD") tagList(selectInput("blk", "Block: group of main plots holding every main-plot level", nm, selected = g$blk),
                                               helpText("The block (replication) each main plot is in, not a number of the main plot itself.")),
      if (is_split(input$design)) tagList(uiOutput("split_check"),
        help_box("Block or main-plot number?",
          p(strong("Block (Split plot in RCBD):"), " the field was divided into blocks and each block holds one main plot of every main-plot level. The block column repeats 1, 2, 3 ... across the main-plot levels."),
          p(strong("Main-plot number (Split plot in CRD):"), " there are no blocks; the main plots of each level are simply numbered 1, 2, 3 ..."),
          p("The two give different degrees of freedom for Error(a), which tests the main-plot factor, so choose the design that matches how the field was laid out. The check above shows the df that your choice gives.")))
    )
  })
  observeEvent(input$resp, {
    req(input$resp, dat())
    if (!is_split(input$design %||% "CRD")) {
      cur_trt <- isolate(input$trt)
      if (input$resp %in% cur_trt) {
        updateSelectizeInput(session, "trt", selected = setdiff(cur_trt, input$resp))
      }
    }
  }, ignoreInit = TRUE)
  output$split_check <- renderUI({
    req(is_split(input$design), input$trt_main, input$trt_sub, input$blk)
    d <- dat(); req(all(c(input$trt_main, input$trt_sub, input$blk) %in% names(d)))
    split_layout_check(d, input$design, input$trt_main, input$trt_sub, input$blk)
  })

  # Runs on "Run ANOVA" only. Results are cached by data and settings, so going back to an earlier choice
  # (e.g. LSD -> Tukey -> LSD) reuses the earlier result instead of analysing again.
  res <- reactive({
    req(input$resp)
    blk <- if (needs_block(input$design)) { req(input$blk); input$blk } else NULL
    if (is_split(input$design)) req(input$trt_main, input$trt_sub) else req(input$trt)
    if (is_comb(input$design)) req(input$env)
    tr <- input$transform %||% "none"
    tryCatch({
      td <- transform_response(dat(), input$resp, tr)
      r <- if (is_split(input$design)) star_splitplot(td$data, td$resp, input$trt_main, input$trt_sub, blk, test = input$pw,
                                                      alpha = as.numeric(input$alpha), design = input$design)
           else if (is_comb(input$design)) star_combined(td$data, td$resp, input$trt, input$env, blk, design = sub("^Comb", "", input$design),
                                                         test = input$pw, alpha = as.numeric(input$alpha))
           else star_aov(td$data, td$resp, input$trt, blk, design = input$design, test = input$pw,
                         alpha = as.numeric(input$alpha))
      if (tr != "none") { r$transform <- tr; r$original_response <- input$resp }
      r
    }, error = function(e) structure(list(message = conditionMessage(e)), class = "star_error"))
  }) |>
    bindCache(dat(), input$design, input$resp, input$transform, input$trt, input$env, input$trt_main, input$trt_sub, input$blk, input$pw, input$alpha, cache = star_cache) |>
    bindEvent(input$go_aov)
  ok <- reactive({ r <- res(); req(!inherits(r, "star_error")); r })
  # the untransformed data at the time Run was pressed: ranks are taken of the original values, so that the medians are
  # in the original unit; priority so that they are stored before the results are drawn
  run_data <- reactiveVal(NULL)
  observeEvent(input$go_aov, run_data(tryCatch(dat(), error = function(e) NULL)), priority = 10)
  # the method shown. It follows the Analysis buttons (or a recommendation's switch) at once, with no new run, because
  # every method is computed from the same data; only CRD and RCBD have the alternatives
  aov_mode <- reactive({
    r <- ok(); m <- input$aov_method %||% "anova"
    if (m == "rank" && r$design %in% c("CRD", "RCBD")) "rank"
    else if (m %in% c("welch", "trimmed") && identical(r$design, "CRD") && length(r$factors) == 1) m
    else "anova"
  })
  # the analyses that exist for the design and the number of treatment factors chosen; the others are greyed out
  # (disabled) in the Analysis buttons, and a choice that stops being available falls back to the standard ANOVA
  aov_support <- reactive({
    d <- input$design %||% "CRD"; one <- length(input$trt %||% 1) == 1
    can_rank <- d %in% c("CRD", "RCBD") && one; can_welch <- d == "CRD" && one
    why <- if (!d %in% c("CRD", "RCBD")) "Only the standard ANOVA is available for split-plot and combined designs."
           else if (!one) "This analysis needs exactly one treatment factor; with two factors only the standard ANOVA is available."
           else if (!can_welch) "Not available for blocks (RCBD): there is no Welch or trimmed-means version with blocks."
    list(ok = c("anova", if (can_rank) "rank", if (can_welch) c("welch", "trimmed")), why = why)
  })
  observe({
    s <- aov_support()
    session$sendCustomMessage("radio_support", list(id = "aov_method", ok = as.list(s$ok), why = s$why %||% ""))
    if (!(isolate(input$aov_method) %||% "anova") %in% s$ok) updateRadioButtons(session, "aov_method", selected = "anova")
  })
  output$aov_method_note <- renderUI({ s <- aov_support(); if (!is.null(s$why)) p(class = "text-muted small", icon("circle-info"), " ", s$why, " Greyed-out analyses do not apply.") })
  # Welch's ANOVA chosen where it does not apply: the standard ANOVA is shown, and the screen says why
  welch_fallback <- reactive((input$aov_method %||% "") %in% c("welch", "trimmed") && aov_mode() == "anova")
  aov_trimmed <- reactive(aov_mode() == "trimmed")
  aov_rank <- reactive(aov_mode() == "rank")
  aov_welch <- reactive(aov_mode() == "welch")
  # rank-based test for one treatment factor in a CRD (Kruskal-Wallis) or an RCBD (Friedman); NULL for other designs,
  # a star_error when it cannot be run (two factors, or a block without every treatment exactly once)
  np_try <- reactive({
    r <- ok()
    if (!r$design %in% c("CRD", "RCBD")) return(NULL)
    d <- run_data(); req(d)
    tryCatch(star_nonparametric(d, r$original_response %||% r$response, r$factors, r$block, r$alpha,
                                posthoc = if (is.null(r$block)) input$np_posthoc %||% "conover" else "conover"), error = as_error)
  })
  np_res <- reactive({ np <- np_try(); if (inherits(np, "star_error")) NULL else np })
  # Welch's ANOVA, on the analysed (possibly transformed) values like the standard ANOVA; CRD only
  welch_try <- reactive({
    r <- ok(); d <- used_data(); req(d)
    if (!identical(r$design, "CRD")) return(structure(list(message = "Welch's ANOVA has no version for blocks (RCBD). A transformation (Advanced options) or the Friedman test (rank-based) can be used instead."), class = "star_error"))
    tryCatch(star_welch(d, r$response, r$factors, r$alpha), error = as_error)
  })
  # which method suits the data (advice only; see star_recommend_aov), and whether the user chose to keep their method
  aov_rec <- reactive(tryCatch(star_recommend_aov(ok()), error = function(e) NULL))
  rec_kept <- reactiveVal(FALSE)
  observeEvent(input$go_aov, rec_kept(FALSE))
  observeEvent(input$rec_stay, rec_kept(TRUE))
  for (id in c("rank", "welch", "trimmed")) local({ m <- id; observeEvent(input[[paste0("rec_", m)]], updateRadioButtons(session, "aov_method", selected = m)) })
  method_names <- c(anova = "the standard ANOVA", welch = "Welch's ANOVA", rank = "the rank-based test", trimmed = "the trimmed-means comparison")
  # trimmed means (20%) with bootstrap intervals, on the analysed values; CRD with one factor
  trimmed_try <- reactive({
    r <- ok(); d <- used_data(); req(d)
    tryCatch(star_trimmed(d, r$response, r$factors, r$alpha), error = as_error)
  })
  np_display <- function(np) {
    m <- np$means
    out <- data.frame(m$Treatment, fmt_n(m$Median), formatC(m$N, format = "d"), fmt_n(m$Rank), stringsAsFactors = FALSE)
    names(out) <- c(np$treatment, "Median", "N", np$rank_label)
    if (np$significant) out$Group <- m$Group
    out
  }
  np_ui <- function(np, summary = TRUE) tagList(
    if (summary) p(np$summary),
    html_table(np_display(np)),
    p(class = "text-muted small mb-0", if (np$significant) sprintf("Treatments with the same letter do not differ (comparison of %s, agricolae). ", tolower(np$rank_label)) else "",
      "The rank-based test does not assume normal residuals or equal variances."))
  welch_display <- function(w) {
    m <- w$means
    out <- data.frame(m$Treatment, fmt_n(m$Mean), fmt_n(m$SD), formatC(m$N, format = "d"), fmt_n(m$SE), stringsAsFactors = FALSE)
    names(out) <- c(w$treatment, "Mean", "SD", "N", "SE")
    if (w$significant) out$Group <- m$Group
    out
  }
  gh_display <- function(w) { z <- w$pairs
    data.frame(Comparison = z$Comparison, Difference = fmt_n(z$Difference), SE = fmt_n(z$SE), t = fmt_n(z$t, 3), DF = fmt_n(z$DF), p = fmt_p(z$P),
               check.names = FALSE, stringsAsFactors = FALSE) }
  # the results screen when Analysis is Welch's ANOVA
  welch_results_ui <- function(r) {
    w <- welch_try()
    if (inherits(w, "star_error")) return(error_box(paste("Welch's ANOVA could not be run:", w$message)))
    m <- w$means; top <- m[which.max(m$Mean), ]
    tagList(
      notes_ui(r$data_notes),
      div(class = if (w$significant) "alert alert-success" else "alert alert-info", w$summary),
      key_stats(
        stat("Test", "Welch's ANOVA", "no equal-variance assumption"),
        stat("F", sprintf("%.2f", w$statistic), sprintf("df = %d and %.2f", w$df1, w$df2)),
        stat("p value", fmt_p(w$p), if (w$significant) paste("significant at", format(w$alpha)) else paste("not significant at", format(w$alpha))),
        stat("Highest mean", fmt_n(top$Mean), top$Treatment)),
      if (!is.null(r$transform)) div(class = "alert alert-light small", icon("circle-info"), sprintf(" The analysis used %s transformed as %s; the means are on the transformed scale.", r$original_response, transforms[[r$transform]]$label)),
      thai_card(thai_summary_welch(w), "cp_welch_th"),
      result_card("Welch's ANOVA: means", html_table(welch_display(w)),
        p(class = "text-muted small mb-0", if (w$significant) "Means with the same letter do not differ (Games-Howell test, which does not assume equal variances). " else "Letters are not given because the test is not significant. ",
          "SE is the group's own standard error."), copy = "cp_welch", copy_label = "Copy table"),
      result_card("Games-Howell comparisons", html_table(gh_display(w)), copy = "cp_gh", copy_label = "Copy table"),
      result_card("APA-style report", p(w$apa), copy = "cp_welch_apa"),
      code_card(welch_script(), "cp_welch_script"),
      p(class = "text-muted small", icon("circle-info"), " The standard ANOVA table and its letters are not shown because Analysis is set to Welch's ANOVA."))
  }
  trimmed_display <- function(x) {
    m <- x$means
    out <- data.frame(m$Treatment, fmt_n(m$TrimmedMean), fmt_n(m$Lower), fmt_n(m$Upper), formatC(m$N, format = "d"), stringsAsFactors = FALSE)
    names(out) <- c(x$treatment, "Trimmed mean", sprintf("Bootstrap %g%% CI: lower", 100 * (1 - x$alpha)), "upper", "N")
    if (x$significant) out$Group <- m$Group
    out
  }
  yuen_display <- function(x) { z <- x$pairs
    data.frame(Comparison = z$Comparison, Difference = fmt_n(z$Difference), SE = fmt_n(z$SE), t = fmt_n(z$t, 3), DF = fmt_n(z$DF), p = fmt_p(z$P), `p (Holm)` = fmt_p(z$P.adj),
               check.names = FALSE, stringsAsFactors = FALSE) }
  # the results screen for the trimmed-means comparison
  trimmed_results_ui <- function(r) {
    x <- trimmed_try()
    if (inherits(x, "star_error")) return(error_box(paste("The trimmed-means comparison could not be run:", x$message)))
    m <- x$means; top <- m[which.max(m$TrimmedMean), ]
    tagList(
      notes_ui(r$data_notes),
      div(class = if (x$significant) "alert alert-success" else "alert alert-info", x$summary),
      key_stats(
        stat("Test", "Trimmed means (20%)", "no normality or equal-variance assumption"),
        stat("F", sprintf("%.2f", x$statistic), sprintf("df = %d and %.2f", x$df1, x$df2)),
        stat("p value", fmt_p(x$p), if (x$significant) paste("significant at", format(x$alpha)) else paste("not significant at", format(x$alpha))),
        stat("Highest trimmed mean", fmt_n(top$TrimmedMean), top$Treatment)),
      if (x$n_min < 10) div(class = "alert alert-light small", icon("circle-info"), sprintf(" With %d observations per treatment, 20%% trimming sets aside few or no values and the bootstrap intervals are rough.", x$n_min)),
      if (!is.null(r$transform)) div(class = "alert alert-light small", icon("circle-info"), sprintf(" The analysis used %s transformed as %s.", r$original_response, transforms[[r$transform]]$label)),
      thai_card(thai_summary_trimmed(x), "cp_trim_th"),
      result_card("Trimmed means", html_table(trimmed_display(x)),
        p(class = "text-muted small mb-0", if (x$significant) "Treatments with the same letter do not differ (Yuen's tests, Holm adjusted). " else "Letters are not given because the test is not significant. ",
          sprintf("Percentile bootstrap intervals from %d resamples (fixed seed, so the same data give the same interval).", x$B)), copy = "cp_trim", copy_label = "Copy table"),
      result_card("Yuen's comparisons", html_table(yuen_display(x)), copy = "cp_yuen", copy_label = "Copy table"),
      result_card("APA-style report", p(x$apa), copy = "cp_trim_apa"),
      code_card(trimmed_script(), "cp_trim_script"),
      p(class = "text-muted small", icon("circle-info"), " The ANOVA table and its letters are not shown because Analysis is set to trimmed means."))
  }
  # the results screen when Analysis is rank-based: the test, medians and letters take the place of the ANOVA
  rank_results_ui <- function(r) {
    np <- np_try()
    if (inherits(np, "star_error")) return(error_box(paste("The rank-based test could not be run:", np$message)))
    m <- np$means; top <- m[which.max(m$Median), ]
    tagList(
      notes_ui(r$data_notes),
      div(class = if (np$significant) "alert alert-success" else "alert alert-info", np$summary),
      if (isFALSE(r$diagnostics$homogeneity$ok)) p(class = "small text-muted", icon("circle-info"),
        " The spread differs among the treatments, so the test compares mean ranks (which treatments tend to give larger values) rather than medians alone."),
      key_stats(
        stat("Test", np$name, if (is.null(np$block)) "CRD, ranks of all plots" else paste("RCBD, ranks within each", np$block)),
        stat("Chi-square", sprintf("%.2f", np$statistic), paste("df =", np$df)),
        stat("p value", fmt_p(np$p), if (np$significant) paste("significant at", format(np$alpha)) else paste("not significant at", format(np$alpha))),
        stat("Highest median", fmt_n(top$Median), top$Treatment)),
      if (!is.null(r$transform)) div(class = "alert alert-light small", icon("circle-info"),
        " The data transformation is not used: ranks do not change under it, and the medians are in the original unit."),
      thai_card(thai_summary_np(np), "cp_np_th"),
      result_card(paste(np$name, "test"), np_ui(np, summary = FALSE), copy = "cp_np", copy_label = "Copy table"),
      result_card("APA-style report", p(np$apa), copy = "cp_np_apa"),
      code_card(np_script(), "cp_np_script"),
      p(class = "text-muted small", icon("circle-info"), " The ANOVA table, means and CV are not shown because Analysis is set to rank-based. Choose ANOVA under Experimental design to see them."))
  }
  # The data behind the current results (for plotting observations). Stored when Run is pressed: an eventReactive would
  # only read the data when first asked for, by which time the user may have switched to another example.
  used_data <- reactiveVal(NULL)
  # with a transformation the plotted observations must be on the transformed scale, like the means
  observeEvent(input$go_aov, used_data(tryCatch(transform_response(dat(), input$resp, input$transform)$data, error = function(e) dat())))
  aov_sig <- reactive(list(input$design, input$src, input$example, input$file$name, to_long(), input$resp, input$transform, input$trt, input$env, input$trt_main, input$trt_sub,
                           input$blk, input$pw, input$alpha))
  aov_stale <- stale_flag("go_aov", aov_sig)
  output$aov_msg <- renderUI({
    if (!isTRUE(input$go_aov > 0)) return(run_hint("Run analysis"))
    r <- res()
    tagList(if (aov_stale()) stale_note("go_aov"), if (inherits(r, "star_error")) error_box(r$message))
  })

  # tables prepared once so that the screen, the clipboard and the Excel file agree
  tables <- reactive({
    r <- ok(); fs <- r$factors
    out <- list(anova = anova_display(r$anova), means = list(), letters = list(), cells = NULL, simple = list(), agronomy = list())
    for (f in fs) {
      show <- isTRUE(r$effects$Significant[r$effects$Effect == f]) && !r$interaction_significant
      out$letters[[f]] <- show
      m_df <- means_display(r$means[[f]], f, show, back = if (!is.null(r$transform)) transforms[[r$transform]]$back)
      out$means[[f]] <- m_df
      out$agronomy[[f]] <- agronomy_display(r, f, m_df)
    }
    if (!is.null(r$cells)) out$cells <- cells_display(r$cells, fs[1], fs[2])
    for (nm in names(r$simple)) out$simple[[nm]] <- simple_display(r$simple[[nm]])
    out
  })

  output$results_ui <- renderUI({
    r <- ok()
    advice <- advice_ui(aov_rec(), aov_mode(), "rec", rec_kept(), method_names[[aov_mode()]])
    if (aov_rank()) return(tagList(advice, rank_results_ui(r)))
    if (aov_welch()) return(tagList(advice, welch_results_ui(r)))
    if (aov_trimmed()) return(tagList(advice, trimmed_results_ui(r)))
    tb <- tables()
    tagList(
      advice,
      if (welch_fallback()) div(class = "alert alert-light small py-2", icon("circle-info"),
        sprintf(if (identical(r$design, "CRD")) " %s needs exactly one treatment factor, so the standard ANOVA is shown."
                else " %s exists only for CRD with one factor (there is no version for blocks or split plots), so the standard ANOVA is shown.",
                if (identical(input$aov_method, "trimmed")) "The trimmed-means comparison" else "Welch's ANOVA")),
      notes_ui(r$data_notes),
      div(class = if (r$significant || r$interaction_significant) "alert alert-success" else "alert alert-info", r$summary),
      result_card("APA-style report", lapply(r$apa, p), copy = "cp_apa"),
      thai_card(thai_summary_aov(r), "cp_aov_th"),
      result_card("Analysis of variance", html_table(tb$anova), copy = "cp_anova", copy_label = "Copy table",
                  if (is_comb(r$design)) p(class = "text-muted small mb-0", sprintf("Environments are fixed effects, as in STAR: %s is tested against %s; the other effects against the pooled error.",
                                                                                  r$env, if (identical(r$design, "CombRCBD")) paste(r$block, "within", r$env) else "the pooled error"))),
      if (is_comb(r$design)) result_card(paste("Analysis of each", r$env), html_table(by_env_display(r)), bartlett_note(r)),
      result_card("Means (publication layout)", copy = "cp_means", copy_label = "Copy means tables",
        if (!is.null(r$transform)) p(class = "text-muted small", icon("circle-info"), sprintf(" The analysis used %s transformed as %s. Means, SE, letters, SE(m), SED and LSD are on the transformed scale; the original-unit column gives the means back-transformed for reporting.",
                                                                                              r$original_response, transforms[[r$transform]]$label)),
        lapply(names(tb$agronomy), function(f) tagList(h6(paste("Means of", f)), html_table(tb$agronomy[[f]]),
                                                     if (tb$letters[[f]]) p(class = "text-muted small", "Means with the same letter are not significantly different.") else no_letters_note(r, f))),
        p(class = "text-muted small", agronomy_note(r)),
        if (!is.null(tb$cells)) tagList(h6(paste("Cell means:", paste(r$factors, collapse = " x "))), html_table(tb$cells))),
      lapply(names(tb$simple), function(nm) {
        sm <- r$simple[[nm]]
        result_card(sprintf("Comparison of %s at each level of %s (%s test)", attr(sm, "compare"), attr(sm, "at"), attr(sm, "test")),
                    html_table(tb$simple[[nm]]),
                    p(class = "text-muted small mb-0", "Means with the same letter (within a column) are not significantly different."))
      }),
      code_card(aov_script(), "cp_aov_script",
                missing = "R code is not available yet for an RCBD with missing responses (the app estimates the missing values).")
    )
  })

  output$plot_picker <- renderUI({
    r <- ok()
    if (aov_rank() || aov_trimmed()) return(p(class = "text-muted small", sprintf("Box plot of the observations, for the %s (error bars and the classic style do not apply).", method_names[[aov_mode()]])))
    ch <- c(setNames(r$factors, paste("Means of", r$factors)), if (!is.null(r$cells)) c("Interaction (grouped bars)" = "interaction", if (use_gg()) c("Interaction (lines)" = "interaction_line")))
    cur <- isolate(input$plot_effect)   # keep the choice when the style is switched
    if (length(ch) > 1) selectInput("plot_effect", "Plot", ch, selected = if (!is.null(cur) && cur %in% ch) cur else if (r$interaction_significant) "interaction" else r$factors[1])
  })
  plot_effect <- reactive({ r <- ok(); pe <- input$plot_effect; if (is.null(pe) || !(pe %in% c(r$factors, if (!is.null(r$cells)) c("interaction", "interaction_line")))) r$factors[1] else pe })
  use_gg <- reactive(identical(input$plot_style, "gg"))
  # the ggplot2 figure (a ggplot object); the classic style is drawn with base graphics instead
  # the result the means plot is drawn from: for Welch's ANOVA the letters are those of the Games-Howell test
  plot_obj <- reactive({
    r <- ok(); if (!aov_welch()) return(r)
    w <- welch_try(); req(!inherits(w, "star_error")); f <- r$factors
    r$means[[f]]$Group <- w$means$Group[match(r$means[[f]]$Treatment, w$means$Treatment)]
    r$effects$Significant[r$effects$Effect == f] <- w$significant
    r$tests_used[[f]] <- "Games-Howell"
    r
  })
  gg_means <- reactive(apply_plot_customizations(do.call(star_ggplot_means, c(list(plot_obj(), plot_effect(), data = if (isTRUE(input$gg_points)) used_data(), error = input$gg_error %||% "se",
                                                         main = input$gg_title, xlab = input$gg_xlab, ylab = input$gg_ylab), plot_style("aov"))), "gg", "aov"))
  gg_rank <- reactive({ np <- if (aov_trimmed()) trimmed_try() else np_try(); req(!is.null(np), !inherits(np, "star_error"))
    apply_plot_customizations(do.call(star_ggplot_rank, c(list(np, points = isTRUE(input$gg_points), main = input$gg_title, xlab = input$gg_xlab, ylab = input$gg_ylab),
                                                         plot_style("aov"))), "gg", "aov") })
  # the figure shown and downloaded: the box plot for a rank-based analysis, else the means
  gg_current <- reactive(if (aov_rank() || aov_trimmed()) gg_rank() else gg_means())
  draw_means <- function() {
    if (aov_rank() || aov_trimmed()) print(gg_rank())
    else if (use_gg()) print(gg_means())
    else star_plot_means(plot_obj(), sub("_line$", "", plot_effect()), main = input$gg_title, xlab = input$gg_xlab, ylab = input$gg_ylab)
  }
  output$means_plot <- renderPlot(draw_means(),
    width = function() live_plot_dims("aov")$width,
    height = function() live_plot_dims("aov")$height
  )
  # cards, messages and flagged table of one set of residuals; `passed` and `messages` describe that set only. Split
  # plots get no recommendation on the Results tab, so their cards do not point to one.
  diag_block <- function(dg, passed, messages, lights = NULL, row_note = "Row = row number in your data.") tagList(
    diag_summary_cards(dg, lights, advice = !is_split(ok()$design)),
    div(class = if (passed) "alert alert-success" else "alert alert-warning", tags$ul(lapply(messages, tags$li))),
    if (nrow(dg$outliers)) {
      o <- dg$outliers
      o[] <- lapply(o, function(v) if (is.numeric(v)) fmt_n(v, 3) else as.character(v))
      tagList(h4("Flagged observations"), p(class = "text-muted", row_note, " |Studentized residual| > 3 = outlier, 2 to 3 = suspect."), html_table(o))
    })
  output$diag_ui <- renderUI({
    dg <- ok()$diagnostics
    if (is.null(dg)) return(div(class = "alert alert-warning", "Assumption checks could not be computed for this data."))
    lights <- tryCatch(star_assumption_lights(ok()), error = function(e) NULL)
    if (!is_split(ok()$design)) return(diag_block(dg, dg$ok, dg$messages, lights))
    sub_ok <- dg$n_outliers == 0 && !isFALSE(dg$normality$ok) && !isFALSE(dg$homogeneity$ok)
    tagList(
      p(class = "text-muted", icon("circle-info"), " A split plot has two error terms, so the residuals are checked at each level: ",
        strong("sub plots"), " (Error(b), single values) and ", strong("main plots"), " (Error(a), the mean of each main plot; below the first plot)."),
      h4(sprintf("Sub plots: %s within main plots (Error(b))", ok()$sub)),
      diag_block(dg, sub_ok, dg$messages_sub, lights))
  })
  output$diag_main_ui <- renderUI({
    r <- ok(); dg <- r$diagnostics
    req(is_split(r$design), dg)
    mp <- dg$main_plot
    tagList(
      hr(),
      h4(sprintf("Main plots: %s (Error(a))", r$main)),
      p(class = "text-muted", sprintf("Each point is the mean of one main plot (%s x %s) over the levels of %s.", r$block, r$main, r$sub)),
      if (is.null(mp)) div(class = "alert alert-info", tags$ul(lapply(dg$messages_main, tags$li)))
      else tagList(
        diag_block(mp, mp$ok, dg$messages_main, row_note = sprintf("Row = main plot (%s / %s).", r$block, r$main)),
        plotOutput("diag_plot_main", height = "440px")))
  })
  output$diag_plot_main <- renderPlot({
    mp <- ok()$diagnostics$main_plot; req(mp)
    if (identical(input$diag_style, "gg")) print(gg_diag(mp, "aov")) else star_plot_diagnostics(mp)
  })
  observeEvent(input$cp_np, session$sendCustomMessage("copy_text", list(what = "Rank-based test", text = paste(np_res()$summary, tsv(np_display(np_res())), sep = "\n"))))
  gg_diag <- function(dg, p) { st <- plot_style(p); star_ggplot_diagnostics(dg, st$base_size - 1, st$theme, st$legend) }
  output$diag_plot <- renderPlot({
    dg <- ok()$diagnostics; req(dg)
    if (identical(input$diag_style, "gg")) print(gg_diag(dg, "aov")) else star_plot_diagnostics(dg)
  })
  # STAR's own printout of the same analysis, built only when the tab is viewed, copied or downloaded
  report_text <- reactive(star_report(ok())) |> bindCache(ok(), cache = star_cache)
  output$report <- renderText(paste(report_text(), collapse = "\n"))

  xlsx_sheets <- reactive({
    r <- ok()
    if (aov_rank()) {
      np <- np_try(); req(!is.null(np), !inherits(np, "star_error"))
      return(list(`Rank test` = np$means,
                  Info = data.frame(Item = c("App version", "Design", "Response", "Treatment factor", "Block", "Test", "Chi-square", "df", "p value", "Alpha", "Summary", "Data checks"),
                                    Value = c(version_label(), r$design, np$response, np$treatment, np$block %||% "-", np$name, round(np$statistic, 4), np$df, signif(np$p, 4), np$alpha, np$summary,
                                              if (length(r$data_notes)) paste(r$data_notes, collapse = " | ") else "none"))))
    }
    if (aov_trimmed()) {
      x <- trimmed_try(); req(!inherits(x, "star_error"))
      return(list(`Trimmed means` = x$means, `Yuen tests` = x$pairs,
                  Info = data.frame(Item = c("App version", "Design", "Response", "Treatment factor", "Test", "Trimming", "Bootstrap resamples", "F", "df1", "df2", "p value", "Alpha", "Summary"),
                                    Value = c(version_label(), r$design, r$original_response %||% r$response, x$treatment, "Heteroscedastic ANOVA on trimmed means (t1way), Yuen tests with Holm adjustment",
                                              x$tr, x$B, round(x$statistic, 4), x$df1, round(x$df2, 4), signif(x$p, 4), x$alpha, x$summary))))
    }
    if (aov_welch()) {
      w <- welch_try(); req(!inherits(w, "star_error"))
      return(list(`Welch means` = w$means, `Games-Howell` = w$pairs,
                  Info = data.frame(Item = c("App version", "Design", "Response", "Transformation", "Treatment factor", "Test", "F", "df1", "df2", "p value", "Alpha", "Summary"),
                                    Value = c(version_label(), r$design, r$original_response %||% r$response, if (is.null(r$transform)) "none" else transforms[[r$transform]]$label,
                                              w$treatment, "Welch's ANOVA (Games-Howell comparisons)", round(w$statistic, 4), w$df1, round(w$df2, 4), signif(w$p, 4), w$alpha, w$summary))))
    }
    tb <- tables()
    sheets <- list(ANOVA = r$anova,
                   # the factors' tables differ in their first column name and in having a Group column
                   `Publication table` = do.call(rbind, lapply(names(tb$agronomy), function(f) {
                     a <- tb$agronomy[[f]]; names(a)[1] <- "Level"
                     if (!"Group" %in% names(a)) a$Group <- ""
                     data.frame(Effect = f, a, check.names = FALSE, stringsAsFactors = FALSE)
                   })),
                   Means = do.call(rbind, lapply(names(tb$means), function(f) {
                     m <- data.frame(Effect = f, r$means[[f]], row.names = NULL)
                     if (!tb$letters[[f]]) m$Group <- ""   # as on the screen: no letters without a significant F test
                     if (!is.null(r$transform)) m$`Mean (original unit)` <- transforms[[r$transform]]$back(m$Mean)
                     pr <- precision_of(r, f); m$`SE(m)` <- pr[["sem"]]; m$SED <- pr[["sed"]]; m$LSD <- pr[["lsd"]]
                     m
                   })))
    if (is_comb(r$design)) sheets$`Each environment` <- cbind(r$by_env, `Bartlett p (all environments)` = c(r$bartlett$p, rep(NA, nrow(r$by_env) - 1)))
    if (!is.null(r$cells)) sheets$`Cell means` <- r$cells
    if (length(r$simple)) sheets$`Simple effects` <- do.call(rbind, lapply(names(r$simple), function(nm) {
      sm <- r$simple[[nm]]; data.frame(Comparison = nm, setNames(sm[c(attr(sm, "at"), attr(sm, "compare"), "Mean", "N", "SE", "Group")], c("At level", "Level", "Mean", "N", "SE", "Group")), row.names = NULL)
    }))
    sheets$Info <- data.frame(Item = c("App version", "Design", "Response", "Transformation", "Treatment factor(s)", "Block", "Mean comparison", "Alpha", "Grand mean", "CV (%)", "Summary", "Data checks"),
                              Value = c(version_label(), r$design, r$response, if (is.null(r$transform)) "none" else paste(transforms[[r$transform]]$label, "of", r$original_response), paste(r$factors, collapse = ", "), if (is.null(r$block)) "-" else r$block,
                                        if (r$test == "auto") "auto (LSD if <= 5 levels, else HSD)" else r$test, r$alpha,
                                        round(r$grand_mean, 4),
                                        if (is.null(r$cv_a)) round(r$cv, 2) else sprintf("CV(a) = %.2f, CV(b) = %.2f", r$cv_a, r$cv_b), r$summary,
                                        if (length(r$data_notes)) paste(r$data_notes, collapse = " | ") else "none"))
    sheets
  })
  output$dl_xlsx <- downloadHandler(function() switch(aov_mode(), rank = "rdstat_rank_test.xlsx", welch = "rdstat_welch_anova.xlsx", trimmed = "rdstat_trimmed_means.xlsx", "rdstat_anova.xlsx"), function(file) writexl::write_xlsx(xlsx_sheets(), file))
  output$dl_txt <- downloadHandler("rdstat_anova_report.txt", function(file) writeLines(c(paste(app_name, "web app", version_label()), "", report_text()), file))
  output$dl_aov_plot <- downloadHandler(function() paste0("rdstat_means_plot.", input$aov_fmt), function(file) {
    fmt <- input$aov_fmt %||% "png"; dpi <- as.numeric(input$aov_dpi %||% 300); wh <- export_dims("aov")
    if (use_gg() || aov_rank() || aov_trimmed()) { star_ggsave(gg_current(), file, width = wh[1], height = wh[2], dpi = dpi); return(invisible()) }
    if (fmt == "png") png(file, width = wh[1] * dpi, height = wh[2] * dpi, res = dpi)
    else if (fmt == "svg") svg(file, width = wh[1], height = wh[2]) else pdf(file, width = wh[1], height = wh[2])
    on.exit(dev.off()); draw_means()
  })

  # ---- Thai summary and Word report ----
  aov_file_name <- reactive(script_file(input$src == "up", if (input$src == "up") input$file$name else setup()$file))
  aov_script <- reactive(script_aov(ok(), aov_file_name()))
  trimmed_script <- reactive({ x <- trimmed_try(); req(!inherits(x, "star_error")); script_trimmed(x, ok(), aov_file_name()) })
  observeEvent(input$cp_trim_script, session$sendCustomMessage("copy_text", list(what = "R code", text = trimmed_script())))
  observeEvent(input$cp_trim_th, session$sendCustomMessage("copy_text", list(what = "สรุปผล", text = thai_summary_trimmed(trimmed_try()))))
  observeEvent(input$cp_trim_apa, session$sendCustomMessage("copy_text", list(what = "APA text", text = trimmed_try()$apa)))
  observeEvent(input$cp_trim, session$sendCustomMessage("copy_text", list(what = "Trimmed means", text = tsv(trimmed_display(trimmed_try())))))
  observeEvent(input$cp_yuen, session$sendCustomMessage("copy_text", list(what = "Yuen table", text = tsv(yuen_display(trimmed_try())))))
  welch_script <- reactive({ w <- welch_try(); req(!inherits(w, "star_error")); script_welch(w, ok(), aov_file_name()) })
  observeEvent(input$cp_welch_script, session$sendCustomMessage("copy_text", list(what = "R code", text = welch_script())))
  observeEvent(input$cp_welch_th, session$sendCustomMessage("copy_text", list(what = "สรุปผล", text = thai_summary_welch(welch_try()))))
  observeEvent(input$cp_welch_apa, session$sendCustomMessage("copy_text", list(what = "APA text", text = welch_try()$apa)))
  observeEvent(input$cp_welch, session$sendCustomMessage("copy_text", list(what = "Welch means", text = tsv(welch_display(welch_try())))))
  observeEvent(input$cp_gh, session$sendCustomMessage("copy_text", list(what = "Games-Howell table", text = tsv(gh_display(welch_try())))))
  np_script <- reactive({ np <- np_try(); req(!is.null(np), !inherits(np, "star_error")); script_np(np, aov_file_name()) })
  observeEvent(input$cp_np_script, session$sendCustomMessage("copy_text", list(what = "R code", text = np_script())))
  observeEvent(input$cp_np_th, session$sendCustomMessage("copy_text", list(what = "สรุปผล", text = thai_summary_np(np_try()))))
  observeEvent(input$cp_np_apa, session$sendCustomMessage("copy_text", list(what = "APA text", text = np_try()$apa)))
  observeEvent(input$cp_aov_script, session$sendCustomMessage("copy_text", list(what = "R code", text = aov_script())))
  observeEvent(input$cp_aov_th, session$sendCustomMessage("copy_text", list(what = "สรุปผล", text = thai_summary_aov(ok()))))
  design_names <- c(CRD = "Completely randomized design (CRD)", RCBD = "Randomized complete block design (RCBD)",
                    SplitCRD = "Split plot in CRD", SplitRCBD = "Split plot in RCBD",
                    CombRCBD = "Combined analysis over environments (RCBD)", CombCRD = "Combined analysis over environments (CRD)")
  output$dl_doc <- downloadHandler(function() switch(aov_mode(), rank = "rdstat_rank_test_report.doc", welch = "rdstat_welch_anova_report.doc", trimmed = "rdstat_trimmed_means_report.doc", "rdstat_anova_report.doc"), function(file) {
    r <- ok(); np <- np_res(); dg <- r$diagnostics
    if (aov_trimmed()) {
      x <- trimmed_try(); req(!inherits(x, "star_error"))
      png <- tryCatch({ f <- tempfile(fileext = ".png"); star_ggsave(gg_rank(), f, width = 6.5, height = 4.5, dpi = 200); f }, error = function(e) NULL)
      info <- data.frame(Item = c("Design", "Response", "Treatment factor", "Test", "Alpha", "Data", "Date", "App"),
                         Value = c(design_names[[r$design]], r$original_response %||% r$response, x$treatment, "Trimmed means (20%), Yuen tests (Holm)", x$alpha,
                                   aov_file_name() %||% "-", format(Sys.Date()), paste(app_name, version_label())), stringsAsFactors = FALSE)
      blocks <- c(word_table(info), w_h2("สรุปผล"), w_p(thai_summary_trimmed(x)), w_p(x$summary),
        w_h2("Trimmed means"), word_table(trimmed_display(x)), w_h2("Yuen's comparisons"), word_table(yuen_display(x)),
        w_h2("APA-style report"), w_p(x$apa),
        w_h2("Methods and references"), w_p(methods_sentence(gg_version())), paste0("<ul>", paste0("<li>", htmltools::htmlEscape(package_refs()), "</li>", collapse = ""), "</ul>"))
      return(word_doc(file, paste(app_name, "- trimmed means:", r$original_response %||% r$response), blocks, png))
    }
    if (aov_welch()) {
      w <- welch_try(); req(!inherits(w, "star_error"))
      png <- tryCatch({ f <- tempfile(fileext = ".png"); star_ggsave(gg_means(), f, width = 6.5, height = 4.5, dpi = 200); f }, error = function(e) NULL)
      info <- data.frame(Item = c("Design", "Response", "Treatment factor", "Transformation", "Test", "Alpha", "Data", "Date", "App"),
                         Value = c(design_names[[r$design]], r$original_response %||% r$response, w$treatment, if (is.null(r$transform)) "none" else transforms[[r$transform]]$label,
                                   "Welch's ANOVA, Games-Howell comparisons", w$alpha, aov_file_name() %||% "-", format(Sys.Date()), paste(app_name, version_label())), stringsAsFactors = FALSE)
      blocks <- c(word_table(info), w_h2("สรุปผล"), w_p(thai_summary_welch(w)), w_p(w$summary),
        w_h2("Welch's ANOVA: means"), word_table(welch_display(w)),
        if (w$significant) w_p("Means with the same letter do not differ (Games-Howell test).", "small"),
        w_h2("Games-Howell comparisons"), word_table(gh_display(w)),
        w_h2("APA-style report"), w_p(w$apa),
        w_h2("Methods and references"), w_p(methods_sentence(gg_version())), paste0("<ul>", paste0("<li>", htmltools::htmlEscape(package_refs()), "</li>", collapse = ""), "</ul>"))
      return(word_doc(file, paste(app_name, "- Welch's ANOVA:", r$original_response %||% r$response), blocks, png))
    }
    if (aov_rank()) {
      req(!is.null(np))
      png <- tryCatch({ f <- tempfile(fileext = ".png"); star_ggsave(gg_rank(), f, width = 6.5, height = 4.5, dpi = 200); f }, error = function(e) NULL)
      info <- data.frame(Item = c("Design", "Response", "Treatment factor", "Block", "Test", "Alpha", "Data", "Date", "App"),
                         Value = c(design_names[[r$design]], np$response, np$treatment, np$block %||% "-", np$name, np$alpha,
                                   aov_file_name() %||% "-", format(Sys.Date()), paste(app_name, version_label())), stringsAsFactors = FALSE)
      blocks <- c(word_table(info), w_h2("สรุปผล"), w_p(thai_summary_np(np)), w_p(np$summary),
        w_h2(paste(np$name, "test")), word_table(np_display(np)),
        if (np$significant) w_p(sprintf("Treatments with the same letter do not differ (comparison of %s, agricolae).", tolower(np$rank_label)), "small"),
        w_h2("APA-style report"), w_p(np$apa),
        w_h2("Methods and references"), w_p(methods_sentence(gg_version())), paste0("<ul>", paste0("<li>", htmltools::htmlEscape(package_refs()), "</li>", collapse = ""), "</ul>"))
      return(word_doc(file, paste(app_name, "-", np$name, "test:", np$response), blocks, png))
    }
    tb <- tables()
    png <- tryCatch({ f <- tempfile(fileext = ".png"); star_ggsave(gg_means(), f, width = 6.5, height = 4.5, dpi = 200); f }, error = function(e) NULL)
    info <- data.frame(Item = c("Design", "Response", "Treatment factor(s)", "Block", "Transformation", "Mean comparison", "Alpha", "Data", "Date", "App"),
                       Value = c(design_names[[r$design]], r$original_response %||% r$response, paste(r$factors, collapse = ", "), r$block %||% "-",
                                 if (is.null(r$transform)) "none" else transforms[[r$transform]]$label, paste(unique(r$tests_used), collapse = ", "),
                                 r$alpha, aov_file_name() %||% "-", format(Sys.Date()), paste(app_name, version_label())), stringsAsFactors = FALSE)
    blocks <- c(word_table(info),
      w_h2("สรุปผล"), w_p(thai_summary_aov(r)), w_p(r$summary),
      w_h2("Analysis of variance"), word_table(tb$anova),
      if (is_comb(r$design)) c(w_h2(paste("Analysis of each", r$env)), word_table(by_env_display(r)), w_p(.star_bartlett_text(r$bartlett, r$alpha), "small")),
      w_h2("Means"), unlist(lapply(names(tb$agronomy), function(f) c(w_p(paste("Means of", f)), word_table(tb$agronomy[[f]])))),
      w_p(paste("Means with the same letter are not significantly different.", agronomy_note(r)), "small"),
      if (!is.null(tb$cells)) c(w_p(paste("Cell means:", paste(r$factors, collapse = " x "))), word_table(tb$cells)),
      unlist(lapply(names(tb$simple), function(nm) c(w_p(nm), word_table(tb$simple[[nm]])))),
      if (!is.null(dg)) c(w_h2("Assumption checks"), paste0("<ul>", paste0("<li>", htmltools::htmlEscape(dg$messages), "</li>", collapse = ""), "</ul>")),
      w_h2("APA-style report"), vapply(r$apa, w_p, ""),
      w_h2("Methods and references"), w_p(methods_sentence(gg_version())), paste0("<ul>", paste0("<li>", htmltools::htmlEscape(package_refs()), "</li>", collapse = ""), "</ul>"))
    word_doc(file, paste(app_name, "- analysis of variance:", r$original_response %||% r$response), blocks, png)
  })

  observeEvent(input$cp_anova, session$sendCustomMessage("copy_text", list(what = "ANOVA table", text = tsv(tables()$anova))))
  observeEvent(input$cp_means, session$sendCustomMessage("copy_text", list(what = "Means tables", text = paste(
    c(unlist(lapply(names(tables()$agronomy), function(f) c(paste("Means of", f), tsv(tables()$agronomy[[f]]), ""))), agronomy_note(ok()), "",
      if (!is.null(tables()$cells)) c("Cell means", tsv(tables()$cells), ""),
      unlist(lapply(names(tables()$simple), function(nm) c(nm, tsv(tables()$simple[[nm]]), "")))), collapse = "\n"))))
  observeEvent(input$cp_apa, session$sendCustomMessage("copy_text", list(what = "APA text", text = paste(ok()$apa, collapse = " "))))
  observeEvent(input$cp_report, session$sendCustomMessage("copy_text", list(what = "Report", text = paste(report_text(), collapse = "\n"))))
  observeEvent(input$copy_status, {
    st <- input$copy_status
    showNotification(if (isTRUE(st$ok)) paste(st$what, "copied to clipboard") else "Copy failed: select the text and copy manually",
                     type = if (isTRUE(st$ok)) "message" else "error", duration = 3)
  })

