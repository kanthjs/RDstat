  # ---- shared by the correlation and regression screens ----
  use_data <- function(p, setups) {
    d <- reactive({
      if (input[[paste0(p, "_src")]] == "ex") { s <- setups[[input[[paste0(p, "_example")]]]]; req(s); return(read.csv(s$file)) }
      f <- input[[paste0(p, "_file")]]; req(f)
      tryCatch(star_read_table(f$datapath, f$name),
               error = function(e) { showNotification(paste("Could not read the file:", conditionMessage(e)), type = "error"); req(FALSE) })
    })
    output[[paste0(p, "_read_note")]] <- renderUI(if (input[[paste0(p, "_src")]] == "up") read_note_ui(d()))
    d
  }
  example_of <- function(p, setups) if (input[[paste0(p, "_src")]] == "ex") setups[[input[[paste0(p, "_example")]]]]
  as_error <- function(e) structure(list(message = conditionMessage(e)), class = "star_error")
  plot_download <- function(p, plot_fn) downloadHandler(
    function() paste0("rdstat_", p, "_plot.", input[[paste0(p, "_fmt")]] %||% "png"),
    function(file) { wh <- export_dims(p); star_ggsave(plot_fn(), file, width = wh[1], height = wh[2], dpi = as.numeric(input[[paste0(p, "_dpi")]] %||% 300)) })
  notes_ui <- function(notes) if (length(notes)) div(class = "alert alert-warning", strong("ตรวจข้อมูล (Data checks)"), tags$ul(lapply(notes, function(n) tags$li(msg_ui(n)))))
  copy_button <- function(id, what, text) observeEvent(input[[id]], session$sendCustomMessage("copy_text", list(what = what, text = text())))

