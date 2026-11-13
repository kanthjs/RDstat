# irid_components.R: Fine-grained reactive components for RDstat using `irid`
# If the `irid` package is installed, components bind reactive values directly to DOM attributes
# and text nodes for surgical, instantaneous updates without DOM destruction (renderUI flicker).
# If `irid` is not available, these functions gracefully fall back to standard Shiny uiOutput/renderUI.

irid_available <- function() {
  requireNamespace("irid", quietly = TRUE)
}

# Attach irid client dependencies to any tag (preserving inline/block container)
irid_container <- function(id, inline = FALSE) {
  if (irid_available()) {
    htmltools::attachDependencies(
      shiny::uiOutput(id, inline = inline),
      irid:::irid_dependency()
    )
  } else {
    shiny::uiOutput(id, inline = inline)
  }
}

# ---- Live View Dimension Badge ----
# Displays real-time download dimensions and aspect ratio in the plot header.
# With irid: only the text child inside the <span> is updated reactively, preserving DOM identity.
live_badge_ui <- function(id) {
  irid_container(id, inline = TRUE)
}

render_live_badge <- function(p, input, export_dims) {
  calc_label <- function() {
    d <- input[[paste0(p, "_dim")]] %||% "std"
    wh_cm <- export_dims(p) * 2.54
    ratio <- round(wh_cm[1] / wh_cm[2], 2)
    switch(d,
      std = sprintf("Standard 20 × 14 ซม. (%s:1)", ratio),
      j1 = sprintf("Journal 1 คอลัมน์ 8.5 × 6.5 ซม. (%s:1)", ratio),
      j2 = sprintf("Journal 2 คอลัมน์ 17.5 × 12 ซม. (%s:1)", ratio),
      slide = sprintf("Slide 16:9 25 × 14 ซม. (%s:1)", ratio),
      custom = sprintf("กำหนดเอง %.1f × %.1f ซม. (%s:1)", wh_cm[1], wh_cm[2], ratio)
    )
  }

  if (irid_available()) {
    irid::renderIrid({
      badge_text <- reactive(calc_label())
      irid::tags$span(
        class = "badge bg-light text-success border border-success-subtle px-2 py-1",
        title = "ขนาดและสัดส่วนที่จะได้เมื่อบันทึกภาพ",
        irid::tags$i(class = "fa fa-ruler-combined me-1"),
        \() badge_text()
      )
    })
  } else {
    shiny::renderUI({
      lbl <- calc_label()
      tags$span(
        class = "badge bg-light text-success border border-success-subtle px-2 py-1",
        title = "ขนาดและสัดส่วนที่จะได้เมื่อบันทึกภาพ",
        tags$i(class = "fa fa-ruler-combined me-1"),
        lbl
      )
    })
  }
}

# ---- Analysis Method Availability Note ----
# Displays contextual constraints when certain analyses (Welch, Trimmed, Rank) are disabled for
# specific designs (e.g. split-plot, 2 factors, RCBD).
# With irid: uses When() control flow to toggle visibility and surgically update the explanation text.
method_note_ui <- function(id) {
  irid_container(id, inline = FALSE)
}

render_method_note <- function(aov_support) {
  if (irid_available()) {
    irid::renderIrid({
      has_why <- reactive(!is.null(aov_support()$why))
      irid::When(
        has_why,
        \() irid::tags$p(
          class = "text-muted small",
          irid::tags$i(class = "fa fa-circle-info me-1"),
          \() paste0(aov_support()$why, " Greyed-out analyses do not apply.")
        )
      )
    })
  } else {
    shiny::renderUI({
      s <- aov_support()
      if (!is.null(s$why)) {
        tags$p(
          class = "text-muted small",
          tags$i(class = "fa fa-circle-info me-1"),
          paste0(s$why, " Greyed-out analyses do not apply.")
        )
      }
    })
  }
}

# ---- Copy Feedback Status Badge ----
# Micro-interaction badge indicating whether text/script was recently copied to clipboard.
copy_feedback_badge_ui <- function(id) {
  irid_container(id, inline = TRUE)
}

render_copy_feedback_badge <- function(status_reactive) {
  if (irid_available()) {
    irid::renderIrid({
      is_copied <- reactive(isTRUE(status_reactive()))
      irid::When(
        is_copied,
        \() irid::tags$span(
          class = "badge bg-success-subtle text-success border border-success px-2 py-1 ms-2",
          irid::tags$i(class = "fa fa-check me-1"),
          "คัดลอกแล้ว"
        )
      )
    })
  } else {
    shiny::renderUI({
      if (isTRUE(status_reactive())) {
        tags$span(
          class = "badge bg-success-subtle text-success border border-success px-2 py-1 ms-2",
          tags$i(class = "fa fa-check me-1"),
          "คัดลอกแล้ว"
        )
      }
    })
  }
}
