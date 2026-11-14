# data source + labels + file export controls shared by the correlation and regression screens
data_source_ui <- function(p, examples, help = NULL) tagList(
  radioButtons(paste0(p, "_src"), NULL, c("Built-in example" = "ex", "Upload file" = "up"), inline = TRUE),
  conditionalPanel(sprintf("input.%s_src == 'ex'", p), selectInput(paste0(p, "_example"), "Example", examples)),
  conditionalPanel(sprintf("input.%s_src == 'up'", p), fileInput(paste0(p, "_file"), "Data file (.csv, .txt, .xlsx, .xls)", accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls")),
                   uiOutput(paste0(p, "_read_note")), help, file_language_help()))
# what the file reader skipped (title lines above the column names, summary rows below the data), so it is not a surprise
read_note_ui <- function(d) {
  notes <- attr(d, "notes")
  if (length(notes)) div(class = "alert alert-info small", icon("circle-info"), " ", strong("อ่านจากไฟล์ (Read from the file): "),
                         tags$ul(class = "mb-0", lapply(notes, function(n) tags$li(msg_ui(n)))))
}
# the label boxes are folded away until wanted, so that the plot comes first on a small screen
plot_labels_ui <- function(p) tags$details(class = "plot-labels",
  tags$summary("Title and axis labels (optional)"),
  fluidRow(
    column(4, textInput(paste0(p, "_title"), "Title", placeholder = "(automatic)")),
    column(4, textInput(paste0(p, "_xlab"), "X axis label", placeholder = "(automatic)")),
    column(4, textInput(paste0(p, "_ylab"), "Y axis label", placeholder = "(automatic)"))),
  fluidRow(
    column(4, checkboxInput(paste0(p, "_show_title"), "Show title", TRUE)),
    column(4, checkboxInput(paste0(p, "_show_sub"), "Show subtitle / notes", TRUE))))
# plot style: one set of choices for every plot. Each screen has its own copy of the controls (input ids must be
# unique) and the server keeps the copies in step, so a change on one screen applies to all of them.
style_screens <- c("aov", "tt", "corr", "reg")
style_choices <- list(
  pal = c("Okabe-Ito (colour-blind)" = "okabe", "Dark2" = "dark2", "Soft colours (Set2)" = "set2",
          "Nature (NPG)" = "npg", "Science (AAAS)" = "aaas", "The Lancet" = "lancet", "J Clin Oncol (JCO)" = "jco",
          "Viridis" = "viridis", "Cividis (colour-blind)" = "cividis", "Mako" = "mako", "Blues" = "blues", "Yellow-green-blue" = "ylgnbu",
          "Greyscale (B&W print)" = "grey", "Greens" = "green"),
  heat = c("Same as colours" = "auto", "Red - blue (RdBu)" = "rdbu", "Brown - teal (BrBG)" = "brbg", "Pink - green (PiYG)" = "piyg",
           "Vik" = "vik", "Berlin" = "berlin"),
  psize = c("Small" = 0.6, "Medium (default)" = 1, "Large" = 1.4, "Extra large" = 1.8, "Huge" = 2.4),
  shape = c("Circle" = "circle", "Square" = "square", "Diamond" = "diamond", "Triangle" = "triangle", "Triangle (down)" = "triangle_down"),
  theme = c("Classic (axes only)" = "classic", "White with grid lines" = "bw", "Minimal" = "minimal", "Grey background" = "grey"),
  size = c("9 (journal figure)" = 9, "10" = 10, "11" = 11, "12 (default)" = 12, "14" = 14, "16 (slides)" = 16, "18" = 18, "20" = 20),
  legend = c("Automatic" = "auto", "Top" = "top", "Right" = "right", "Bottom" = "bottom", "None" = "none"),
  xangle = c("Automatic" = "auto", "Horizontal (0°)" = "0", "Angled (45°)" = "45", "Vertical (90°)" = "90"))
style_defaults <- c(pal = "okabe", theme = "classic", size = "12", legend = "auto", xangle = "auto", shape = "circle", psize = "1")
plot_style_ui <- function(p) tags$details(class = "plot-labels",
  tags$summary("Plot style: colours, points, theme, font size"),
  fluidRow(
    column(3, selectInput(paste0(p, "_pal"), "Colours", style_choices$pal)),
    column(3, selectInput(paste0(p, "_theme"), "Theme", style_choices$theme)),
    column(3, selectInput(paste0(p, "_size"), "Font size (pt)", style_choices$size, selected = 12)),
    column(3, selectInput(paste0(p, "_legend"), "Legend", style_choices$legend))),
  fluidRow(
    column(3, selectInput(paste0(p, "_shape"), "Point shape", style_choices$shape)),
    column(3, selectInput(paste0(p, "_psize"), "Point size", style_choices$psize, selected = 1)),
    column(3, selectInput(paste0(p, "_xangle"), "X-axis labels", style_choices$xangle)),
    if (p == "corr") column(3, selectInput("corr_heat", "Heatmap colours (+/- correlation)", style_choices$heat))),
  p(class = "text-muted", style = "font-size: 13px;", "These settings apply to every plot in the app. For a one-column journal figure, use font size 9 or 10."))
# downloaded plot size, width x height in cm
export_sizes <- list(std = c(20, 14), j1 = c(8.5, 6.5), j2 = c(17.5, 12), slide = c(25, 14))
plot_live_ui <- function(id, p) {
  tags$div(class = "plot-live-wrapper",
    tags$div(class = "plot-live-card",
      tags$div(class = "plot-live-header",
        tags$div(class = "plot-live-header-left",
          tags$span(class = "plot-live-title",
            tags$i(class = "fa fa-eye text-success me-1"), " Live View ",
            tags$span(class = "text-muted small ms-1", "(สัดส่วนตามขนาดดาวน์โหลดจริง)")
          )
        ),
        tags$div(class = "plot-live-header-right",
          uiOutput(paste0(p, "_live_badge"), inline = TRUE)
        )
      ),
      tags$div(class = "plot-live-viewport",
        stale_dim(imageOutput(id, height = "auto"))
      )
    )
  )
}
plot_export_ui <- function(p) tagList(fluidRow(
  column(2, selectInput(paste0(p, "_fmt"), "File type", c("PNG" = "png", "SVG" = "svg", "PDF" = "pdf"))),
  column(3, conditionalPanel(sprintf("input.%s_fmt == 'png'", p),
                             selectInput(paste0(p, "_dpi"), "Resolution", c("Screen (120 dpi)" = 120, "Print (300 dpi)" = 300, "High (600 dpi)" = 600), selected = 300))),
  column(4, selectInput(paste0(p, "_dim"), "Size", c("Standard (20 x 14 cm)" = "std", "Journal, 1 column (8.5 x 6.5 cm)" = "j1",
                                                    "Journal, 2 columns (17.5 x 12 cm)" = "j2", "Slide 16:9 (25 x 14 cm)" = "slide", "Custom" = "custom"))),
  column(3, br(), stale_dim(conditionalPanel(sprintf("input.go_%s > 0", p), downloadButton(paste0("dl_", p, "_plot"), "Download plot"))))),
  conditionalPanel(sprintf("input.%s_dim == 'custom'", p), fluidRow(
    column(2, offset = 5, numericInput(paste0(p, "_w"), "Width (cm)", 20, min = 3, max = 60, step = 0.5)),
    column(2, numericInput(paste0(p, "_h"), "Height (cm)", 14, min = 3, max = 60, step = 0.5)))),
  p(class = "text-muted small mt-1 mb-0", "ขนาดและสัดส่วนที่เลือกจะแสดงใน Live View ด้านบนแบบเรียลไทม์ และใช้เป็นขนาดของไฟล์ที่ดาวน์โหลด"))
