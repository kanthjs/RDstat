# Export the app as a static Shinylive site, with RDstat's loading screen (prototype/loading_screen.html) in place of
# Shinylive's bare animation while R downloads and the app starts.
# Usage (from the repo root, after scripts/build_app.R): Rscript scripts/export_site.R [destdir]   (default: _site)
args <- commandArgs(trailingOnly = TRUE)
dest <- if (length(args)) args[1] else "_site"
loading <- paste(readLines(file.path("prototype", "loading_screen.html"), encoding = "UTF-8"), collapse = "\n")
shinylive::export(
  "prototype/app", dest,
  template_params = list(title = "RDstat", include_before_body = loading)
)
