# Copy the engine sources the app needs into the Shiny app so Shinylive can bundle them.
# Only the app's own analysis code (star_*.R) is bundled: it uses base R, agricolae and lmtest, not the functions
# recovered from IRRI STAR, and every bundled file is downloaded and parsed each time the app is opened.
# Usage (from the repo root): Rscript scripts/build_app.R
needed <- c(
  "star_aov", "star_correlation", "star_diagnostics", "star_ggplot", "star_ggplot_stats", "star_helpers",
  "star_prepare_data", "star_read_table", "star_regression", "star_splitplot", "star_ttest", "star_combined",
  "star_nonparametric", "star_recommend", "star_trimmed")
dest <- file.path("prototype", "app", "star")
unlink(dest, recursive = TRUE)
dir.create(dest, recursive = TRUE)
src <- file.path("engine", "R", paste0(needed, ".R"))
stopifnot(all(file.exists(src)), all(file.copy(src, dest)))
# version shown in the app and written to every result file
commit <- tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE), error = function(e) "unknown")
writeLines(c(paste0("version=", Sys.getenv("STAR_APP_VERSION", "0.1.0")), paste0("commit=", commit[1]),
             paste0("built=", format(Sys.Date()))), file.path("prototype", "app", "VERSION"))
cat("copied", length(src), "engine files to", dest, "\n")
