# Copy the engine sources the app needs into the Shiny app so Shinylive can bundle them.
# The app's own analysis code (star_*.R) goes to prototype/app/star; it uses base R, agricolae, lmtest and mctest.
# The functions recovered from IRRI STAR that print the Classic STAR report go to prototype/app/star/legacy, which
# app.R sources into an environment of its own. Every bundled file is downloaded and parsed each time the app is opened,
# so only the files these need are copied.
# Usage (from the repo root): Rscript scripts/build_app.R
needed <- c(
  "star_aov", "star_correlation", "star_diagnostics", "star_ggplot", "star_ggplot_stats", "star_helpers",
  "star_prepare_data", "star_read_table", "star_regression", "star_splitplot", "star_ttest", "star_combined",
  "star_nonparametric", "star_recommend", "star_trimmed", "star_report", "star_repeated")
# STAR's AOVTest() and combAOVTest() and what they call, for star_report(). If a function is reported missing, add its
# file here (prototype/tests/test_server.R catches that).
legacy <- c(
  "AOVTest", "AOVTest.default", "ConstructAOVTable", "ConstructAOVTable.default", "ConstructAOVTable.summary.aov",
  "ConstructAOVTable.summary.aovlist", "cv", "cv.data.frame", "cv.default", "DataAttribute", "is.wholenumber", "lastC",
  "lillie.test", "LSD.test", "HSD.test", "duncan.test", "SNK.test", "scheffe.test", "order.group", "printDataFrame",
  "tapply.stat", "trimStrings", "zzz_compat", "pairwiseWithin", "pairwiseAmong", "pairwiseComparison",
  "SignificantEffect", "SignificantEffect.default", "printAOVTable", "printAOVTable.default", "PredictedValues",
  "PredictedValues.default", "PredictedValues.aovlist", "ClassInformation", "ClassInformation.default",
  "combAOVTest", "combAOVTest.default", "buildAOVModel", "GenerateBalanceData", "estMissData", "prelimCombAOV",
  "HeteroskedasticityTest", "HeteroskedasticityTest.default")
dest <- file.path("prototype", "app", "star")
unlink(dest, recursive = TRUE)
dir.create(dest, recursive = TRUE)
src <- file.path("engine", "R", paste0(needed, ".R"))
stopifnot(all(file.exists(src)), all(file.copy(src, dest)))
dir.create(file.path(dest, "legacy"))
legacy_src <- file.path("engine", "R", paste0(legacy, ".R"))
stopifnot(all(file.exists(legacy_src)), all(file.copy(legacy_src, file.path(dest, "legacy"))))
# version shown in the app and written to every result file
commit <- tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE), error = function(e) "unknown")
writeLines(c(paste0("version=", Sys.getenv("STAR_APP_VERSION", "0.1.0")), paste0("commit=", commit[1]),
             paste0("built=", format(Sys.Date()))), file.path("prototype", "app", "VERSION"))
cat("copied", length(src), "engine files to", dest, "and", length(legacy_src), "STAR files to", file.path(dest, "legacy"), "\n")
