# Copy the engine sources the app needs into the Shiny app so Shinylive can bundle them.
# Only these files are bundled: the rest of the recovered STAR package is not used by the web app, and every
# bundled file is downloaded and parsed each time the app is opened.
# Usage (from the repo root): Rscript scripts/build_app.R
# If a function is reported missing, add its file here (prototype/tests/test_server.R catches that).
needed <- c(
  "AOVTest", "AOVTest.default", "BivariateCorrelationTest", "BivariateCorrelationTest.default",
  "ConstructAOVTable", "ConstructAOVTable.default", "ConstructAOVTable.summary.aov", "ConstructAOVTable.summary.aovlist",
  "cv", "cv.data.frame", "cv.default", "DataAttribute", "Heatmap", "is.wholenumber", "lastC", "lillie.test",
  "LinearRegressionAnalysis", "LinearRegressionAnalysis.default", "LSD.test", "order.group", "printDataFrame",
  "tapply.stat", "trimStrings", "vif", "vif.lm", "zzz_compat", "pairwiseWithin", "pairwiseAmong", "pairwiseComparison", "SignificantEffect.default", "SignificantEffect", "printAOVTable.default", "printAOVTable", "PredictedValues.default", "PredictedValues.aovlist", "PredictedValues", "ClassInformation.default", "ClassInformation",
  # mean comparison tests chosen by name at run time (get(paste0(test, ".test")) in star_helpers.R)
  "HSD.test", "duncan.test", "SNK.test", "scheffe.test",
  "star_aov", "star_correlation", "star_diagnostics", "star_ggplot", "star_ggplot_stats", "star_helpers",
  "star_prepare_data", "star_read_table", "star_regression", "star_splitplot", "star_ttest", "star_combined", "star_nonparametric", "star_recommend", "star_trimmed",
  # STAR's combined analysis, for the classic report of star_combined()
  "combAOVTest", "combAOVTest.default", "buildAOVModel", "GenerateBalanceData", "estMissData", "prelimCombAOV",
  "HeteroskedasticityTest", "HeteroskedasticityTest.default")
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
