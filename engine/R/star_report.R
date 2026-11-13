## Classic STAR printout of an analysis of variance (star_aov, star_splitplot or star_combined result).
## The tables, means and letters of the app do not come from this printout: it is STAR's own AOVTest() or
## combAOVTest() run again on the same data, for users who want the familiar STAR layout. It is built only when asked.
## In the web app the STAR functions live in their own environment (`star_legacy`, see app.R) so that they do not mask
## the functions of R or of the packages the analyses use; in the engine package they are found in the namespace.

star_report <- function(x) {
  if (is.null(x$data)) {
    return(character())
  }
  args <- list(data = x$data, respvar = x$response, sig = x$alpha)
  fun <- "AOVTest"
  if (x$design %in% c("CombRCBD", "CombCRD")) {
    fun <- "combAOVTest"
    args <- c(args, list(design = sub("^Comb", "", x$design), factor1 = x$treatment, set = x$env))
    if (x$design == "CombRCBD") args$rep1 <- x$block
  } else if (x$design %in% c("SplitRCBD", "SplitCRD", "RepRCBD", "RepCRD")) {
    # STAR has no repeated-measures analysis: a repeated-measures result is printed as the split plot in time it is
    # (without the sphericity correction)
    args <- c(args, list(design = sub("^Rep", "Split", x$design), factor1 = x$main, factor2 = x$sub, rep1 = x$block))
  } else {
    args <- c(args, list(design = x$design, factor1 = x$factors))
    if (x$design == "RCBD") args$rep1 <- x$block
  }
  # STAR chooses the test of each effect (LSD up to 5 levels, else Tukey) unless one was picked; it has no Bonferroni
  if (!x$test %in% c("auto", "bonferroni")) {
    args$pwTest <- x$test
    args$pwVar <- x$response
  }
  out <- tryCatch(
    utils::capture.output(do.call(.star_legacy_fun(fun), args)),
    error = function(e) "The classic STAR report could not be produced for this data."
  )
  if (isTRUE(x$design %in% c("RepRCBD", "RepCRD"))) {
    out <- c("Note: STAR has no repeated-measures analysis. This is its split plot in time (treatment = main plot, time = sub plot),",
             "with uncorrected F tests; see the Results tab for the sphericity check and the corrected tests.", "", out)
  }
  out
}

# A STAR function: from the app's `star_legacy` environment when there is one, else from the engine namespace
.star_legacy_fun <- function(name) {
  env <- if (exists("star_legacy", envir = globalenv(), inherits = FALSE)) get("star_legacy", envir = globalenv()) else environment(star_report)
  get(name, envir = env, mode = "function")
}
