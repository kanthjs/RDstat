# ---- helpers ---------------------------------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
# name of this web app: change here (and in the About text) when it is renamed. "STAR" below always means the IRRI program.
app_name <- "RDstat"
app_maintainer <- "Kanthanawit Jaisong"
feedback_form_url <- Sys.getenv("RDSTAT_FEEDBACK_URL", "https://docs.google.com/forms/d/e/1FAIpQLScjlgyby2p30Czp8L3MDcIyHSQ03c3ja07yieUgIeR79M7PQw/viewform")
# short explanation folded away until wanted (for users who are not statisticians)
help_box <- function(title, ...) tags$details(class = "help-box", tags$summary(title), div(class = "help-body", ...))
# "what you get" box shown under the choice the user made (Thai): when to use it, what it gives, what it does not give,
# and the question it answers. Used on the screens (under each choice) and at the top of the Methods topics.
get_box <- function(use, gets, not = NULL, answers = NULL) div(class = "get-box", lang = "th",
  p(strong("ใช้เมื่อ: "), use),
  p(strong("จะได้:")),
  tags$ul(lapply(gets, tags$li)),
  if (!is.null(not)) p(strong("ไม่ได้ / ข้อควรระวัง: "), not),
  if (!is.null(answers)) p(class = "get-q", icon("circle-question"), " ", answers))
# one get_box per choice of a radio button, each shown only while that choice is selected
get_boxes <- function(id, boxes) tagList(lapply(names(boxes), function(k)
  conditionalPanel(sprintf("input.%s == '%s'", id, k), boxes[[k]])))
# version of this build (written by scripts/build_app.R); shown in the About tab and written to every result file
app_version <- local({
  v <- if (file.exists("VERSION")) readLines("VERSION") else character()
  get <- function(k) sub(paste0("^", k, "="), "", grep(paste0("^", k, "="), v, value = TRUE)[1])
  c(version = get("version"), commit = get("commit"), built = get("built"))
})
version_label <- function() { v <- app_version; if (anyNA(v)) "development build" else sprintf("v%s (%s, built %s)", v[["version"]], v[["commit"]], v[["built"]]) }
methods_sentence <- function(gg) sprintf("Data were analysed with %s (web app %s) running R %s.%s in the browser. Analysis of variance used aov(), with missing plots of an RCBD estimated by least squares (Yates's method, using lm() and predict()) and environments combined following Gomez and Gomez (1984), their error variances compared with bartlett.test(); means were compared with the agricolae package (LSD, Tukey HSD, Duncan), and SE(m), SED and LSD were computed from the error mean square of each factor; Welch's ANOVA used oneway.test() with Games-Howell comparisons; trimmed means (20%%) used Yuen's test and Wilcox's heteroscedastic ANOVA with percentile bootstrap intervals; rank-based tests used kruskal() and friedman() (agricolae), with Dunn's test as an option after Kruskal-Wallis; t tests used t.test() and var.test(), with wilcox.test() as the rank-based alternative; correlations used cor.test(); regression used lm(), with variance inflation factors from imcdiag() (mctest); residual normality used the Shapiro-Wilk test; constant variance used the Breusch-Pagan test (lmtest); graphics used ggplot2 %s. See the references below.", app_name, version_label(), R.version$major, R.version$minor, gg)
# packages whose results or tools appear in the output; their references are built from each package's own citation()
cited_packages <- c("agricolae", "lmtest", "mctest", "ggplot2", "readxl", "writexl")
# what each piece of software does in the app, for the table on the Methods page: the package ("R" for base R and
# stats), what it is used for (Thai) and the functions. Tests written in
# this app (Games-Howell, Dunn, trimmed means) are listed under the R functions they call. Keep this in step with methods_sentence() and method_topics.
software_uses <- list(
  list(pkg = "R", use = "ANOVA, ประมาณค่าที่หายไปใน RCBD, การวิเคราะห์ร่วมหลายสภาพแวดล้อม (Bartlett's test), t-test, Welch's ANOVA และ Games-Howell, Dunn's test, ค่าเฉลี่ยตัดปลาย (Yuen) และ bootstrap, สหสัมพันธ์, การถดถอย, Wilcoxon, Shapiro-Wilk, ปรับค่า p, SE(m) SED LSD",
       fun = c("aov()", "lm()", "predict()", "bartlett.test()", "t.test()", "var.test()", "oneway.test()", "ptukey()", "pnorm()", "pt()", "pf()", "quantile()", "qt()",
               "cor.test()", "wilcox.test()", "shapiro.test()", "p.adjust()")),
  list(pkg = "agricolae", use = "เปรียบเทียบค่าเฉลี่ยหลัง ANOVA (LSD, Tukey HSD, Duncan), Kruskal-Wallis, Friedman",
       fun = c("LSD.test()", "HSD.test()", "duncan.test()", "kruskal()", "friedman()")),
  list(pkg = "lmtest", use = "ทดสอบความแปรปรวนคงที่ในการถดถอย (Breusch-Pagan)", fun = "bptest()"),
  list(pkg = "mctest", use = "variance inflation factor (VIF) ของตัวแปรอิสระในการถดถอยพหุคูณ", fun = "imcdiag()"),
  list(pkg = "ggplot2", use = "กราฟทั้งหมด", fun = "ggplot()"),
  list(pkg = "readxl", use = "อ่านไฟล์ Excel ที่อัปโหลด", fun = "read_excel()"),
  list(pkg = "writexl", use = "เขียนผลเป็นไฟล์ Excel", fun = "write_xlsx()")
)
# the version shown beside each entry of software_uses
software_version <- function(pkg) switch(pkg,
  R = paste(R.version$major, R.version$minor, sep = "."),
  { v <- pkg_version(pkg); if (nzchar(v)) v else "ไม่ได้ติดตั้ง" })
# the reference of one package ("base" for R itself), from its citation(); markup and line breaks removed
package_ref <- function(pkg) {
  ref <- tryCatch(paste(format(utils::citation(pkg), style = "text"), collapse = " "),
                  error = function(e) sprintf("%s: R package%s.", pkg, tryCatch(paste(" version", utils::packageVersion(pkg)), error = function(e) "")))
  gsub("[*_]", "", gsub("[[:space:]]+", " ", ref))
}
# the version as the package writes it (1.3-7, not 1.3.7); "" when it is not installed
pkg_version <- function(pkg) tryCatch(utils::packageDescription(pkg)$Version %||% "", error = function(e) "", warning = function(w) "")
package_refs <- function() unname(c(package_ref("base"), vapply(cited_packages, package_ref, "")))
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.0001, "<0.0001", formatC(p, format = "f", digits = 4)))
fmt_n <- function(x, digits = 2) ifelse(is.na(x), "", formatC(x, format = "f", digits = digits, big.mark = ","))

anova_display <- function(a) {
  data.frame(Source = a$Source, DF = formatC(a$DF, format = "d"), `Sum of squares` = fmt_n(a$SS),
             `Mean square` = fmt_n(a$MS), `F value` = fmt_n(a$F), `Pr(>F)` = fmt_p(a$P),
             check.names = FALSE, stringsAsFactors = FALSE)
}

# optional transformation of the response before an ANOVA (Gomez & Gomez, 1984, chapter 7). `back` turns a mean on the
# transformed scale back into the original unit, so that both can be reported.
transforms <- list(
  log = list(label = "log(y + 1)", back = function(m) exp(m) - 1, f = function(y) {
    if (any(y <= -1, na.rm = TRUE)) stop("Cannot apply log(y + 1): every value must be greater than -1.", call. = FALSE)
    log(y + 1) }),
  sqrt = list(label = "sqrt(y + 0.5)", back = function(m) m^2 - 0.5, f = function(y) {
    if (any(y < -0.5, na.rm = TRUE)) stop("Cannot apply sqrt(y + 0.5): every value must be at least -0.5.", call. = FALSE)
    sqrt(y + 0.5) }),
  arcsin = list(label = "arcsin(sqrt(y / 100)), in degrees", back = function(m) 100 * sin(m * pi / 180)^2, f = function(y) {
    if (any(y < 0 | y > 100, na.rm = TRUE)) stop("Cannot apply the arcsine transformation: it is for percentages, so every value must be between 0 and 100.", call. = FALSE)
    asin(sqrt(y / 100)) * 180 / pi }))
# the data with the transformed response added as a new column; `resp` is that column (unchanged for "none")
transform_response <- function(d, resp, tr) {
  if (is.null(tr) || identical(tr, "none")) return(list(data = d, resp = resp))
  new <- paste0(resp, "_", tr)
  d[[new]] <- transforms[[tr]]$f(star_parse_number(d[[resp]]))
  list(data = d, resp = new)
}

# means of one factor; the Group column only when the effect was compared (significant, no interaction);
# with a transformed response also the means back-transformed to the original unit
means_display <- function(m, factor_name, show_groups, back = NULL) {
  out <- data.frame(m$Treatment, fmt_n(m$Mean), stringsAsFactors = FALSE)
  names(out) <- c(factor_name, "Mean")
  if (!is.null(back)) out$`Mean (original unit)` <- fmt_n(back(m$Mean))
  out$N <- formatC(m$N, format = "d"); out$SE <- fmt_n(m$SE)
  if (show_groups) out$Group <- m$Group
  out
}

# significance marks used in agronomy tables: ** p < 0.01, * p < 0.05, ns otherwise
sig_mark <- function(p) ifelse(is.na(p), "", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "ns")))
# the error term (row of the ANOVA table) that a factor is tested against: Error(a) / Error(b) in a split plot
error_of <- function(r, f) {
  nm <- if (!is.null(r$error_terms)) r$error_terms[[f]]   # combined analysis: environments use Rep within Env
        else if (is_split(r$design)) (if (identical(f, r$main)) "Error(a)" else "Error(b)") else "Error"
  r$anova[r$anova$Source == nm, ]
}
# standard error of a mean, standard error of the difference between two means and LSD for one factor, from the
# error mean square of its error term and its number of observations per level (harmonic mean when unequal)
precision_of <- function(r, f) {
  e <- error_of(r, f); n <- r$means[[f]]$N
  n <- length(n) / sum(1 / n)
  sed <- sqrt(2 * e$MS / n)
  c(sem = sqrt(e$MS / n), sed = sed, lsd = stats::qt(1 - r$alpha / 2, e$DF) * sed, df = e$DF)
}
# publication layout (GenStat / ARM style): the means table of one factor followed by Grand mean, CV, the F test with
# its significance mark, SE(m), SED and LSD, so that the table can be pasted into a report as it is
agronomy_display <- function(r, f, m_df) {
  eff <- r$effects[r$effects$Effect == f, ]
  pr <- precision_of(r, f)
  cv_val <- if (is.null(r$cv_a)) sprintf("%.1f", r$cv) else sprintf("%.1f (a) / %.1f (b)", r$cv_a, r$cv_b)
  footer <- data.frame(
    c(paste0("F test (", f, ")"), "Grand mean", "CV (%)", "SE(m)", "SED", sprintf("LSD (%s)", r$alpha)),
    c(if (nrow(eff) && !is.na(eff$F)) paste0(fmt_n(eff$F), sig_mark(eff$P)) else "-",
      fmt_n(r$grand_mean), cv_val, fmt_n(pr[["sem"]]), fmt_n(pr[["sed"]]), fmt_n(pr[["lsd"]])),
    stringsAsFactors = FALSE)
  footer[names(m_df)[-(1:2)]] <- ""
  names(footer) <- names(m_df)
  rbind(m_df, footer)
}
agronomy_note <- function(r) paste0("** significant at p < 0.01, * at p < 0.05, ns not significant. SE(m), SED and LSD use the error mean square that tests each factor",
                                    if (is_rep(r$design)) sprintf(" (Error(a) for %s, Error(b) for %s; those of %s assume sphericity)", r$main, r$sub, r$sub)
                                    else if (is_split(r$design)) " (Error(a) for the main plot, Error(b) for the sub plot)"
                                    else if (identical(r$design, "CombRCBD")) sprintf(" (the pooled error for %s, %s within %s for %s)", r$treatment, r$block, r$env, r$env) else "",
                                    if (!is.null(r$transform)) "; they are on the transformed scale, as are the letters" else "", ".")

# combined analysis: the analysis of each environment on its own, and Bartlett's test of their error variances
by_env_display <- function(r) {
  z <- r$by_env
  data.frame(z[[1]], N = formatC(z$N, format = "d"), Mean = fmt_n(z$Mean), `CV (%)` = fmt_n(z$CV, 1), `Error MS` = fmt_n(z$ErrorMS, 4),
             `Error DF` = formatC(z$ErrorDF, format = "d"), F = paste0(fmt_n(z$F), sig_mark(z$P)), `Pr(>F)` = fmt_p(z$P),
             check.names = FALSE, stringsAsFactors = FALSE) |> setNames(c(r$env, "N", "Mean", "CV (%)", "Error MS", "Error DF", paste("F", r$treatment), "Pr(>F)"))
}
bartlett_note <- function(r) {
  b <- r$bartlett
  if (is.na(b$p)) return(p(class = "text-muted small mb-0", "Bartlett's test could not be computed."))
  ok <- b$p >= r$alpha
  div(class = paste("small mb-0 alert", if (ok) "alert-light border" else "alert-warning"),
      icon(if (ok) "circle-check" else "triangle-exclamation", class = if (ok) "text-success"), " ",
      strong("Homogeneity of error variances (Bartlett): "),
      sprintf("chi-square = %.2f, df = %d, p = %s; largest / smallest error MS = %.1f. ", b$statistic, b$df, fmt_p(b$p), b$ratio),
      if (ok) "The environments can be pooled." else "The error variances differ, so the pooled error of the combined analysis may not be valid; report each environment separately or try a transformation.")
}

# cell means, wide: rows = first factor, columns = second factor
cells_display <- function(cells, f1, f2) {
  lv1 <- unique(cells[[f1]]); lv2 <- unique(cells[[f2]])
  out <- data.frame(lv1, stringsAsFactors = FALSE); names(out) <- f1
  for (b in lv2) out[[paste0(f2, " = ", b)]] <- vapply(lv1, function(a) fmt_n(cells$Mean[cells[[f1]] == a & cells[[f2]] == b]), "")
  out
}

# simple effects, wide: rows = compared factor, columns = levels of the other factor, cells "mean letter"
simple_display <- function(sm) {
  cmp_f <- attr(sm, "compare"); at_f <- attr(sm, "at")
  lv_c <- unique(sm[[cmp_f]]); lv_a <- unique(sm[[at_f]])
  out <- data.frame(lv_c, stringsAsFactors = FALSE); names(out) <- cmp_f
  for (b in lv_a) {
    rows <- sm[sm[[at_f]] == b, ]
    out[[paste0(at_f, " = ", b)]] <- vapply(lv_c, function(a) { r <- rows[rows[[cmp_f]] == a, ]; paste(fmt_n(r$Mean), r$Group) }, "")
  }
  out
}

tsv <- function(df) paste(c(paste(names(df), collapse = "\t"), apply(df, 1, paste, collapse = "\t")), collapse = "\n")

# numbers (also "1,250.50", "<0.0001", "0.419**") are right-aligned, text such as groups and names is left-aligned
html_table <- function(df) {
  num <- vapply(seq_along(df), function(j) {
    v <- as.character(df[[j]]); v <- v[nzchar(v)]
    j > 1 && length(v) > 0 && all(grepl("^[-<]?[0-9][0-9,.]*[*]*$|^[-]?[0-9.]+e[-+][0-9]+$", v))
  }, NA)
  al <- ifelse(num, "text-align:right;", "text-align:left;")
  div(class = "table-responsive",
    tags$table(class = "table table-condensed table-striped", style = "width:auto;",
      tags$thead(tags$tr(lapply(seq_along(df), function(j) tags$th(names(df)[j], style = al[j])))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i)
        tags$tr(lapply(seq_along(df), function(j) tags$td(as.character(df[i, j]), style = al[j], class = if (num[j]) "num")))))))
}

# size and first rows of the data as read, so that the user can check that the file was understood
data_preview_ui <- function(d, n = 10) {
  shown <- head(d, n)
  shown[] <- lapply(shown, function(v) ifelse(is.na(v), "", as.character(v)))
  tagList(p(strong(formatC(nrow(d), format = "d", big.mark = ",")), "rows x", strong(ncol(d)), "columns.",
            if (nrow(d) > n) sprintf("Showing the first %d rows.", n)),
          html_table(shown))
}

# cards of the residual checks, each with its traffic light: green met, yellow a mild departure the analysis is
# robust to, red a clear violation (see star_assumption_lights); without lights, passed is green and failed red.
# advice = FALSE for designs that have no recommendation on the Results tab (split plots)
diag_summary_cards <- function(dg, lights = NULL, advice = TRUE) {
  if (is.null(dg)) return(NULL)
  light_of <- function(check, ok) {
    l <- if (!is.null(lights)) lights$Light[lights$Check == check] else character()
    if (length(l) && !is.na(l)) l else if (isTRUE(ok)) "green" else if (isFALSE(ok)) "red" else NA_character_
  }
  badge <- function(l, txt) span(class = paste("badge", switch(l %||% "none", green = "bg-success", yellow = "bg-warning text-dark", red = "bg-danger", "bg-secondary")), txt)
  nl <- light_of("normality", dg$normality$ok)
  norm_badge <- if (is.na(nl)) badge(NA, "Not tested") else badge(nl, sprintf("%s (p = %s)", c(green = "Passed", yellow = "Mild, robust", red = "Departure")[[nl]], fmt_p(dg$normality$p)))
  norm_desc <- if (is.na(nl)) "Too few observations to test" else c(
    green = "No evidence that the residuals depart from a normal distribution",
    yellow = "The residuals depart from normal, but with 30 or more per group and a moderate shape the analysis is robust",
    red = if (advice) "The residuals clearly depart from normality: see the recommendation on the Results tab"
          else "The residuals clearly depart from normality: check the flagged values, or try a transformation (Advanced options)")[[nl]]
  has_homo <- !is.null(dg$homogeneity)
  vl <- if (has_homo) light_of("variance", dg$homogeneity$ok) else NA_character_
  homo_badge <- if (is.na(vl)) badge(NA, if (has_homo) "Not tested" else "Not applicable")
                else badge(vl, sprintf("%s (p = %s)", c(green = "Equal", yellow = "Moderately unequal", red = "Unequal")[[vl]], fmt_p(dg$homogeneity$p)))
  homo_desc <- if (is.na(vl)) "Too few observations per treatment to test" else c(
    green = "Variances are similar across treatment groups",
    yellow = "Variances differ moderately (largest/smallest at most 4) with equal group sizes, which the F test tolerates",
    red = if (advice) "Variances clearly differ across groups: Welch's ANOVA (CRD) or a transformation"
          else "Variances clearly differ across groups: try a transformation (Advanced options)")[[vl]]
  n_out <- dg$n_outliers %||% 0; n_sus <- dg$n_suspect %||% 0
  ol <- light_of("outliers", n_out == 0)
  out_badge <- badge(ol, if (n_out > 0) sprintf("%d outlier(s)", n_out) else if (n_sus > 0) sprintf("%d suspect", n_sus) else "None detected")
  out_desc <- if (n_out > 0) "Studentized residuals beyond |3|: check them for errors" else if (n_sus > 0) "Studentized residuals between |2| and |3|: worth a look" else "No studentized residuals exceed |3|"
  card <- function(title, b, desc, cls) div(class = cls,
    div(class = "card h-100 p-3 bg-white border shadow-none", style = "border-color: #e5ebe5 !important;",
      div(class = "d-flex justify-content-between align-items-center mb-1", strong(title), b),
      p(class = "text-muted small mb-0", desc)))
  cls <- if (has_homo) "col-md-4" else "col-md-6"
  div(class = "row g-3 mb-3",
    card("Residual normality", norm_badge, norm_desc, cls),
    if (has_homo) card("Variance equality", homo_badge, homo_desc, cls),
    card("Outliers", out_badge, out_desc, cls))
}
# the layout the analyses expect: one row per plot, the replication in its own column (long format)
long_format_help <- function() help_box("How should the file be laid out?",
  p("One row per plot (experimental unit), one column per variable. The replication or block is a column of its own, and all the values of the response are in one column (", strong("long format", .noWS = "outside"), "):"),
  html_table(data.frame(Variety = c("RD41", "RD41", "...", "RD47"), Rep = c("1", "2", "...", "1"), Yield = c("5557", "5706", "...", "4652"), check.names = FALSE)),
  p("Not one column per replication (", strong("wide format", .noWS = "outside"), "), as often typed in Excel:"),
  html_table(data.frame(Variety = c("RD41", "RD47"), `Rep 1` = c("5557", "4652"), `Rep 2` = c("5706", "4965"), `Rep 3` = c("...", "..."), check.names = FALSE)),
  p("A file in wide format with columns named like Rep 1, Rep 2 or ซ้ำที่ 1, ซ้ำที่ 2 is recognised, and the app offers to convert it."))
# which language to type the file in: either works; the names in the file are what the plots show
file_language_help <- function() help_box("ภาษาในไฟล์ข้อมูล",
  p("ใช้ภาษาไทยหรืออังกฤษก็ได้ ทั้งชื่อคอลัมน์และชื่อทรีตเมนต์ แอปอ่านและแสดงผลได้ทั้งสองภาษา"),
  p("ชื่อในไฟล์จะขึ้นบนแกนและในคำอธิบายของกราฟตามที่พิมพ์ไว้ ถ้าจะนำกราฟไปลงวารสารภาษาอังกฤษ ควรตั้งชื่อเป็นภาษาอังกฤษตั้งแต่ในไฟล์ ",
    "(ในแอปเปลี่ยนได้เฉพาะชื่อแกนและชื่อกราฟ ไม่ได้เปลี่ยนชื่อทรีตเมนต์)"),
  p("ชื่อสั้น ๆ จะอ่านง่ายกว่าในกราฟขนาดเล็ก เช่น RD41 แทน พันธุ์ข้าว กข41"))
# columns of a wide-format file that each hold one replication: named Rep1, R 2, Block_3, ซ้ำที่ 1, ... and holding numbers
wide_rep_cols <- function(d) {
  rx <- "^\\s*(rep(lication)?|r|block|blk|b|ซ้ำ(ที่)?|บล็อก(ที่)?|บล็อค(ที่)?)\\s*[._#-]?\\s*[0-9]+\\s*$"
  cols <- names(d)[grepl(rx, names(d), ignore.case = TRUE, perl = TRUE)]
  cols <- cols[vapply(cols, function(cc) { v <- d[[cc]][!star_is_blank(d[[cc]])]; length(v) > 0 && mean(!is.na(star_parse_number(v))) > 0.5 }, NA)]
  if (length(cols) >= 2 && length(cols) < ncol(d)) cols else character()
}
# first choices of the variables for an uploaded file, from the column names rather than their order (a file laid
# out Variety, Rep, Yield must not get Variety as the block): a column named like Rep, Block, ซ้ำ or บล็อก is the
# block, one named like Site, Season, Year, สถานที่ or ฤดู the environment, and the treatments are the other columns
# that hold labels (text, or numbers with few distinct values such as rates), in file order (main plot before sub
# plot); never the response
guess_roles <- function(d, resp) {
  nm <- names(d)
  match1 <- function(rx) nm[grepl(rx, trimws(nm), ignore.case = TRUE, perl = TRUE) & nm != resp][1]
  blk <- match1("^(rep(lication)?s?|block|blocks|blk|ซ้ำ(ที่)?|บล็อ[กค](ที่)?)\\s*(no\\.?|number|#)?$")
  # a bare R or B only when nothing above matched and the columns are not factors named A, B, ...
  if (is.na(blk) && !any(toupper(trimws(nm)) == "A")) blk <- match1("^[rb]$")
  env <- match1("^(env(ironment)?|site|location|loc|year|season|trial|สถานที่|ปี|ฤดู(กาล)?|ฤดูปลูก|จังหวัด)$")
  rest <- setdiff(nm, c(resp, blk, env))
  num <- numeric_cols(d)
  few <- vapply(rest, function(cc) length(unique(d[[cc]][!star_is_blank(d[[cc]])])) <= max(2, nrow(d) %/% 2), NA)
  trt <- rest[!rest %in% num | few]
  # no column is named like a block: one numbered 1, 2, ..., k is most likely the replication
  if (is.na(blk)) for (cc in intersect(trt, num)) {
    v <- sort(unique(star_parse_number(d[[cc]][!star_is_blank(d[[cc]])])))
    if (length(v) >= 2 && all(v == seq_along(v))) { blk <- cc; trt <- setdiff(trt, cc); break }
  }
  if (!length(trt)) trt <- c(rest, stats::na.omit(env))
  if (is.na(blk)) blk <- setdiff(nm, c(resp, trt[1], env))[1]
  list(blk = if (is.na(blk)) nm[1] else blk, env = if (is.na(env)) nm[1] else env,
       trt = if (length(trt)) trt else nm[min(2, length(nm))])
}
# wide -> long: the replication columns are stacked into a Rep column (their number) and one value column
wide_to_long <- function(d, cols, value = "Value") {
  ids <- setdiff(names(d), cols)
  rep_name <- make.unique(c(ids, "Rep"))[length(ids) + 1]
  value <- make.unique(c(ids, rep_name, value))[length(ids) + 2]
  out <- do.call(rbind, lapply(cols, function(cc) {
    x <- d[ids]; x[[rep_name]] <- sub("^\\D*([0-9]+)\\D*$", "\\1", cc); x[[value]] <- d[[cc]]; x
  }))
  rownames(out) <- NULL
  out[c(ids, rep_name, value)]
}
# split plot: what the chosen block / replication column means for the layout, before the analysis is run, so that a
# main-plot number chosen as a block (or the reverse) is caught and the degrees of freedom of Error(a) are visible
split_layout_check <- function(d, design, main, sub, blk) {
  if (length(unique(c(main, sub, blk))) < 3) return(div(class = "alert alert-warning small", "Choose three different columns for the main-plot factor, the sub-plot factor and the ", if (design == "SplitCRD") "replication." else "block."))
  keep <- !star_is_blank(d[[main]]) & !star_is_blank(d[[sub]]) & !star_is_blank(d[[blk]])
  m <- star_squash_space(as.character(d[[main]][keep])); s <- star_squash_space(as.character(d[[sub]][keep])); b <- star_squash_space(as.character(d[[blk]][keep]))
  a <- length(unique(m)); r <- length(unique(b)); nb <- length(unique(s))
  mains_per_b <- tapply(m, b, function(v) length(unique(v)))
  rows_per_wp <- table(b, m); rows_per_wp <- rows_per_wp[rows_per_wp > 0]
  cnt <- table(b, m, s)
  bad <- function(...) div(class = "alert alert-warning small mb-2", icon("triangle-exclamation"), " ", ...)
  if (all(rows_per_wp == 1)) return(bad(strong(blk), " gives a different value to every row within each level of ", main, ", so it numbers the sub plots, not the ", if (design == "SplitCRD") "main plots." else "blocks."))
  if (r > 1 && all(mains_per_b == 1)) return(bad(strong(blk), sprintf(" has %d values and each belongs to a single level of %s, so it numbers the main plots across the whole trial", r, main),
    if (design == "SplitRCBD") tagList(" rather than grouping them into blocks. If the main plots were not laid out in blocks, choose ", strong("Split plot in CRD"), "; otherwise choose the column that holds the block.")
    else tagList(". Renumber the main plots from 1 within each level of ", main, " (1, 2, 3, ... for each level) so that each number appears once with every level.")))
  if (any(cnt != 1)) return(bad(sprintf("The layout is not balanced: %d of the %d %s x %s x %s combinations are missing and %d appear more than once. Check that %s is the %s column.",
                                        sum(cnt == 0), length(cnt), blk, main, sub, sum(cnt > 1), blk, if (design == "SplitCRD") "replication" else "block")))
  df_rcbd <- (r - 1) * (a - 1); df_crd <- a * (r - 1)
  div(class = "alert alert-light border small mb-2",
    icon("circle-check", class = "text-success"), " ",
    sprintf("%d %s x %d levels of %s = %d main plots, each split into %d sub plots. ", r, if (design == "SplitCRD") "replications" else "blocks", a, main, a * r, nb),
    strong(sprintf("Error(a) has %d df", if (design == "SplitCRD") df_crd else df_rcbd)),
    if (design == "SplitCRD") sprintf(" = %d x (%d - 1). If each value of %s is a block holding one main plot of every level side by side, choose Split plot in RCBD instead (Error(a) %d df).", a, r, blk, df_rcbd)
    else sprintf(" = (%d - 1) x (%d - 1). If the main plots were not grouped into blocks and %s only numbers them, choose Split plot in CRD instead (Error(a) %d df).", r, a, blk, df_crd))
}
# repeated measures: the plot column with the treatment must identify the plots, and each plot needs one value per time
rep_layout_check <- function(d, design, trt, time, blk) {
  crd <- design == "RepCRD"
  if (length(unique(c(trt, time, blk))) < 3) return(div(class = "alert alert-warning small", "Choose three different columns for the treatment, the time and the ", if (crd) "plot number." else "block."))
  keep <- !star_is_blank(d[[trt]]) & !star_is_blank(d[[time]]) & !star_is_blank(d[[blk]])
  m <- star_squash_space(as.character(d[[trt]][keep])); s <- star_squash_space(as.character(d[[time]][keep])); b <- star_squash_space(as.character(d[[blk]][keep]))
  a <- length(unique(m)); r <- length(unique(b)); nt <- length(unique(s))
  rows_per_plot <- table(b, m); rows_per_plot <- rows_per_plot[rows_per_plot > 0]
  cnt <- table(b, m, s)
  bad <- function(...) div(class = "alert alert-warning small mb-2", icon("triangle-exclamation"), " ", ...)
  if (all(rows_per_plot == 1)) return(bad(strong(blk), " gives a different value to every row within each treatment, so it does not say which plot was measured at each time. Choose the column that numbers the ", if (crd) "plots (pots) within each treatment." else "blocks."))
  if (r > 1 && all(tapply(m, b, function(v) length(unique(v))) == 1)) return(bad(strong(blk), sprintf(" has %d values and each belongs to a single treatment, so it numbers the plots across the whole trial", r),
    if (crd) tagList(". Renumber the plots from 1 within each treatment (1, 2, 3, ... for each) so that each number appears once with every treatment.")
    else tagList(" rather than grouping them into blocks. If the plots were not laid out in blocks, choose ", strong("Repeated measures over time: CRD"), ".")))
  if (any(cnt != 1)) return(bad(sprintf("Not every plot has exactly one value at each time: %d of the %d %s x %s x %s combinations are missing and %d appear more than once. Check that %s is the %s column; plots with a missing measurement must be removed or filled in.",
                                        sum(cnt == 0), length(cnt), blk, trt, time, sum(cnt > 1), blk, if (crd) "plot" else "block")))
  div(class = "alert alert-light border small mb-2",
    icon("circle-check", class = "text-success"), " ",
    sprintf("%d %s x %d treatments = %d plots, each measured at %d times. ", r, if (crd) "plots per treatment" else "blocks", a, a * r, nt),
    strong(sprintf("Error(a) has %d df.", if (crd) a * (r - 1) else (r - 1) * (a - 1))),
    if (nt > 2 && a * (r - 1) < nt - 1) sprintf(" With %d plots per treatment and %d times there are too few plots to test sphericity, so the Greenhouse-Geisser correction will be used.", r, nt))
}
# repeated measures: the within-plot tests with and without the sphericity correction
sphericity_display <- function(r) {
  s <- r$sphericity; z <- s$tests
  out <- data.frame(Effect = sub(":", " x ", z$Effect), F = fmt_n(z$F), DF = sprintf("%d, %d", as.integer(z$DF1), as.integer(z$DF2)),
                    `p (sphericity assumed)` = fmt_p(z$P), `p (Greenhouse-Geisser)` = fmt_p(z$P_GG), `p (Huynh-Feldt)` = fmt_p(z$P_HF),
                    check.names = FALSE, stringsAsFactors = FALSE)
  out$`p used` <- paste0(fmt_p(z$P_used), sig_mark(z$P_used))
  out
}
sphericity_note <- function(r) {
  s <- r$sphericity; m <- s$mauchly
  warn <- s$correction != "none"
  div(class = paste("small mb-0 alert", if (warn) "alert-warning" else "alert-light border"),
      icon(if (warn) "triangle-exclamation" else "circle-check", class = if (!warn) "text-success"), " ",
      strong("Sphericity: "), .star_sphericity_text(s, r$alpha),
      if (s$n_times > 2) sprintf(" Mauchly's test has little power with few plots, so look at the epsilons too: 1 means sphericity holds, %s is the lowest possible value.", fmt_n(1 / (s$n_times - 1), 3)),
      " The corrected p values decide the letters; the means of ", r$sub, " are compared with Error(b), which assumes sphericity.")
}
sphericity_sheet <- function(r) {
  s <- r$sphericity; z <- s$tests
  data.frame(z[c("Effect", "F", "DF1", "DF2", "P", "P_GG", "P_HF", "P_used")],
             `Mauchly W` = c(s$mauchly$W, NA), `Mauchly p` = c(s$mauchly$p, NA), `GG epsilon` = c(s$gg, NA), `HF epsilon` = c(s$hf, NA),
             Correction = c(s$correction, NA), check.names = FALSE, stringsAsFactors = FALSE)
}
# why a means table has no letters (protected test: means are compared only when the F test is significant)
no_letters_note <- function(r, f) {
  e <- r$effects[r$effects$Effect == f, ]
  why <- if (isTRUE(e$Significant) && isTRUE(r$interaction_significant))
    sprintf("the %s x %s interaction is significant, so the levels of %s are compared within each level of the other factor (tables below) instead of overall.", r$factors[1], r$factors[2], f)
  else sprintf("the F test for %s is not significant (p = %s, not below %s), so its means are not shown to differ. This is the protected test: letters are given only after a significant F test.", f, fmt_p(e$P), format(r$alpha))
  p(class = "text-muted small", icon("circle-info"), " No letters: ", why)
}
# shown while nothing has been run yet
run_hint <- function(button) div(class = "empty-state", icon("chart-simple"),
  h5("No results yet"), p(sprintf("Choose your data and variables, then press %s.", button)),
  p(class = "small", "Each screen starts with a built-in example, so you can press it right away to see what you get."))

# sidebar parts: numbered steps, options folded away, and a Run button that stays in view
side_step <- function(n, title, ...) div(class = "side-step", div(class = "side-step-title", span(class = "num", n), title), ...)
advanced_box <- function(...) tags$details(class = "advanced", tags$summary(icon("sliders"), " Advanced options"), div(class = "advanced-body", ...))
run_bar <- function(id, label) div(class = "run-bar", actionButton(id, tagList(icon("play"), " ", label), class = "btn-primary w-100"))

# results: a row of key numbers, then each table in a card with its copy button in the header
key_stats <- function(...) div(class = "key-stats", lapply(Filter(Negate(is.null), list(...)), function(s)
  div(class = "key-stat", div(class = "label", s$label), div(class = "value", s$value), if (!is.null(s$note)) div(class = "note", s$note))))
stat <- function(label, value, note = NULL) list(label = label, value = value, note = note)
result_card <- function(title, ..., copy = NULL, copy_label = "Copy") bslib::card(class = "result-card",
  bslib::card_header(class = "d-flex justify-content-between align-items-center", span(title),
                     if (!is.null(copy)) actionButton(copy, tagList(icon("copy"), " ", copy_label), class = "btn-sm")),
  bslib::card_body(...))
# the Thai summary card, with a reminder that the text is generated and must be read against the real context of the data
thai_card <- function(text, copy) result_card("สรุปผล (ภาษาไทย)", p(lang = "th", text),
  p(lang = "th", class = "text-muted small mb-0 mt-2", strong("ข้อเสนอแนะ:"), " สรุปนี้สร้างจากโปรแกรม ผู้วิจัยควรนำไปแปลความหมายด้วยความรอบคอบ โดยพิจารณาร่วมกับบริบทและสภาพแวดล้อมที่แท้จริงของข้อมูล"),
  copy = copy, copy_label = "คัดลอก")
# what the data checks suggest (from star_recommend_aov / star_recommend_tt), as advice the analyst may follow or not:
# the traffic light of the assumption checks, an observation, a recommendation, and a button for each method offered
# (on red, one per reporting goal: ranks or medians, or means without the normal curve) plus one to keep the current
# method. Buttons are <prefix>_<method id> and <prefix>_stay. `current` is the method shown, `kept` TRUE once the user
# chose to keep it, `current_label` names it.
advice_ui <- function(rc, current, prefix, kept, current_label) {
  if (is.null(rc)) return(NULL)
  cls <- paste("alert", c(green = "alert-success", yellow = "alert-warning", red = "alert-danger")[[rc$light]])
  title <- div(span(class = paste0("light-dot light-", rc$light)),
               strong(c(green = "Assumptions met", yellow = "Mild departure: the analysis is robust", red = "Clear violation: another method is advised")[[rc$light]]))
  obs <- div(class = "mt-1", icon("magnifying-glass"), " ", strong("Observation: "), rc$observed)
  ids <- vapply(rc$options, function(o) o$id, "")
  if (current %in% ids) return(div(class = paste(cls, "small"), title, obs,
    div(class = "mt-1", icon("circle-check"), sprintf(" You are using %s, a recommended method; the choice remains yours.", current_label))))
  rec <- div(class = "mt-1", icon("lightbulb"), " ", strong("Recommendation: "), rc$advice)
  if (!length(ids)) return(div(class = paste(cls, "small"), title, obs, rec))
  if (isTRUE(kept)) return(div(class = paste(cls, "small"), title, obs, div(class = "mt-1", icon("circle-info"), sprintf(" You chose to continue with %s.", current_label))))
  opts <- lapply(seq_along(rc$options), function(i) { o <- rc$options[[i]]
    div(class = "mt-2 d-flex flex-wrap align-items-center gap-2", if (!is.null(o$goal)) span(paste0(o$goal, ":")),
        actionButton(paste0(prefix, "_", o$id), o$label, class = if (i == 1) "btn-light btn-sm border" else "btn-light btn-sm border")) })
  div(class = cls, title, obs, rec, opts,
      div(class = "mt-2", actionButton(paste0(prefix, "_stay"), sprintf("Continue with %s", current_label), class = "btn-light btn-sm border")),
      div(class = "small text-muted mt-1", "Advice only: the analyst decides which method to report."))
}
# the R code that repeats an analysis, folded under a one-line heading: most users never need it, and those who want
# to check the results or rerun them in R open it. `missing` is shown instead when there is no code for the analysis.
code_card <- function(code, copy, missing = NULL) tags$details(class = "code-card",
  tags$summary(icon("code"), " R code to repeat this analysis",
               span(class = "code-card-hint", "for checking the results or running them again in R")),
  div(class = "code-card-body",
      if (is.null(code)) p(class = "text-muted mb-0", missing)
      else tagList(div(class = "text-end mb-2", actionButton(copy, tagList(icon("copy"), " Copy code"), class = "btn-sm")),
                   tags$pre(class = "small mb-0", style = "white-space: pre-wrap;", code))))
# settings changed after the last run: the results on the screen no longer match the controls
# outputs that show results; greyed out and made inert (no clicks, copying or downloading) while the results are stale
stale_dim <- function(tag) tagAppendAttributes(tag, class = "stale-dim")
stale_js <- "
Shiny.addCustomMessageHandler('stale', function(msg) {
  var el = document.getElementById(msg.id); if (!el) return;
  el.classList.toggle('is-stale', msg.on);
  el.querySelectorAll('.stale-dim').forEach(function(x) { if (msg.on) x.setAttribute('inert', ''); else x.removeAttribute('inert'); });
});"
# Download buttons on a tab that was not open yet (the Plot tabs) can miss the download address in Shinylive, so they
# stay disabled with no href, and a click saves the app page as .html. The address is in Shiny's received values:
# put it on the button when its tab opens and when it is clicked; with no address yet, block the click.
download_fix_js <- "
(function() {
  function fix(a) {
    var app = window.Shiny && Shiny.shinyapp; if (!app || !a.id) return;
    var url = app.$values && app.$values[a.id];
    if (url && a.getAttribute('href') !== url) { a.setAttribute('href', url); a.classList.remove('disabled'); a.removeAttribute('aria-disabled'); a.removeAttribute('tabindex'); }
  }
  function fixAll(root) { (root || document).querySelectorAll('a.shiny-download-link').forEach(fix); }
  document.addEventListener('shown.bs.tab', function() { fixAll(); });
  document.addEventListener('shiny:value', function() { setTimeout(fixAll, 0); });
  document.addEventListener('shiny:visualchange', function() { fixAll(); });
  document.addEventListener('click', function(e) {
    var a = e.target.closest && e.target.closest('a.shiny-download-link'); if (!a) return;
    fix(a);
    var h = a.getAttribute('href');
    if (!h || h === '#' ) { e.preventDefault(); e.stopPropagation(); if (window.Shiny) Shiny.notifications && Shiny.notifications.show({ html: 'The file is not ready yet. Please wait a moment and click again.', type: 'warning', duration: 4000 }); }
  }, true);
})();"
# the results below are greyed out and cannot be copied or downloaded until the analysis is run again (see stale_js)
stale_note <- function(button) div(class = "alert alert-warning d-flex justify-content-between align-items-center gap-2",
  span(icon("triangle-exclamation"), " ", strong("The settings have changed since the last run."),
       " The results below are from the earlier settings, so they are greyed out and copying and downloading are paused."),
  tags$button(type = "button", class = "btn btn-warning btn-sm flex-none", onclick = sprintf("document.getElementById('%s').click()", button), icon("rotate-right"), " Run again"))
error_box <- function(msg) div(class = "alert alert-danger d-flex justify-content-between align-items-center gap-2",
  div(icon("triangle-exclamation", class = "me-2"), strong("พบข้อผิดพลาด: "), msg),
  tags$button(type = "button", class = "btn btn-sm btn-outline-danger bg-white flex-none", onclick = "var el = document.getElementById('open_feedback'); if (el) el.click();", icon("comment-dots"), " แจ้งปัญหานี้"))

# columns that hold numbers (also numbers stored as text such as "1,250.5"); candidates for correlation / regression
numeric_cols <- function(d) names(d)[vapply(d, function(v) {
  shown <- !star_is_blank(v)
  sum(shown) > 0 && mean(!is.na(star_parse_number(v[shown]))) > 0.5
}, NA)]
fmt_sig <- function(x, digits = 4) ifelse(is.na(x), "", formatC(x, format = "g", digits = digits))

corr_matrix_display <- function(r) {
  k <- length(r$vars)
  m <- matrix("", k, k, dimnames = list(r$vars, r$vars))
  for (i in seq_len(k)) for (j in seq_len(k)) {
    m[i, j] <- if (i == j) "1" else if (is.na(r$r[i, j])) "" else paste0(formatC(r$r[i, j], format = "f", digits = 3), .star_stars(r$p[i, j], r$alpha))
  }
  data.frame(Variable = r$vars, m, check.names = FALSE, stringsAsFactors = FALSE)
}
corr_pairs_display <- function(r) {
  z <- r$pairs
  out <- data.frame(`Variable 1` = z$Variable1, `Variable 2` = z$Variable2, r = fmt_n(z$r, 3), N = formatC(z$N, format = "d"),
                    p = fmt_p(z$PAdj), `CI lower` = fmt_n(z$Lower, 3), `CI upper` = fmt_n(z$Upper, 3), Strength = z$Strength,
                    Significant = ifelse(z$Significant, "yes", "no"), check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[3] <- r$symbol
  if (r$adjust != "none") names(out)[5] <- paste0("p (", r$adjust, ")")
  if (r$method != "pearson") out[c("CI lower", "CI upper")] <- NULL
  out
}
reg_fit_display <- function(r) {
  f <- r$fit
  data.frame(Item = c("Observations", "R-squared", "Adjusted R-squared", "Root MSE", paste("Mean of", r$response), "CV (%)", "AIC"),
             Value = c(formatC(f$n, format = "d"), fmt_n(f$r2, 4), fmt_n(f$adj_r2, 4), fmt_sig(f$rmse), fmt_sig(f$mean_y), fmt_n(f$cv, 2), fmt_n(f$aic, 2)),
             stringsAsFactors = FALSE)
}
reg_anova_display <- function(r) {
  a <- r$anova
  data.frame(Source = a$Source, DF = formatC(a$DF, format = "d"), `Sum of squares` = fmt_sig(a$SS, 6), `Mean square` = fmt_sig(a$MS, 6),
             `F value` = fmt_n(a$F), `Pr(>F)` = fmt_p(a$P), check.names = FALSE, stringsAsFactors = FALSE)
}
reg_coef_display <- function(r) {
  z <- r$coefficients
  out <- data.frame(Term = z$Term, Estimate = fmt_sig(z$Estimate), `Std. error` = fmt_sig(z$SE), `t value` = fmt_n(z$t), `Pr(>|t|)` = fmt_p(z$P),
                    Lower = fmt_sig(z$Lower), Upper = fmt_sig(z$Upper), `Std. beta` = fmt_n(z$StdBeta, 3), VIF = fmt_n(z$VIF),
                    check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[6:7] <- paste0(c("Lower ", "Upper "), format(100 * (1 - r$alpha)), "% CI")
  out[, vapply(out, function(v) any(nzchar(v)), NA), drop = FALSE]
}

corr_examples <- c("Car data: mileage, weight, power (mtcars)" = "cars", "Rice plant traits and yield" = "rice")
corr_setup <- list(cars = list(file = "example_cars.csv", vars = c("mpg", "hp", "wt", "qsec")),
                   rice = list(file = "example_rice.csv", vars = c("PP", "PH", "NGL", "Yield")))
reg_examples <- c("Fuel use vs weight and power (multiple)" = "cars_multi", "Fuel use vs weight (simple)" = "cars_simple",
                  "Rice yield vs plant traits (multiple)" = "rice", "Grain yield vs seeding rate (quadratic)" = "seeding")
reg_setup <- list(cars_multi = list(file = "example_cars.csv", resp = "mpg", pred = c("wt", "hp"), degree = "1"),
                  cars_simple = list(file = "example_cars.csv", resp = "mpg", pred = "wt", degree = "1"),
                  rice = list(file = "example_rice.csv", resp = "Yield", pred = c("PP", "PH", "NGL"), degree = "1"),
                  seeding = list(file = "example_seeding.csv", resp = "GrainYield", pred = "SeedingRate", degree = "2"))

