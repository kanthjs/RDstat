# Results in other forms: a summary in Thai and a report that opens in Microsoft Word.
# Sourced by app.R (needs fmt_n, fmt_p, transforms, is_comb, is_rep).

# ---- summary in Thai ---------------------------------------------------------------------------------------------
th_p <- function(p) if (is.na(p)) "p = NA" else if (p < 0.0001) "p < 0.0001" else paste("p =", formatC(p, format = "f", digits = 4))
th_f <- function(r, f) { e <- r$effects[r$effects$Effect == f, ]; sprintf("F = %s, %s", fmt_n(e$F), th_p(e$P)) }
th_test <- c(LSD = "LSD", HSD = "Tukey HSD", duncan = "Duncan (DMRT)", SNK = "SNK", scheffe = "Scheffe", bonferroni = "Bonferroni")
th_best <- function(m) sprintf("%s (%s)", m$Treatment[which.max(m$Mean)], fmt_n(max(m$Mean)))

thai_summary_aov <- function(r) {
  y <- r$original_response %||% r$response
  fs <- r$factors
  out <- character()
  if (!is.null(r$transform)) out <- c(out, sprintf("วิเคราะห์ข้อมูล %s ที่แปลงด้วย %s ค่าเฉลี่ยและการเปรียบเทียบอยู่ในสเกลที่แปลงแล้ว", y, transforms[[r$transform]]$label))
  main_sentence <- function(f) {
    e <- r$effects[r$effects$Effect == f, ]
    if (isTRUE(e$Significant))
      sprintf("%s มีผลต่อ %s อย่างมีนัยสำคัญทางสถิติ (%s) ค่าเฉลี่ยสูงสุดคือ %s ค่าเฉลี่ยที่มีอักษรเหมือนกันไม่แตกต่างกันทางสถิติ (เปรียบเทียบด้วยวิธี %s)",
              f, y, th_f(r, f), th_best(r$means[[f]]), th_test[[r$tests_used[[f]]]] %||% r$tests_used[[f]])
    else sprintf("%s ไม่มีผลต่อ %s อย่างมีนัยสำคัญทางสถิติ (%s)", f, y, th_f(r, f))
  }
  if (length(fs) == 1) {
    out <- c(out, main_sentence(fs))
  } else {
    inter <- paste(fs, collapse = ":")
    if (isTRUE(r$interaction_significant)) {
      out <- c(out, sprintf("ปฏิสัมพันธ์ระหว่าง %s และ %s มีนัยสำคัญทางสถิติ (%s) แสดงว่าผลของปัจจัยหนึ่งขึ้นอยู่กับระดับของอีกปัจจัยหนึ่ง จึงเปรียบเทียบค่าเฉลี่ยภายในแต่ละระดับ (simple effects) แทนค่าเฉลี่ยรวม",
                            fs[1], fs[2], th_f(r, inter)))
      cl <- r$cells[which.max(r$cells$Mean), ]
      out <- c(out, sprintf("ค่าเฉลี่ยสูงสุดคือ %s = %s และ %s = %s (%s)", fs[1], cl[[fs[1]]], fs[2], cl[[fs[2]]], fmt_n(cl$Mean)))
    } else {
      out <- c(out, sprintf("ปฏิสัมพันธ์ระหว่าง %s และ %s ไม่มีนัยสำคัญทางสถิติ (%s)", fs[1], fs[2], th_f(r, inter)), vapply(fs, main_sentence, ""))
    }
  }
  out <- c(out, if (is.null(r$cv_a)) sprintf("ค่าสัมประสิทธิ์ความแปรปรวน (CV) = %.1f%%", r$cv)
                else sprintf("ค่าสัมประสิทธิ์ความแปรปรวน CV(a) = %.1f%% และ CV(b) = %.1f%%", r$cv_a, r$cv_b))
  if (is_rep(r$design)) {
    sp <- r$sphericity; m <- sp$mauchly
    cor_th <- c(GG = "Greenhouse-Geisser", HF = "Huynh-Feldt")
    out <- c(out, if (sp$n_times == 2) "วัดซ้ำเพียงสองครั้ง จึงไม่ต้องทดสอบ sphericity"
      else if (sp$correction == "none") sprintf("ทดสอบ sphericity ด้วย Mauchly's test (W = %.3f, %s) ไม่ปฏิเสธสมมติฐาน จึงใช้ F test ตามปกติสำหรับ %s และปฏิสัมพันธ์", m$W, th_p(m$p), r$sub)
      else sprintf("%s จึงปรับองศาอิสระของการทดสอบ %s และปฏิสัมพันธ์ด้วยวิธี %s (epsilon = %.3f) ค่า p ข้างต้นเป็นค่าที่ปรับแล้ว",
                   if (is.na(m$p)) "จำนวนแปลงน้อยเกินกว่าจะทดสอบ sphericity ได้" else sprintf("Mauchly's test พบว่าข้อมูลไม่เป็นไปตาม sphericity (W = %.3f, %s)", m$W, th_p(m$p)),
                   r$sub, cor_th[[sp$correction]], sp$epsilon))
  }
  if (is_comb(r$design) && !is.na(r$bartlett$p)) {
    out <- c(out, if (r$bartlett$p >= r$alpha) sprintf("ความแปรปรวนของความคลาดเคลื่อนในแต่ละ %s ไม่แตกต่างกัน (Bartlett, %s) จึงรวมวิเคราะห์ได้", r$env, th_p(r$bartlett$p))
                  else sprintf("ความแปรปรวนของความคลาดเคลื่อนในแต่ละ %s แตกต่างกัน (Bartlett, %s) การรวมวิเคราะห์อาจไม่เหมาะสม ควรรายงานผลแยกแต่ละ %s หรือแปลงข้อมูล", r$env, th_p(r$bartlett$p), r$env))
  }
  # the assumption checks are left to the analyst (Outliers & assumptions tab), not judged in the summary
  paste(out, collapse = " ")
}

thai_summary_tt <- function(r) {
  tr <- r$test[r$chosen, ]
  sig <- r$significant
  ci <- sprintf("ช่วงความเชื่อมั่น %g%% ของผลต่าง %s ถึง %s", 100 * r$conf_level, fmt_n(tr$Lower), fmt_n(tr$Upper))
  st <- sprintf("t = %.2f, df = %s, %s", tr$t, .star_tt_df(tr$DF), th_p(tr$P))
  main <- switch(r$type,
    independent = sprintf("ค่าเฉลี่ย %s ของ %s (%s) %sกับ %s (%s) ผลต่าง = %s (%s; %s)", r$response, r$labels[1], fmt_n(r$groups$Mean[1]),
                          if (sig) "แตกต่างอย่างมีนัยสำคัญทางสถิติ" else "ไม่แตกต่างอย่างมีนัยสำคัญทางสถิติ", r$labels[2], fmt_n(r$groups$Mean[2]), fmt_n(tr$Difference), st, ci),
    paired = sprintf("ค่าเฉลี่ยของ %s (%s) และ %s (%s) %s ผลต่างเฉลี่ย = %s (%s; %s)", r$x, fmt_n(r$groups$Mean[1]), r$y, fmt_n(r$groups$Mean[2]),
                     if (sig) "แตกต่างกันอย่างมีนัยสำคัญทางสถิติ" else "ไม่แตกต่างกันอย่างมีนัยสำคัญทางสถิติ", fmt_n(tr$Difference), st, ci),
    one = sprintf("ค่าเฉลี่ย %s (%s) %sจากค่า %s (%s; %s)", r$response, fmt_n(r$groups$Mean[1]),
                  if (sig) "แตกต่างอย่างมีนัยสำคัญทางสถิติ" else "ไม่แตกต่างอย่างมีนัยสำคัญทางสถิติ", format(r$mu), st, ci))
  size <- c(negligible = "น้อยมาก", small = "เล็ก", medium = "ปานกลาง", large = "ใหญ่")[[.star_tt_effect_word(r$effect_size)]]
  paste0(main, sprintf(" ขนาดอิทธิพล Cohen's d = %.2f (%s)", r$effect_size, size))
}

thai_summary_np <- function(np) {
  m <- np$means; top <- m[which.max(m$Median), ]
  st <- sprintf("χ² = %.2f, df = %d, %s", np$statistic, np$df, th_p(np$p))
  how <- if (is.null(np$block)) "การทดสอบ Kruskal-Wallis" else sprintf("การทดสอบ Friedman (จัดอันดับภายในแต่ละ %s)", np$block)
  if (np$significant)
    sprintf("%s พบว่า %s ของ %s แตกต่างกันอย่างมีนัยสำคัญทางสถิติ (%s) ค่ามัธยฐานสูงสุดคือ %s (%s) สิ่งทดลองที่มีอักษรเหมือนกันไม่แตกต่างกันทางสถิติ (เปรียบเทียบ%s ด้วยวิธี %s)",
            how, np$response, np$treatment, st, top$Treatment, fmt_n(top$Median), if (is.null(np$block)) "อันดับเฉลี่ย" else "ผลรวมอันดับ", np$posthoc_label)
  else sprintf("%s พบว่า %s ของ %s ไม่แตกต่างกันอย่างมีนัยสำคัญทางสถิติ (%s)", how, np$response, np$treatment, st)
}

thai_summary_welch <- function(w) {
  m <- w$means; top <- m[which.max(m$Mean), ]
  st <- sprintf("F = %.2f, df = %d และ %.2f, %s", w$statistic, w$df1, w$df2, th_p(w$p))
  if (w$significant)
    sprintf("Welch's ANOVA (ไม่สมมติว่าความแปรปรวนเท่ากัน) พบว่า %s มีผลต่อ %s อย่างมีนัยสำคัญทางสถิติ (%s) ค่าเฉลี่ยสูงสุดคือ %s (%s) ค่าเฉลี่ยที่มีอักษรเหมือนกันไม่แตกต่างกันทางสถิติ (เปรียบเทียบด้วยวิธี Games-Howell)",
            w$treatment, w$response, st, top$Treatment, fmt_n(top$Mean))
  else sprintf("Welch's ANOVA (ไม่สมมติว่าความแปรปรวนเท่ากัน) พบว่า %s ไม่มีผลต่อ %s อย่างมีนัยสำคัญทางสถิติ (%s)", w$treatment, w$response, st)
}

thai_summary_trimmed <- function(x) {
  m <- x$means; top <- m[which.max(m$TrimmedMean), ]
  st <- sprintf("F = %.2f, df = %d และ %.2f, %s", x$statistic, x$df1, x$df2, th_p(x$p))
  if (x$significant)
    sprintf("การเปรียบเทียบค่าเฉลี่ยตัดปลาย 20%% (ไม่สมมติการแจกแจงปกติและความแปรปรวนเท่ากัน) พบว่า %s มีผลต่อ %s อย่างมีนัยสำคัญทางสถิติ (%s) ค่าเฉลี่ยตัดปลายสูงสุดคือ %s (%s) สิ่งทดลองที่มีอักษรเหมือนกันไม่แตกต่างกันทางสถิติ (Yuen's test ปรับค่า p แบบ Holm)",
            x$treatment, x$response, st, top$Treatment, fmt_n(top$TrimmedMean))
  else sprintf("การเปรียบเทียบค่าเฉลี่ยตัดปลาย 20%% (ไม่สมมติการแจกแจงปกติและความแปรปรวนเท่ากัน) พบว่า %s ไม่มีผลต่อ %s อย่างมีนัยสำคัญทางสถิติ (%s)", x$treatment, x$response, st)
}

thai_summary_tt_trim <- function(r, s) {
  z <- s$table
  sprintf("%s: ผลต่าง = %s (t = %.2f, df = %.2f, %s; ช่วงความเชื่อมั่น %g%% %s ถึง %s; bootstrap %s ถึง %s) %s",
          s$method, fmt_n(z$Estimate), z$t, z$DF, th_p(z$P), 100 * r$conf_level, fmt_n(z$Lower), fmt_n(z$Upper), fmt_n(z$BootLower), fmt_n(z$BootUpper),
          if (s$significant) "แตกต่างอย่างมีนัยสำคัญทางสถิติ" else "ไม่แตกต่างอย่างมีนัยสำคัญทางสถิติ")
}

thai_summary_tt_np <- function(r) {
  w <- r$nonparametric; sig <- !is.na(w$P) && w$P < r$alpha; md <- r$groups$Median
  st <- sprintf("%s, %s", w$Statistic, th_p(w$P))
  ci <- if (is.na(w$Lower)) "" else sprintf(" ช่วงความเชื่อมั่น %g%% %s ถึง %s", 100 * r$conf_level, fmt_n(w$Lower), fmt_n(w$Upper))
  est <- sprintf("ค่าประมาณ Hodges-Lehmann = %s%s", fmt_n(w$Estimate), ci)
  diff <- if (sig) "แตกต่างอย่างมีนัยสำคัญทางสถิติ" else "ไม่แตกต่างอย่างมีนัยสำคัญทางสถิติ"
  switch(r$type,
    independent = sprintf("การทดสอบ Wilcoxon rank-sum (Mann-Whitney) พบว่า %s ของ %s (มัธยฐาน %s) %sกับ %s (มัธยฐาน %s) (%s; %s)",
                          r$response, r$labels[1], fmt_n(md[1]), diff, r$labels[2], fmt_n(md[2]), st, est),
    paired = sprintf("การทดสอบ Wilcoxon signed-rank พบว่า %s (มัธยฐาน %s) และ %s (มัธยฐาน %s) %s (%s; %s)",
                     r$x, fmt_n(md[1]), r$y, fmt_n(md[2]), diff, st, est),
    one = sprintf("การทดสอบ Wilcoxon signed-rank พบว่า %s (มัธยฐาน %s) %sจากค่า %s (%s; %s)",
                  r$response, fmt_n(md[1]), diff, format(r$mu), st, est))
}

# ---- report for Microsoft Word ------------------------------------------------------------------------------------
# Word opens a single-file web page (MHTML) saved as .doc, with the tables as Word tables and the plot embedded; this
# needs no extra R package, so it also works when the app runs in the browser.
word_table <- function(df) {
  if (is.null(df) || !nrow(df)) return("")
  esc <- function(x) htmltools::htmlEscape(as.character(x))
  num <- vapply(df, function(v) { v <- as.character(v); v <- v[nzchar(v)]; length(v) > 0 && all(grepl("^[-<]?[0-9][0-9,.]*[*a-z ]*$", v)) }, NA)
  num[1] <- FALSE
  th <- paste0("<th", ifelse(num, " class=n", ""), ">", esc(names(df)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(df)), function(i) paste0("<tr>", paste0("<td", ifelse(num, " class=n", ""), ">", esc(unlist(df[i, ], use.names = FALSE)), "</td>", collapse = ""), "</tr>"), "")
  paste0("<table><tr>", th, "</tr>", paste(rows, collapse = ""), "</table>")
}
word_doc <- function(file, title, blocks, png = NULL) {
  css <- paste("body { font-family: 'TH Sarabun New', 'TH SarabunPSK', 'Cordia New', sans-serif; font-size: 16pt; }",
               "h1 { font-size: 22pt; } h2 { font-size: 18pt; margin-top: 14pt; } p.small { font-size: 13pt; color: #555; }",
               "table { border-collapse: collapse; margin: 4pt 0 8pt; } th, td { border-top: 1px solid #999; border-bottom: 1px solid #999; padding: 1pt 6pt; font-size: 14pt; }",
               "th { text-align: left; } .n { text-align: right; } pre { font-family: Consolas, monospace; font-size: 10pt; }")
  body <- paste(c(sprintf("<h1>%s</h1>", htmltools::htmlEscape(title)), unlist(blocks),
                  if (!is.null(png)) "<h2>Plot</h2><p><img width=600 src=\"file:///C:/rdstat/plot.png\"></p>"), collapse = "\n")
  html <- sprintf("<html xmlns:o=\"urn:schemas-microsoft-com:office:office\" xmlns:w=\"urn:schemas-microsoft-com:office:word\"><head><meta charset=\"utf-8\"><style>%s</style></head><body lang=th>%s</body></html>", css, body)
  b <- "----=_NextPart_RDstat"
  parts <- c("MIME-Version: 1.0", sprintf("Content-Type: multipart/related; boundary=\"%s\"", b), "",
             paste0("--", b), "Content-Type: text/html; charset=\"utf-8\"", "Content-Transfer-Encoding: base64",
             "Content-Location: file:///C:/rdstat/report.htm", "", b64_lines(charToRaw(enc2utf8(html))))
  if (!is.null(png)) parts <- c(parts, "", paste0("--", b), "Content-Type: image/png", "Content-Transfer-Encoding: base64",
                                 "Content-Location: file:///C:/rdstat/plot.png", "", b64_lines(readBin(png, "raw", file.size(png))))
  writeLines(c(parts, "", paste0("--", b, "--")), file, useBytes = TRUE)
}
b64_lines <- function(raw) { s <- jsonlite::base64_enc(raw); s <- gsub("\\s", "", s); substring(s, seq(1, nchar(s), 76), pmin(nchar(s), seq(76, nchar(s) + 75, 76))) }
w_h2 <- function(x) sprintf("<h2>%s</h2>", htmltools::htmlEscape(x))
w_p <- function(x, cls = NULL) sprintf("<p%s>%s</p>", if (is.null(cls)) "" else sprintf(" class=%s", cls), htmltools::htmlEscape(x))
