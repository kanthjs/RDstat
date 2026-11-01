## New API (not part of the original STAR): read an uploaded data file (.csv, .tsv, .txt, .xlsx) into a data frame
## whose columns are all text, so that star_prepare_data() decides what is numeric. Delimiter (, ; tab |) and
## encoding are detected: UTF-8 (with or without BOM), UTF-16 (Excel's "Unicode Text"), Thai Windows-874 / TIS-620
## (what Excel on a Thai Windows writes as "CSV (Comma delimited)"), else Latin-1. Excel files are read with readxl.

##
## The table need not start in the first row: title lines above the column names (an experiment name, a date) are
## skipped, as are empty columns and summary rows below the data (Mean, Total, CV, LSD, เฉลี่ย, รวม, ...). What was
## skipped is described in attr(result, "notes"), so that the user can see it.

star_read_table <- function(path, name = path) {
  ext <- tolower(sub(".*[.]", "", name))
  raw <- if (ext %in% c("xlsx", "xlsm")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Reading Excel files needs the 'readxl' package.")
    as.data.frame(readxl::read_excel(path, sheet = 1, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  } else if (ext == "xls") {
    stop("Old .xls files are not supported; save the sheet as .xlsx or .csv.")
  } else {
    lines <- strsplit(star_decode_text(readBin(path, "raw", file.info(path)$size)), "\r\n|\r|\n")[[1]]
    lines <- lines[nzchar(trimws(lines))]
    if (!length(lines)) stop("The file is empty.")
    sep <- star_guess_sep(lines)
    read <- function(f, ...) { con <- textConnection(lines, encoding = "UTF-8"); on.exit(close(con)); f(con, sep = sep, quote = "\"", comment.char = "", ...) }
    # the widest line sets the number of columns (read.table would look at the first five only, i.e. at the title)
    width <- max(read(count.fields, blank.lines.skip = TRUE), 1, na.rm = TRUE)
    read(read.table,
      header = FALSE, col.names = paste0("V", seq_len(width)), colClasses = "character",
      strip.white = TRUE, na.strings = c("", "NA"), fill = TRUE, encoding = "UTF-8"
    )
  }
  star_table_from_cells(raw)
}

## the delimiter that splits the most lines into the same number of fields (a title line above the table, or
## decimal commas in a semicolon file, do not change the answer); ties go to the one giving more fields
star_guess_sep <- function(lines) {
  lines <- utils::head(lines, 200)
  score <- vapply(c(",", ";", "\t", "|"), \(s) {
    n <- lengths(regmatches(lines, gregexpr(s, lines, fixed = TRUE)))
    if (!any(n > 0)) return(c(0, 0))
    mode <- as.integer(names(which.max(table(n[n > 0]))))
    c(mean(n == mode), mode)
  }, numeric(2))
  if (all(score[1, ] == 0)) return(",")
  best <- order(-score[1, ], -score[2, ])[1]
  colnames(score)[best]
}

## a grid of cells (all text, no header yet) -> the data frame under its header row, with notes on what was dropped
star_table_from_cells <- function(raw) {
  if (!is.data.frame(raw) || !nrow(raw) || !ncol(raw)) stop("The file is empty.")
  m <- vapply(raw, \(v) { v <- trimws(as.character(v)); v[is.na(v)] <- ""; v }, character(nrow(raw)))
  if (!is.matrix(m)) m <- matrix(m, nrow = nrow(raw))
  notes <- character()
  m <- m[, colSums(m != "") > 0, drop = FALSE]
  m <- m[rowSums(m != "") > 0, , drop = FALSE]
  if (!nrow(m)) stop("The file is empty.")
  filled <- rowSums(m != "")
  # the header is the first row with at least two cells and no fewer cells than the rows that follow it
  # (a title line has one cell, or fewer than the table is wide)
  hdr <- 1L
  for (i in seq_len(min(nrow(m), 20L))) {
    nxt <- filled[seq.int(i + 1L, length.out = min(5L, nrow(m) - i))]
    if (filled[i] >= 2 && (!length(nxt) || filled[i] >= max(nxt))) { hdr <- i; break }
  }
  if (hdr > 1) {
    skipped <- apply(m[seq_len(hdr - 1), , drop = FALSE], 1, \(r) paste(r[nzchar(r)], collapse = " "))
    notes <- c(notes, sprintf("Skipped %d line%s above the column names: %s.", hdr - 1, if (hdr > 2) "s" else "",
                              paste0("“", skipped, "”", collapse = ", ")))
  }
  nm <- m[hdr, ]
  m <- m[-seq_len(hdr), , drop = FALSE]
  # columns with neither a name nor any data under the header (e.g. a note beside the title)
  keep <- nzchar(nm) | colSums(m != "") > 0
  nm <- nm[keep]; m <- m[, keep, drop = FALSE]
  # summary rows at the bottom of the table, labelled in their first filled cell
  lab <- apply(m, 1, \(r) r[nzchar(r)][1])
  is_sum <- star_is_summary_label(lab)
  n_sum <- 0L
  while (n_sum < nrow(m) && is_sum[nrow(m) - n_sum]) n_sum <- n_sum + 1L
  if (n_sum && n_sum < nrow(m)) {
    rows <- seq.int(nrow(m) - n_sum + 1L, nrow(m))
    notes <- c(notes, sprintf("Skipped %d summary row%s below the data: %s.", n_sum, if (n_sum > 1) "s" else "",
                              paste0("“", lab[rows], "”", collapse = ", ")))
    m <- m[-rows, , drop = FALSE]
  }
  if (length(nm) < 2 || nrow(m) < 1) stop("The file needs a header row, at least two columns and one data row.")
  if (anyDuplicated(nm) || any(!nzchar(nm))) stop("Column names must be non-empty and unique.")
  m[m == ""] <- NA
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  names(df) <- nm
  rownames(df) <- NULL
  attr(df, "notes") <- notes
  df
}

## labels of rows that summarise the data rather than hold a plot: Mean, Grand mean, Total, CV (%), LSD 0.05, SE,
## เฉลี่ย, รวม, ...
star_is_summary_label <- function(x) {
  k <- tolower(trimws(x))
  k <- gsub("\\(.*\\)|[0-9%:=.,*]", " ", k)
  k <- trimws(gsub("\\s+", " ", k))
  words <- c("mean", "means", "average", "avg", "grand mean", "overall mean", "total", "grand total", "sum", "cv",
             "lsd", "hsd", "dmrt", "sd", "se", "sem", "s e", "s d", "std dev", "stdev", "std error", "standard error",
             "standard deviation", "min", "max", "minimum", "maximum", "f test", "f-test",
             "เฉลี่ย", "ค่าเฉลี่ย", "เฉลี่ยรวม", "ค่าเฉลี่ยรวม", "รวม", "ผลรวม", "รวมทั้งหมด")
  !is.na(k) & k %in% words
}

## the bytes of a text file as one UTF-8 string. Decoded by hand rather than with iconv(), whose encodings
## differ between platforms (and webR, where the app runs).
star_decode_text <- function(b) {
  b <- as.integer(b)
  out <- function(cp) enc2utf8(intToUtf8(cp))
  if (length(b) >= 3 && all(b[1:3] == c(0xEF, 0xBB, 0xBF))) b <- b[-(1:3)]
  if (length(b) >= 2 && (all(b[1:2] == c(0xFF, 0xFE)) || all(b[1:2] == c(0xFE, 0xFF)))) {
    le <- b[1] == 0xFF
    b <- b[-(1:2)]
    b <- b[seq_len(length(b) %/% 2 * 2)]
    b1 <- b[c(TRUE, FALSE)]; b2 <- b[c(FALSE, TRUE)]
    u <- if (le) b1 + 256L * b2 else 256L * b1 + b2
    # surrogate pairs (characters beyond the Basic Multilingual Plane)
    s <- which(u >= 0xD800 & u <= 0xDBFF & c(u[-1], 0L) >= 0xDC00 & c(u[-1], 0L) <= 0xDFFF)
    if (length(s)) { u[s] <- 0x10000L + (u[s] - 0xD800L) * 1024L + (u[s + 1] - 0xDC00L); u <- u[-(s + 1)] }
    return(out(u[u < 0xD800 | u > 0xDFFF]))
  }
  if (validUTF8(rawToChar(as.raw(b[b != 0])))) {
    x <- rawToChar(as.raw(b[b != 0])); Encoding(x) <- "UTF-8"
    return(x)
  }
  if (star_looks_thai(b)) {
    cp <- b
    th <- b >= 0xA1 & b <= 0xFB
    cp[th] <- 0x0E01L + b[th] - 0xA1L
    # the Windows-874 additions in 0x80-0x9F: euro, ellipsis, quotes, bullet, dashes
    w <- c(`128` = 0x20AC, `133` = 0x2026, `145` = 0x2018, `146` = 0x2019, `147` = 0x201C, `148` = 0x201D,
           `149` = 0x2022, `150` = 0x2013, `151` = 0x2014)
    hit <- as.character(b) %in% names(w)
    cp[hit] <- w[as.character(b[hit])]
    return(out(cp))
  }
  out(b) # Latin-1: each byte is its own code point
}

## Thai text in TIS-620 is almost all high bytes, in runs (a word is several Thai characters); Latin-1 text has an
## accented letter here and there between ASCII letters. Bytes TIS-620 leaves undefined rule it out.
star_looks_thai <- function(b) {
  hi <- b >= 0x80
  if (!any(hi) || any(b %in% c(0xDB:0xDE, 0xFC:0xFF))) return(FALSE)
  run <- hi & (c(FALSE, hi[-length(hi)]) | c(hi[-1], FALSE))
  mean(run[hi]) >= 0.5
}
