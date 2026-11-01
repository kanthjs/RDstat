## New API (not part of the original STAR): read an uploaded data file (.csv, .tsv, .txt, .xlsx) into a data frame
## whose columns are all text, so that star_prepare_data() decides what is numeric. Delimiter (, ; tab |) and
## encoding are detected: UTF-8 (with or without BOM), UTF-16 (Excel's "Unicode Text"), Thai Windows-874 / TIS-620
## (what Excel on a Thai Windows writes as "CSV (Comma delimited)"), else Latin-1. Excel files are read with readxl.

star_read_table <- function(path, name = path) {
  ext <- tolower(sub(".*[.]", "", name))
  df <- if (ext %in% c("xlsx", "xlsm")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Reading Excel files needs the 'readxl' package.")
    as.data.frame(readxl::read_excel(path, sheet = 1, col_names = TRUE, col_types = "text", .name_repair = "minimal"))
  } else if (ext == "xls") {
    stop("Old .xls files are not supported; save the sheet as .xlsx or .csv.")
  } else {
    lines <- strsplit(star_decode_text(readBin(path, "raw", file.info(path)$size)), "\r\n|\r|\n")[[1]]
    first <- lines[nzchar(trimws(lines))]
    if (!length(first)) stop("The file is empty.")
    cnt <- vapply(c(",", ";", "\t", "|"), \(s) sum(lengths(regmatches(first[1], gregexpr(s, first[1], fixed = TRUE)))), 0)
    sep <- if (max(cnt) == 0) "," else names(cnt)[which.max(cnt)]
    con <- textConnection(lines, encoding = "UTF-8")
    on.exit(close(con))
    read.table(con,
      header = TRUE, sep = sep, quote = "\"", colClasses = "character", comment.char = "",
      strip.white = TRUE, na.strings = c("", "NA"), check.names = FALSE, fill = TRUE, encoding = "UTF-8"
    )
  }
  if (!is.data.frame(df) || ncol(df) < 2 || nrow(df) < 1) stop("The file needs a header row, at least two columns and one data row.")
  df[] <- lapply(df, as.character)
  names(df) <- trimws(names(df))
  if (anyDuplicated(names(df)) || any(!nzchar(names(df)))) stop("Column names must be non-empty and unique.")
  df
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
