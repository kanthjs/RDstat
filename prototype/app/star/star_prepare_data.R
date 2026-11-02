## New API (not part of the original STAR): check and repair the columns chosen for an analysis before they reach
## the original STAR functions. Real data files often have numbers stored as text ("1,250.5", "12 kg", "n/a"),
## padded or empty labels, infinite values, or a factor stored as a number. Everything that is changed or dropped
## is recorded in the `notes` attribute so that the user can see exactly what happened.
##   response : coerced to numeric (text that cannot be read as a number becomes missing)
##   factors  : treatment and block columns become factors; padding is trimmed; empty labels count as missing
## Rows that are missing a treatment or block label are dropped (they cannot be analysed); missing responses are kept
## because STAR handles them itself.

star_parse_number <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  s <- trimws(star_squash_space(as.character(x)))
  s[tolower(s) %in% c("", "na", "n/a", "nan", "null", "none", "missing", "-", "--", ".")] <- NA
  s <- sub("%$", "", s) # 12.5% -> 12.5
  s <- sub("^\\$\\s*(?=[-+]?[0-9.,])", "", s, perl = TRUE) # leading dollar sign
  s <- sub("(?<=[0-9.])\\s*[A-Za-z/%]+$", "", s, perl = TRUE) # trailing unit (kg, g/plot)
  s <- gsub(" ", "", s)
  # decimal comma ("12,5") versus thousands separator ("1,250.5" / "1,250")
  has_dot <- grepl(".", s, fixed = TRUE)
  has_com <- grepl(",", s, fixed = TRUE)
  both <- has_dot & has_com & !is.na(s)
  last_comma <- both & grepl(",[^.]*$", s)
  s[last_comma] <- gsub(",", ".", gsub(".", "", s[last_comma], fixed = TRUE), fixed = TRUE) # 1.250,5
  only_com <- has_com & !has_dot & !is.na(s)
  thousands <- only_com & grepl("^[-+]?[0-9]{1,3}(,[0-9]{3})+$", s)
  s[thousands] <- gsub(",", "", s[thousands], fixed = TRUE) # 1,250
  s[only_com & !thousands] <- sub(",", ".", s[only_com & !thousands], fixed = TRUE) # 12,5
  s[both & !last_comma] <- gsub(",", "", s[both & !last_comma], fixed = TRUE) # 1,250.5
  out <- suppressWarnings(as.numeric(s))
  out
}

star_prepare_data <- function(data, response, factors, block = NULL) {
  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  notes <- character()
  note <- function(...) notes <<- c(notes, paste0(...))
  keep <- c(response, factors, block)
  d <- data[, keep, drop = FALSE]
  n0 <- nrow(d)
  if (n0 == 0) stop("The data set has no rows.")

  # ---- response ----
  y_raw <- d[[response]]
  if (is.factor(y_raw)) y_raw <- as.character(y_raw)
  y <- star_parse_number(y_raw)
  if (!is.numeric(y_raw)) {
    bad <- is.na(y) & !is.na(y_raw) & !star_is_blank(y_raw)
    n_text <- sum(!star_is_blank(y_raw) & !is.na(y_raw))
    if (n_text == 0 || sum(!is.na(y)) == 0) {
      stop("The response '", response, "' contains no numbers. Choose a numeric column.")
    }
    if (sum(bad) > 0.5 * n_text) {
      stop(
        "The response '", response, "' is mostly text (", sum(bad), " of ", n_text,
        " entries are not numbers, e.g. '", paste(utils::head(unique(as.character(y_raw[bad])), 3), collapse = "', '"),
        "'). Choose a numeric column."
      )
    }
    changed <- !is.na(y) & as.character(y_raw) != as.character(y)
    if (any(changed)) {
      note(
        "Response '", response, "': ", sum(changed), " text value(s) were read as numbers (e.g. '",
        as.character(y_raw[changed][1]), "' -> ", y[changed][1], ")."
      )
    }
    if (any(bad)) {
      note(
        "Response '", response, "': ", sum(bad), " value(s) are not numbers and were treated as missing (e.g. '",
        as.character(y_raw[bad][1]), "', row ", which(bad)[1], ")."
      )
    }
  }
  inf <- is.infinite(y)
  if (any(inf)) {
    y[inf] <- NA
    note("Response '", response, "': ", sum(inf), " infinite value(s) were treated as missing.")
  }
  d[[response]] <- y

  # ---- treatment / block columns ----
  clean_label <- function(v, nm) {
    if (is.factor(v)) v <- as.character(v)
    if (is.numeric(v)) {
      v <- ifelse(is.finite(v), as.character(v), NA)
    } else {
      v <- trimws(star_squash_space(as.character(v)))
      v <- gsub("\\s+", " ", v)
      v[tolower(v) %in% c("", "na", "n/a", "nan", "null")] <- NA
    }
    v
  }
  lvl_order <- list()
  for (nm in c(factors, block)) {
    raw <- data[[nm]]
    lab <- clean_label(raw, nm)
    if (is.character(raw) || is.factor(raw)) {
      trimmed <- sum(!is.na(raw) & as.character(raw) != lab & !is.na(lab))
      if (trimmed > 0) note("Column '", nm, "': extra spaces were removed from ", trimmed, " label(s).")
    }
    lv <- unique(lab[!is.na(lab)])
    low <- tapply(lv, tolower(lv), \(z) z)
    clash <- low[vapply(low, length, 0L) > 1]
    if (length(clash)) {
      note(
        "Column '", nm, "': labels that differ only in upper/lower case are treated as different levels (",
        paste(vapply(clash, paste, "", collapse = " / "), collapse = "; "), "). Check for typing errors."
      )
    }
    d[[nm]] <- lab
    lvl_order[[nm]] <- if (is.factor(raw)) trimws(levels(raw)) else NULL # keep the order the user gave a factor
  }

  # ---- drop rows that cannot be analysed ----
  lab_cols <- c(factors, block)
  miss_lab <- Reduce(`|`, lapply(lab_cols, \(nm) is.na(d[[nm]])))
  if (any(miss_lab)) {
    for (nm in lab_cols) {
      if (any(is.na(d[[nm]]))) {
        note("Column '", nm, "': ", sum(is.na(d[[nm]])), " row(s) have no label.")
      }
    }
    note(
      sum(miss_lab), " row(s) with a missing treatment or block label were removed (row ",
      paste(utils::head(which(miss_lab), 5), collapse = ", "), if (sum(miss_lab) > 5) ", ..." else "", ")."
    )
    d <- d[!miss_lab, , drop = FALSE]
  }
  if (nrow(d) == 0) stop("No rows are left after removing rows with missing treatment or block labels.")
  for (nm in lab_cols) {
    lv <- star_level_order(d[[nm]])
    if (!is.null(lvl_order[[nm]])) lv <- c(intersect(lvl_order[[nm]], lv), setdiff(lv, lvl_order[[nm]]))
    d[[nm]] <- factor(d[[nm]], levels = lv)
  }

  n_miss <- sum(is.na(d[[response]]))
  if (n_miss > 0) note("Response '", response, "' has ", n_miss, " missing value(s) out of ", nrow(d), " rows.")
  if (sum(!is.na(d[[response]])) < 3) stop("The response '", response, "' needs at least 3 non-missing values.")
  if (length(unique(stats::na.omit(d[[response]]))) < 2) stop("The response '", response, "' is constant; there is nothing to analyse.")
  for (nm in factors) {
    if (nlevels(d[[nm]]) < 2) stop("The factor '", nm, "' needs at least two levels (it has ", nlevels(d[[nm]]), ").")
    if (nlevels(d[[nm]]) >= nrow(d)) {
      stop("The factor '", nm, "' has a different level on every row (", nlevels(d[[nm]]), " levels); it looks like an ID or a continuous measurement, not a treatment.")
    }
    if (nlevels(d[[nm]]) > 0.5 * nrow(d)) {
      note("Column '", nm, "' has ", nlevels(d[[nm]]), " levels for ", nrow(d), " rows; check that this is a treatment factor.")
    }
  }
  if (!is.null(block) && nlevels(d[[block]]) < 2) stop("The block column '", block, "' needs at least two levels.")
  if (n0 != nrow(d)) note(nrow(d), " of ", n0, " rows are used in the analysis.")
  structure(d, notes = notes)
}

star_is_blank <- function(x) {
  s <- trimws(as.character(x))
  is.na(x) | !nzchar(s) | tolower(s) %in% c("na", "n/a", "nan", "null", "none", "missing", "-", "--", ".")
}

# Levels in natural order: numeric labels sort as numbers (2 before 10), otherwise alphabetically
star_level_order <- function(lab) {
  lv <- unique(lab[!is.na(lab)])
  num <- suppressWarnings(as.numeric(lv))
  if (!anyNA(num)) lv[order(num)] else sort(lv)
}

# non-breaking and zero-width spaces (common in files copied from Word / web pages) -> ordinary space / nothing
star_squash_space <- function(s) {
  s <- gsub(intToUtf8(160), " ", s, fixed = TRUE)
  gsub(intToUtf8(8203), "", s, fixed = TRUE)
}
