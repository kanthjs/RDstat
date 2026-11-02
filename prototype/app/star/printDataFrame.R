`printDataFrame` <-
function (dataFrame, border = TRUE, digits = NULL) 
{
    if (!is.data.frame(dataFrame)) {
        stop("The argument 'dataFrame' should be of class data.frame.")
    }
    dataChar <- DataAttribute(dataFrame)[, 1:2]
    dataChar[, 1] <- as.character(dataChar[, 1])
    colWidth <- NULL
    onlyDecimal <- NULL
    emptyColumn <- NULL
    for (i in (1:nrow(dataChar))) {
        if (all(is.na(dataFrame[, i])) || all(is.nan(dataFrame[, 
            i])) || all(dataFrame[, i] == Inf) || all(dataFrame[, 
            i] == -Inf)) {
            colWidth <- c(colWidth, max(nchar(as.character(dataChar[i, 
                1])), max(nchar(as.character(dataFrame[, i])))) + 
                2)
            emptyColumn <- c(emptyColumn, i)
        }
        else {
            tempsubdata <- subset(dataFrame[, i], !is.na(dataFrame[, 
                i]))
            if (length(tempsubdata) == 0) {
                colWidth <- c(colWidth, max(nchar(as.character(dataChar[i, 
                  1])), max(nchar(as.character(dataFrame[, i])))) + 
                  2)
            }
            else {
                if (dataChar[i, 2] == "numeric" || dataChar[i, 
                  2] == "integer") {
                  tempsubdata <- subset(tempsubdata, !is.nan(tempsubdata))
                  if (length(tempsubdata) == 0) {
                    colWidth <- c(colWidth, max(nchar(as.character(dataChar[i, 
                      1])), max(nchar(as.character(dataFrame[, 
                      i])))) + 2)
                  }
                  else {
                    tempsubdata <- subset(tempsubdata, tempsubdata != 
                      Inf)
                    if (length(tempsubdata) == 0) {
                      colWidth <- c(colWidth, max(nchar(as.character(dataChar[i, 
                        1])), max(nchar(as.character(dataFrame[, 
                        i])))) + 2)
                    }
                    else {
                      tempsubdata <- subset(tempsubdata, tempsubdata != 
                        -Inf)
                      if (length(tempsubdata) == 0) {
                        colWidth <- c(colWidth, max(nchar(as.character(dataChar[i, 
                          1])), max(nchar(as.character(dataFrame[, 
                          i])))) + 2)
                      }
                      else {
                        if (all(tempsubdata < 1) && all(tempsubdata > 
                          -1)) {
                          if (is.null(digits)) {
                            colWidth <- c(colWidth, max(3, nchar(as.character(dataChar[i, 
                              1])), max(nchar(round(dataFrame[, 
                              i], 0))) + 5) + 2)
                          }
                          else {
                            colWidth <- c(colWidth, max(3, nchar(as.character(dataChar[i, 
                              1])), max(nchar(round(dataFrame[, 
                              i], 0))) + digits) + 2)
                          }
                          onlyDecimal <- c(onlyDecimal, i)
                        }
                        else {
                          if (all(is.wholenumber(tempsubdata))) {
                            dataChar[i, 2] <- "integer"
                            colWidth <- c(colWidth, max(3, nchar(as.character(dataChar[i, 
                              1])), nchar(max(tempsubdata))) + 
                              2)
                          }
                          else {
                            if (is.null(digits)) {
                              colWidth <- c(colWidth, max(3, 
                                nchar(as.character(dataChar[i, 
                                  1])), max(nchar(round(dataFrame[, 
                                  i], 0))) + 3) + 2)
                            }
                            else {
                              colWidth <- c(colWidth, max(3, 
                                nchar(as.character(dataChar[i, 
                                  1])), max(nchar(round(dataFrame[, 
                                  i], 0))) + digits) + 2)
                            }
                          }
                        }
                      }
                    }
                  }
                }
                else {
                  colWidth <- c(colWidth, max(nchar(as.character(dataChar[i, 
                    1])), max(nchar(as.character(dataFrame[, 
                    i])))) + 2)
                }
            }
        }
    }
    if (border) 
        cat(formatC(paste(rep("-", sum(colWidth) + nrow(dataChar) - 
            1), collapse = ""), width = sum(colWidth) + nrow(dataChar) - 
            1, format = "s"), "\n")
    for (j in (1:ncol(dataFrame))) {
        if (dataChar[j, 2] == "numeric" || dataChar[j, 2] == 
            "integer") {
            cat(formatC(dataChar[j, 1], width = colWidth[j], 
                format = "s"), sep = "")
        }
        else {
            cat(formatC(dataChar[j, 1], width = colWidth[j], 
                format = "s", flag = "-"), sep = "")
        }
        if (j == ncol(dataFrame)) {
            cat("\n")
        }
        else {
            cat(formatC("", width = 1, format = "s"))
        }
    }
    if (border) 
        cat(formatC(paste(rep("-", sum(colWidth) + nrow(dataChar) - 
            1), collapse = ""), width = sum(colWidth) + nrow(dataChar) - 
            1, format = "s"), "\n")
    for (i in (1:nrow(dataFrame))) {
        for (j in (1:ncol(dataFrame))) {
            if (!is.na(match(j, emptyColumn))) {
                cat(formatC("", width = colWidth[j], format = "s", 
                  flag = "-"), sep = "")
            }
            else {
                if (dataChar[j, 2] == "numeric") {
                  if (is.na(match(j, onlyDecimal))) {
                    if (is.na(dataFrame[i, j]) || is.nan(dataFrame[i, 
                      j]) || dataFrame[i, j] == Inf || all(dataFrame[i, 
                      j] == -Inf)) {
                      cat(formatC("", width = colWidth[j], format = "s"), 
                        sep = "")
                    }
                    else {
                      if (!is.null(digits)) {
                        cat(formatC(dataFrame[i, j], width = colWidth[j], 
                          format = "f", digits = digits), sep = "")
                      }
                      else {
                        cat(formatC(dataFrame[i, j], width = colWidth[j], 
                          format = "f", digits = 2), sep = "")
                      }
                    }
                  }
                  else {
                    if (is.na(dataFrame[i, j]) || is.nan(dataFrame[i, 
                      j]) || dataFrame[i, j] == Inf || all(dataFrame[i, 
                      j] == -Inf)) {
                      cat(formatC("", width = colWidth[j], format = "s"), 
                        sep = "")
                    }
                    else {
                      if (!is.null(digits)) {
                        cat(formatC(dataFrame[i, j], width = colWidth[j], 
                          format = "f", digits = digits), sep = "")
                      }
                      else {
                        cat(formatC(dataFrame[i, j], width = colWidth[j], 
                          format = "f", digits = 4), sep = "")
                      }
                    }
                  }
                }
                else {
                  if (dataChar[j, 2] == "integer") {
                    if (is.na(dataFrame[i, j]) || is.nan(dataFrame[i, 
                      j]) || dataFrame[i, j] == Inf || all(dataFrame[i, 
                      j] == -Inf)) {
                      cat(formatC("", width = colWidth[j], format = "s"), 
                        sep = "")
                    }
                    else {
                      cat(formatC(dataFrame[i, j], width = colWidth[j], 
                        format = "d"), sep = "")
                    }
                  }
                  else {
                    if (is.na(dataFrame[i, j])) {
                      cat(formatC("", width = colWidth[j], format = "s", 
                        flag = "-"), sep = "")
                    }
                    else {
                      cat(formatC(as.character(dataFrame[i, j]), 
                        width = colWidth[j], format = "s", flag = "-"), 
                        sep = "")
                    }
                  }
                }
            }
            if (j == ncol(dataFrame)) {
                cat("\n")
            }
            else {
                cat(formatC("", width = 1, format = "s"))
            }
        }
    }
    if (border) 
        cat(formatC(paste(rep("-", sum(colWidth) + nrow(dataChar) - 
            1), collapse = ""), width = sum(colWidth) + nrow(dataChar) - 
            1, format = "s"), "\n")
}
