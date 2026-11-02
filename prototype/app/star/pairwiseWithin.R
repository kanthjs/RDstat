`pairwiseWithin` <-
function (data, respvar, typeTest, nobs1, nobs2, dfError, MSError, 
    f1, f2, f3 = NULL, siglevel) 
{
    comparison <- NULL
    if (nlevels(data[, f1]) > 26) {
        for (j in (1:nobs2)) {
            command <- paste(typeTest, ".test(data['", respvar, 
                "'][(data['", f2, "']) == levels(data[,'", f2, 
                "'])[[", j, "]]", f3, "], data['", f1, "'][(data['", 
                f2, "']) == levels(data[,'", f2, "'])[[", j, 
                "]]", f3, "], dfError, MSError, alpha = ", siglevel, 
                ", group = FALSE, pwOrder = 'trmt')", sep = "")
            cat("\n-----", f2, " = ", levels(data[, f2])[[j]], 
                "-----\n")
            eval(parse(text = command))
        }
    }
    else {
        for (j in (1:nobs2)) {
            k = 1 + 2 * (j - 1)
            command <- paste(typeTest, ".test(data['", respvar, 
                "'][(data['", f2, "']) == levels(data[,'", f2, 
                "'])[[", j, "]]", f3, "], data['", f1, "'][(data['", 
                f2, "']) == levels(data[,'", f2, "'])[[", j, 
                "]]", f3, "], dfError, MSError, alpha = ", siglevel, 
                ", group = TRUE, pwOrder = 'trmt')", sep = "")
            capture.output(result.pw <- eval(parse(text = command)))
            if (is.null(comparison)) {
                comparison <- result.pw$summary[, c(1, 3, 2, 
                  5)]
                colnames(comparison)[k] <- f1
                colnames(comparison)[k + 2] <- paste(f2, " = ", 
                  levels(data[, f2])[[j]], sep = "")
            }
            else {
                comparison <- cbind(comparison, result.pw$summary[, 
                  c(2, 5)])
                colnames(comparison)[ncol(comparison) - 1] <- paste(f2, 
                  " = ", levels(data[, f2])[[j]], sep = "")
            }
        }
        options(width = 5000)
        cat("\n", result.pw$method, "\n\n", sep = "")
        if (typeTest == "LSD" || typeTest == "HSD" || typeTest == 
            "scheffe") {
            maxWidthEntry <- max(nchar(dfError), nchar(round(MSError, 
                0)), nchar(round(result.pw$tabValue, 0)), nchar(round(result.pw$testStat, 
                0))) + 7
        }
        else {
            maxWidthEntry <- max(nchar(dfError), nchar(round(MSError, 
                0))) + 7
        }
        cat(formatC("Alpha", format = "s", width = 25, flag = "-"), 
            formatC(siglevel, format = "f", digits = 2, width = maxWidthEntry, 
                flag = "#"), "\n", sep = "")
        cat(formatC("Error Degrees of Freedom", format = "s", 
            width = 25, flag = "-"), formatC(dfError, format = "d", 
            width = maxWidthEntry, flag = "#"), "\n", sep = "")
        cat(formatC("Error Mean Square", format = "s", width = 25, 
            flag = "-"), formatC(MSError, format = "f", digits = 4, 
            width = maxWidthEntry, flag = "#"), "\n", sep = "")
        if (typeTest == "LSD" || typeTest == "HSD" || typeTest == 
            "scheffe") {
            cat(formatC("Critical Value", format = "s", width = 25, 
                flag = "-"), formatC(result.pw$tabValue, format = "f", 
                digits = 4, width = maxWidthEntry, flag = "#"), 
                "\n", sep = "")
            cat(formatC("Test Statistic", format = "s", width = 25, 
                flag = "-"), formatC(result.pw$testStat, format = "f", 
                digits = 4, width = maxWidthEntry, flag = "#"), 
                "\n\n", sep = "")
        }
        else {
            cat("\n")
            printDataFrame(result.pw$testStat, digits = 4)
            cat("\n")
        }
        colnames(result.pw$summary)[1] <- f1
        cat("Summary:", "\n")
        printDataFrame(comparison, digits = 4)
        cat("Means with the same letter are not significantly different\n\n")
    }
}
