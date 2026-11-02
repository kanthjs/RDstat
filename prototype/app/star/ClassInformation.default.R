`ClassInformation.default` <-
function (data, respvar = NULL) 
{
    result <- DataAttribute(data)
    result <- result[result[, "TYPE"] == "factor", c(1, 3, 4)]
    result[, 2] <- as.numeric(result[, 2])
    result[, 2] <- factor(result[, 2])
    colnames(result) <- c("FACTOR", "NO. OF LEVELS", "LEVELS")
    cat("Summary Information\n")
    printDataFrame(result)
    if (!is.null(respvar) && length(respvar) == 1) {
        if (length(na.omit(data[, respvar])) == nrow(data)) {
            cat("Number of Observations Read and Used:", nrow(data), 
                "\n")
        }
        else {
            cat("Number of Observations Read:", nrow(data), "\n")
            cat("Number of Observations Used:", length(na.omit(data[, 
                respvar])), "\n\n")
        }
    }
    if (!is.null(respvar) && length(respvar) > 1) {
        numObsNM = nrow((na.omit(data[, respvar])))
        numObs = nrow((data[, respvar]))
        if (numObs == numObsNM) {
            cat("Number of Observations:", numObs, "\n")
        }
        else {
            cat("Number of Observations:", numObs, "\n")
            cat("Number of Observations Used:", numObsNM, "\n\n")
        }
    }
}
