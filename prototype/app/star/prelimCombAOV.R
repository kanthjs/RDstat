`prelimCombAOV` <-
function (model, data, set) 
{
    outputData <- NULL
    for (i in (1:nlevels(data[, set]))) {
        tmpData <- data[data[, set] == levels(data[, set])[i], 
            ]
        result <- suppressWarnings(aov(formula(model), data = tmpData))
        residNfittedData <- data.frame(PredictedValues(result))
        if (inherits(result, what = "aovlist")) {
            residNfittedData <- data.frame(residNfittedData, 
                proj(result)[[length(result)]][, "Residuals"])
        }
        else {
            residNfittedData <- data.frame(residNfittedData, 
                residuals(result))
        }
        colnames(residNfittedData) <- c(paste(colnames(result$model)[1], 
            "pred", sep = "_"), paste(colnames(result$model)[1], 
            "resid", sep = "_"))
        outputData <- rbind(outputData, cbind(byVar = levels(data[, 
            set])[i], result$model, residNfittedData))
    }
    colnames(outputData)[1] <- set
    capture.output(bartlett.result <- HeteroskedasticityTest(data = outputData, 
        var = names(result$model)[1], grp = set, method = c("bartlett")))
    cat("Bartlett's Test for Homogeneity of Variances\n")
    printDataFrame(bartlett.result[, 3:ncol(bartlett.result)])
    cat("\n")
    return(list(outputData, test = bartlett.result))
}
