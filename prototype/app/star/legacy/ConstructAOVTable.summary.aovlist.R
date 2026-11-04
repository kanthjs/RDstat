`ConstructAOVTable.summary.aovlist` <-
function (aovTable, computeTotal = TRUE) 
{
    temp <- NULL
    for (i in (1:length(aovTable))) {
        temp <- rbind(temp, aovTable[[i]][[1]])
        rownames(temp)[nrow(temp)] <- paste("Error(", letters[i], 
            ")", sep = "")
    }
    if (computeTotal) {
        temp[nrow(temp) + 1, 1:2] <- c(sum(temp[, 1]), sum(temp[, 
            2]))
        rownames(temp)[nrow(temp)] <- "Total"
    }
    return(temp)
}
