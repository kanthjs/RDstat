`ConstructAOVTable.summary.aov` <-
function (aovTable, computeTotal = TRUE) 
{
    if (is.list(aovTable)) 
        temp <- aovTable[[1]]
    else temp <- aovTable
    rownames(temp)[nrow(temp)] <- "Error"
    if (computeTotal) {
        temp[nrow(temp) + 1, 1:2] <- c(sum(temp[, 1]), sum(temp[, 
            2]))
        rownames(temp)[nrow(temp)] <- "Total"
    }
    return(temp)
}
