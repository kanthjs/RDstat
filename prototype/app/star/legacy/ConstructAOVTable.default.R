`ConstructAOVTable.default` <-
function (aovTable, computeTotal = TRUE) 
{
    temp <- aovTable
    rownames(temp)[nrow(temp)] <- "Error"
    if (computeTotal) {
        temp[nrow(temp) + 1, 1:2] <- c(sum(temp[, 1]), sum(temp[, 
            2]))
        rownames(temp)[nrow(temp)] <- "Total"
    }
    return(temp)
}
