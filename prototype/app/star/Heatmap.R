`Heatmap` <-
function (R, data, genAs = "fixed", row, column, respvar, model, 
    env) 
{
    data[, match(env, names(data))] <- factor(trimStrings(data[, 
        match(env, names(data))]))
    for (i in 1:length(respvar)) {
        for (j in 1:length(levels(data[, match(env, names(data))]))) {
            resid = paste(respvar[i], "_", "resid_", genAs, sep = "")
            envlevel = levels(data[, match(env, names(data))])[j]
            data1 = subset(data, data[, match(env, names(data))] == 
                envlevel)
            Rdata1 = subset(R, R[match(env, names(R))] == envlevel)
            lp = levelplot(Rdata1[[match(resid, names(Rdata1))]] ~ 
                data1[[match(row, names(data1))]] * data1[[match(column, 
                  names(data1))]], xlab = row, ylab = column, 
                main = paste("Residuals (", resid, ")", sep = ""), 
                sub = paste(env, " = ", envlevel, ", ", model, 
                  sep = ""), col.regions = colorRampPalette(c("yellow", 
                  "red"))(50))
            png(paste(getwd(), "/heatmap_", resid, "_", model, 
                "_", envlevel, ".png", sep = ""))
            print(lp)
            dev.off()
        }
    }
}
