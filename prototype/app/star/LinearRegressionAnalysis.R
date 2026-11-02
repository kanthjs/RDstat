`LinearRegressionAnalysis` <-
function (data, depVar, indepVar, constant = TRUE, statistics = TRUE, 
    confInt = FALSE, confLevel = 0.95, covMatrix = FALSE, normality = NULL, 
    heteroskedasticity = NULL, autoCorr = FALSE, VIF = FALSE, 
    COOKS = FALSE, leverage = FALSE) 
UseMethod("LinearRegressionAnalysis")
