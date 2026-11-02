`AOVTest` <-
function (data, design, respvar, factor1, factor2 = NULL, factor3 = NULL, 
    factor4 = NULL, rep1 = NULL, rep2 = NULL, set = NULL, descriptive = FALSE, 
    normality = FALSE, homogeneity = FALSE, pwTest = NULL, pwVar = NULL, 
    contrastOption = NULL, sig = 0.05, outputPath = NULL) 
UseMethod("AOVTest")
