# TODO: Add comment
# 
# Author: claudio
###############################################################################
systemOptions <- list()
systemOptions[["eq.tolerance"]] <- 10^-6
systemOptions[["allowedCurrencies"]] <- c("CHF","EUR","USD","AUD","NOK","GBP","INR","BRL","JPY","CAD","INR","NZD","CHr")
if (exists("isTest") & .Platform$OS.type!="windows") {
	systemOptions[["homeDir"]] <- "/home/claudio/riskman"
} else {
	systemOptions[["homeDir"]] <- "c:/riskman"
}