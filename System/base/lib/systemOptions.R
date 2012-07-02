# TODO: Add comment
# 
# Author: claudio
###############################################################################
systemOptions <- list()
systemOptions[["eq.tolerance"]] <- 10^-6
systemOptions[["allowedCurrencies"]] <- c("CHF","EUR","USD","AUD","NOK","GBP","INR","BRL","JPY","CAD","INR","NZD","CHr")

systemOptions[["homeDir"]] <- homeDir
systemOptions[["sourceCodeDir"]] <- sourceCodeDir

# the number of minutes necessary before a bloomberg data is considered
systemOptions[["bloombergUpdateInterval"]] <- as.difftime(240, units = "mins")

if (.Platform$OS.type=="windows") {
	systemOptions[["R_ZIPCMD"]] <- "C:/Rtools/bin/zip.exe"
} else {
	systemOptions[["R_ZIPCMD"]] <- "/usr/bin/zip"
}
Sys.setenv(R_ZIPCMD=systemOptions[["R_ZIPCMD"]])
