# TODO: Add comment
# 
# Author: claudio
###############################################################################
sys <- list()
sys[["eq.tolerance"]] <- 10^-6
sys[["allowedCurrencies"]] <- c("CHF","EUR","USD","AUD","NOK","GBP","INR","BRL","JPY","CAD","INR","NZD","CHr")

sys[["homeDir"]] <- homeDir
sys[["sourceCodeDir"]] <- sourceCodeDir

# the number of minutes necessary before a bloomberg data is considered
sys[["bloombergUpdateInterval"]] <- as.difftime(240, units = "mins")

if (.Platform$OS.type=="windows") {
	sys[["R_ZIPCMD"]] <- "C:/Rtools/bin/zip.exe"
} else {
	sys[["R_ZIPCMD"]] <- "/usr/bin/zip"
}
Sys.setenv(R_ZIPCMD=sys[["R_ZIPCMD"]])
