# TODO: Add comment
# 
# Author: claudio
###############################################################################
sys <- list()
sys[["eq.tolerance"]] <- 10^-3

# set the sleep time in seconds for new incoming message check
sys[["sleepTime"]] <- 20

sys[["stopHour"]] <- "17:45:00"

# "CHr" will be removed and is deprecated (when removed please remove also in 
# repository exchange rate, i.e. identical(toCurrency,new("Currency","CHr"))
sys[["allowedCurrencies"]] <- c("ATS",
		"AUD","BEF","BRL","CAD",
		"CHF","DEM","DKK",
		"ESP","EUR","FIM","FRF",
		"GBP","GRD","IDR","IEP",
		"INR","ITL","JPY","MXN",
		"NLG","NOK","NZD","PLN",
		"PTE","SEK","SGD",
		"TRY","USD","ZAR","CHr"
)

sys[["tradedCurrencies"]] <- c(
		"AUD","BRL","CAD",
		"CHF","DKK","EUR",
		"GBP","IDR","INR",
		"JPY","MXN","NOK",
		"NZD","PLN","SEK",
		"SGD","TRY","USD",
		"ZAR"
)

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
