# TODO: Add comment
# 
# Author: Claudio
###############################################################################

downloadExchangeRatesFromBloomberg <- function() {
	# create the list of currencies
	currencies <- c("AUD","BRL","CAD","DKK","EUR","GBP","IDR",
			"JPY","MXN","NOK","NZD","PLN","SEK","SGD","TRY","USD","ZAR")
	
	# create the Bloomberg currency ID vis a vis CHF
	currenciesBlb <- paste(currencies,"CHF Curncy",sep="")
	
	# connect to bloomberg
	conn <- blpConnect()
	
	# download the rates
	rates.df <- bdp(conn,currenciesBlb,"PX_LAST")
	
	# set the updateTime
	lastUpdateDateTime <- Sys.time() 
			
	# disconnect from Bloomberg
	blpDisconnect(conn)
	
	# create the vector
	rates <- rates.df[,"PX_LAST"]
	names(rates) <- currencies
	
	lastUpdateDateTime <- rep(lastUpdateDateTime,length(rates))
	names(lastUpdateDateTime) <- currencies
	
	# adjust 
	divideBYHundred <- c("JPY","SEK","NOK")
	rates[divideBYHundred] <- rates[divideBYHundred] / 100
	rates["IDR"] <- rates["IDR"] / 10000
	
	return(list(rates=rates,lastUpdateDateTime=lastUpdateDateTime))
}

# source("./base/unitTests/utilities/createExchangeRatesVector.R")
data <- downloadExchangeRatesFromBloomberg()

rm(downloadExchangeRatesFromBloomberg)

# create the exchangeRates repository		
updated <- repositories$exchangeRates$update(data$rates,data$lastUpdateDateTime)
rm(data)

directoryExchangeRates <- file.path(systemOptions[["homeDir"]],"data","exchangeRates")
# directoryExchangeRates <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files","exchangeRates")

if (updated) saveLastObject(repositories$exchangeRates,"exchangeRates.RData",directoryExchangeRates)
rm(update,directoryExchangeRates)

