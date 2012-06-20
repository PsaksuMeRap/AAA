# TODO: Add comment
# 
# Author: Claudio
###############################################################################

downloadExchangeRatesFromBloomberg <- function(currencies) {
	# create the list of currencies
	if (missing(currencies)) currencies <- c("AUD","BRL","CAD","DKK","EUR","GBP","IDR",
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

updateFromBloomberg_exchangeRatesRepository <- function(currencies,saveIfNewer=FALSE) {
	
	if (missing(currencies)) currencies <- c("AUD","BRL","CAD","DKK","EUR","GBP","IDR",
				"JPY","MXN","NOK","NZD","PLN","SEK","SGD","TRY","USD","ZAR")
	
	data <- downloadExchangeRatesFromBloomberg(currencies)
	
	rm(downloadExchangeRatesFromBloomberg)
	
	# create the exchangeRates repository		
	updated <- repositories$exchangeRates$update(data$rates,data$lastUpdateDateTime)
	rm(data)
	
	directoryExchangeRates <- file.path(systemOptions[["homeDir"]],"data","exchangeRates")
	# directoryExchangeRates <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files","exchangeRates")
	
	if (updated & saveIfNewer) saveLastObject(repositories$exchangeRates,"exchangeRates.RData",directoryExchangeRates)
	rm(update,directoryExchangeRates)
	
}

load_repositoryExchangeRate <- function(saveIfNewer=FALSE,directory=file.path(systemOptions[["homeDir"]],"data","exchangeRates"),
		fileName="exchangeRates.RData") {
	
	file <- file.path(systemOptions[["homeDir"]],"data","exchangeRates","exchangeRates.RData")
	tmpEnvir <- new.env()
	load(file,envir=tmpEnvir)
	
	if (exists("exchangeRates",where=repositories,inherits=FALSE)) {
		updated <- repositories$exchangeRates$update(tmpEnvir$object$rates,tmpEnvir$object$lastUpdateDateTime)
		if (updated & saveIfNewer) saveLastObject(object=repositories$exchangeRates,fileName=fileName,
					directory=directory)
	} else {
		assign("exchangeRates",tmpEnvir$object,pos=repositories)
	}
	
	
}

