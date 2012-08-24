# TODO: Add comment
# 
# Author: Claudio
###############################################################################

downloadExchangeRatesFromBloomberg <- function(currencies) {
	# create the list of currencies
	if (missing(currencies)) currencies <- c("AUD","BRL","CAD","DKK","EUR","GBP","IDR",
				"INR","JPY","MXN","NOK","NZD","PLN","SEK","SGD","TRY","USD","ZAR")
	
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
	
	return(list(rates=rates,lastUpdateDateTime=lastUpdateDateTime))
}

updateFromBloomberg_exchangeRatesRepository <- function(currencies,saveIfNewer=FALSE) {
	
	if (missing(currencies)) currencies <- sys[["tradedCurrencies"]]
	
	#remove CHF if present
	currencies <- currencies[!currencies=="CHF"]
	
	data <- downloadExchangeRatesFromBloomberg(currencies)
	
	# adjust the rates for the price multiplication factor (not necessary when downloading with API)
	multFactors <- repositories$exchangeRates$chfMultFactors[currencies]
	data$rates <- data$rates * multFactors
	
	# create the exchangeRates repository
	currentRates <- repositories$exchangeRates$rates
	currentLastUpdateDateTime <- repositories$exchangeRates$lastUpdateDateTime
	currentRates[currencies] <- data$rates
	currentLastUpdateDateTime[currencies] <- data$lastUpdateDateTime
	
	updated <- repositories$exchangeRates$update(currentRates,currentLastUpdateDateTime)
	rm(data)
	
	directoryExchangeRates <- file.path(sys[["homeDir"]],"data","exchangeRates")
	# directoryExchangeRates <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","files","exchangeRates")
	
	if (updated & saveIfNewer) saveLastObject(repositories$exchangeRates,"exchangeRates.RData",directoryExchangeRates)
	rm(updated,directoryExchangeRates)
	
}

load_repositoryExchangeRate <- function(saveIfNewer=FALSE,directory=file.path(sys[["homeDir"]],"data","exchangeRates"),
		fileName="exchangeRates.RData") {
	
	file <- file.path(sys[["homeDir"]],"data","exchangeRates","exchangeRates.RData")
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

save_repositoryExchangeRate <- function(exchangeRatesRepo,directory=file.path(sys[["homeDir"]],"data","exchangeRates"),
		fileName="exchangeRates.RData") {
	
	oldFileFullName <- file.path(directory,fileName)
	fileExists <- file.exists(oldFileFullName)
	
	if (fileExists) {
		dateTime <- format(Sys.time(),format="%Y-%m-%d_%H-%M-%S")
		file.rename(from=oldFileFullName,to=file.path(directory,paste(dateTime,fileName,sep="_")))

	}
	
	saveLastObject(object=exchangeRatesRepo,fileName=fileName,directory=directory)
	
}

save_testRepositoryExchangeRate <- function() {
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data","exchangeRates")
	save_repositoryExchangeRate(repositories$exchangeRates,directory=directory)
}

