# TODO: Add comment
# 
# Author: Claudio
###############################################################################

downloadExchangeRatesFromBloomberg <- function() {
	# create the list of currencies
	currencies <- c("AUD","BRL","CAD","CNY","DKK","EUR","GBP","IDR","INR",
			"JPY","MXN","NOK","NZD","PLN","RUB","SEK","SGD","TRY","USD","ZAR")
	
	# create the Bloomberg currency ID vis a vis CHF
	currenciesBlb <- paste(currencies,"CHF Curncy",sep="")
	
	# connect to bloomberg
	conn <- blpConnect()
	
	# download the rates
	rates.df <- bdp(conn,currenciesBlb,"PX_LAST")
	
	# disconnect from Bloomberg
	blpDisconnect(conn)
	
	# create the vector
	rates <- rates.df[,"PX_LAST"]
	names(rates) <- currencies
	
	# adjust 
	divideBYHundred <- c("JPY","SEK","NOK")
	rates[divideBYHundred] <- rates[divideBYHundred] / 100
	rates["IDR"] <- rates["IDR"] / 10000
	
}

#if (!bloombergExchangeRates) {
	# if it is not possible to access bloomber use the exchange rates in the file
	# System/data/repositoryExchangeRates.csv
#	file <- file.path(systemOptions[["sourceCodeDir"]],"data","repositoryExchangeRates.csv")
#	rates.df <- read.csv(file=file,header=TRUE,stringsAsFactors=FALSE)
#	rates <- rates.df[,"CHFPar"]
#	names(rates) <- rates.df[,"Moneta"]
#	rm(file,rates.df)
#}

# source("./unitTests/utilities/createExchangeRatesVector.R")
rates <- downloadExchangeRatesFromBloomberg()
rm(downloadExchangeRatesFromBloomberg)

# create the exchangeRates repository		
exchangeRates <- create_testRepositoryExchangeRates(rates)
rm(rates)

# assign the repository	
assign("exchangeRates",exchangeRates,envir=repositories)

rm(exchangeRates)


