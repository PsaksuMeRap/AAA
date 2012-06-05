# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertEquityTradeToPosition <- function() {
	# set the fileName from which to import trades
	fileName <- "equityTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade)
	
	checkEquals(class(newSecurity)[[1]],"Equity")

}

test.shouldConvertFuturesOnIndexTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "futureEquityIndexTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <<- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade)	
	checkEquals(class(newSecurity)[[1]],"Futures_EQ")
	
}


test.shouldConvertBondTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "bondTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <<- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade)	
	checkEquals(class(newSecurity)[[1]],"Bond")	
	
}

test.shouldConvertFxSpotToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "fxSpotTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <<- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade)	
	checkEquals(class(newSecurity)[[1]],"Conto_corrente")	
	
}

test.shouldConvertOptionEquityToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "OptionEquityTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <<- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade)	
	checkEquals(class(newSecurity)[[1]],"Opzioni_su_azioni")	
	
}
