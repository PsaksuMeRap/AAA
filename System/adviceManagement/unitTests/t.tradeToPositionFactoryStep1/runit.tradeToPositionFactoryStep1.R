# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertEquityTradeToSecurityStep1 <- function() {
	# set the fileName from which to import trades
	fileName <- "equityTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactoryStep1") 
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactoryStep1(trade)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade)
	
	checkEquals(class(newSecurity)[[1]],"Equity")
	
}

test.shouldConvertFuturesOnIndexTradeToSecurityStep1 <- function() {
	# set the fileName from which to import trades
	fileName <- "futureEquityIndexTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactoryStep1") 
	blRequestHandler <<- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactoryStep1(trade)	
	checkEquals(class(newSecurity)[[1]],"Futures_EQ")
	
}


test.shouldConvertBondTradeToSecurityStep1 <- function() {
	# set the fileName from which to import trades
	fileName <- "bondTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactoryStep1") 
	blRequestHandler <<- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactoryStep1(trade)	
	checkEquals(class(newSecurity)[[1]],"Bond")	
	
}

test.shouldConvertFxSpotToSecurityStep1 <- function() {
	# set the fileName from which to import trades
	fileName <- "fxSpotTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactoryStep1") 
	blRequestHandler <<- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactoryStep1(trade)	
	checkEquals(class(newSecurity)[[1]],"Conto_corrente")	
	
}

test.shouldConvertOptionEquityToSecurityStep1 <- function() {
	# set the fileName from which to import trades
	fileName <- "OptionEquityTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactoryStep1") 
	blRequestHandler <<- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactoryStep1(trade)	
	checkEquals(class(newSecurity)[[1]],"Opzioni_su_azioni")	
	
}
