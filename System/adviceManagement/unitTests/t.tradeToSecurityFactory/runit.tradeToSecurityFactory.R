# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertEquityTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "equityTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Equity")
	
}

test.shouldConvertFuturesOnIndexTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "futureEquityIndexTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Futures_EQ")
	
}


test.shouldConvertBondTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "bondTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Bond")	
	
}

test.shouldConvertFxSpotToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "fxSpotTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Conto_corrente")	
	
}

test.shouldConvertOptionEquityToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "optionEquityTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Opzioni_su_azioni")	
	
}


test.shouldParseOptionFxName <- function() {
	
	name <- "eurchf  09/13/12 c1.2000"
	
	result <- parseOptionFxName(name)
	
	checkEquals(result[["name"]],"eurchf  09/13/12 c1.2000")
	checkEquals(result[["expiryDate"]],"09/13/2012")
	checkEquals(result[["optionType"]],"Call")
	checkEquals(result[["strike"]],1.2)
}


test.shouldConvertOptionFxToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "optionFxTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Opzioni_su_divise")	
	checkEquals(newSecurity@expiryDate,"09/13/2012")
	checkEquals(newSecurity@optionType,"Call")
	checkEquals(newSecurity@strike,1.2)
	checkEquals(newSecurity@name,"eurchf  09/13/12 c1.2000")
	
}
