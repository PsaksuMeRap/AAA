# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertEquityTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_equityTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Equity")
	
}

test.shouldConvertFuturesOnIndexTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_futureEquityIndexTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Futures_EQ")
	
}


test.shouldConvertBondTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_bondTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Bond")	
	
}

test.shouldConvertFxSpotTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_fxSpotTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Conto_corrente")	
	
}

test.shouldConvertOptionEquityTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_optionEquityTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
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
	checkEquals(result[["underlying"]],"EUR")
	checkEquals(result[["numeraire"]],"CHF")
}


test.shouldConvertOptionFxTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_optionFxTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Opzioni_su_divise")	
	checkEquals(newSecurity@expiryDate,"09/13/2012")
	checkEquals(newSecurity@optionType,"Call")
	checkEquals(newSecurity@strike,1.2)
	checkEquals(newSecurity@name,"eurchf  09/13/12 c1.2000")
	
}


test.shouldParseFxForwardName <- function() {
	
	name <- "eurchf 08/27/12"	
	info <- parseFxForwardName(name)
	checkEquals(info[["currencyCodes"]],"EURCHF")
	checkEquals(info[["settlementDate"]],"08/27/2012")
	checkEquals(info[["underlying"]],"EUR")
	checkEquals(info[["numeraire"]],"CHF")
}


test.shouldConvertFxForwardTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_fxForwardTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"FX_Forward")	
	checkEquals(newSecurity@name,"EURCHF 08/27/2012")
	
	# trade$Currency <- "USD"
	# newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
}
