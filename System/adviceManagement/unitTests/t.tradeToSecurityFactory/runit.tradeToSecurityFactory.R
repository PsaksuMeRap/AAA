# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertEquityTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToPositionFactory") 
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade)	
	checkEquals(class(newSecurity)[[1]],"Equity")
	
}

test.shouldConvertFuturesOnIndexTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToPositionFactory") 
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[3]]
	
	newSecurity <- tradeToSecurityFactory(trade)	
	checkEquals(class(newSecurity)[[1]],"Equity")
	
}