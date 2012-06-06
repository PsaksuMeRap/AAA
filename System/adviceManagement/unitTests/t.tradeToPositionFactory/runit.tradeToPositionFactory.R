# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertEquityTradeToPosition <- function() {
	
	# create the BloombergData
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "equityTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(newPosition)[[1]],"PositionEquity")
	checkEquals(class(newPosition@id)[[1]],"IdBloomberg")
	checkEquals(newPosition@value,toMoney(100*11.08,"CHF"))
	
}

test.shouldConvertFuturesOnIndexTradeToSecurity <- function() {
	# create the BloombergData
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "futureEquityIndexTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(newPosition)[[1]],"PositionFutures_EQ")
	checkEquals(class(newPosition@id)[[1]],"IdBloomberg")
	checkEquals(newPosition@value,toMoney(5864*10*5,"CHF"))
	
}

test.shouldConvertFXSpotTradeToSecurity <- function() {
	# create the BloombergData
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "fxSpotTrade.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(newPosition)[[1]],"PositionFutures_EQ")
	checkEquals(class(newPosition@id)[[1]],"IdBloomberg")
	checkEquals(newPosition@value,toMoney(5864*10*5,"CHF"))
	
}

#test.shouldConvertBondTradeToSecurity <- function() {
	# set the fileName from which to import trades
#	fileName <- "bondTrade.csv"
#	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
#	blRequestHandler <<- create_BloombergRequestHandler()
	
	# import trades
#	trades <- tradesFactory(fileName,directory)
#	trade <- trades[[1]]
	
#	newSecurity <- tradeToSecurityFactory(trade)	
#	checkEquals(class(newSecurity)[[1]],"Bond")	
	
#}

#test.shouldConvertFxSpotToSecurity <- function() {
	# set the fileName from which to import trades
#	fileName <- "fxSpotTrade.csv"
#	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
#	blRequestHandler <<- create_BloombergRequestHandler()
	
	# import trades
#	trades <- tradesFactory(fileName,directory)
#	trade <- trades[[1]]
	
#	newSecurity <- tradeToSecurityFactory(trade)	
#	checkEquals(class(newSecurity)[[1]],"Conto_corrente")	
	
#}

#test.shouldConvertOptionEquityToSecurity <- function() {
	# set the fileName from which to import trades
#	fileName <- "OptionEquityTrade.csv"
#	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
#	blRequestHandler <<- create_BloombergRequestHandler()
	
	# import trades
#	trades <- tradesFactory(fileName,directory)
#	trade <- trades[[1]]
	
#	newSecurity <- tradeToSecurityFactory(trade)	
#	checkEquals(class(newSecurity)[[1]],"Opzioni_su_azioni")	
	
#}
