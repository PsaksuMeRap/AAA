# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertEquityTradeToPortfolioPositions <- function() {
	
	# create the BloombergData
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-21_12-33-21_Riskmanager_equityTrade_confirmation.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	positions <- tradeToPositionsFactory(newPosition,trade)
	
	checkEquals(sum(positions),toMoney(0,positions[[1]]@security@currency))
	
}

test.shouldConvertFuturesOnIndexTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_futureEquityIndexTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	positions <- tradeToPositionsFactory(newPosition,trade)
	
	checkEquals(sum(positions),toMoney(0,positions[[1]]@security@currency))
	
}

test.shouldConvertBondTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_bondTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[2]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	positions <- tradeToPositionsFactory(newPosition,trade)
	
	checkEquals(sum(positions),toMoney(0,positions[[1]]@security@currency))

}

test.shouldConvertFXSpotTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_fxSpotTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	positions <- tradeToPositionFactory(newSecurity,trade,blData)
	result <- tradeToPositionsFactory(positions,trade)
	
	checkEquals(positions,result)
	checkEquals(positions[[1]]@value@amount,result[[2]]@value@amount/(-1.201))
}

test.shouldConvertOptionOnEquityTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_optionEquityTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	positions <- tradeToPositionsFactory(newPosition,trade)
	
	checkEquals(sum(positions),toMoney(0,positions[[1]]@security@currency))
	
}


test.shouldConvertOptionOnFxTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_optionFxTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	positions <- tradeToPositionsFactory(newPosition,trade)
	
	checkEquals(sum(positions),toMoney(0,positions[[1]]@security@currency))
}

test.shouldConvertForwardOnFxTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_fxForwardTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	positions <- tradeToPositionFactory(newSecurity,trade,blData)
	result <- tradeToPositionsFactory(positions,trade)
	
	checkEquals(positions,result)
	checkEquals(positions[[1]]@value@amount,result[[2]]@value@amount/(-1.1998))
	
}

