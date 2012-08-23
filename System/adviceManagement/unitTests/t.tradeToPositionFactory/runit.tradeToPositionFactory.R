# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertEquityTradeToPosition <- function() {
	
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_equityTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(newPosition)[[1]],"PositionEquity")
	checkEquals(class(newPosition@id)[[1]],"IdBloomberg")
	checkEquals(newPosition@value,toMoney(100*11.08,"CHF"))
	
}

test.shouldConvertFuturesOnIndexTradeToPosition <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_futureEquityIndexTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(newPosition)[[1]],"PositionFutures_EQ")
	checkEquals(class(newPosition@id)[[1]],"IdBloomberg")
	checkEquals(newPosition@value,toMoney(5864*10*5,"CHF"))
	
}

test.shouldConvertBondTradeToPosition <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_bondTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[2]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(newPosition)[[1]],"PositionBond")
	checkEquals(class(newPosition@id)[[1]],"IdBloomberg")
	checkEquals(newPosition@value,toMoney(0.01*(trade$Price+1.7)*trade$Quantity,"EUR"))
	checkEquals(newPosition@accruedInterest,as(toMoney(0.017*trade$Quantity,"EUR"),"AccruedInterest"))
}



test.shouldConvertFXSpotTradeToPosition <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_fxSpotTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	positions <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(positions[[1]])[[1]],"PositionConto_corrente")
	checkEquals(class(positions[[2]])[[1]],"PositionConto_corrente")
	checkEquals(class(positions[[1]]@id)[[1]],"IdBloomberg")
	checkEquals(positions[[1]]@value,toMoney(500000,"EUR"))
	checkEquals(positions[[2]]@value,toMoney(500000 * -1.201,"CHF"))
	
}

test.shouldConvertOptionOnEquityTradeToPosition <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_optionEquityTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(newPosition)[[1]],"PositionOpzioni_su_azioni")
	checkEquals(class(newPosition@id)[[1]],"IdBloomberg")
	checkEquals(newPosition@value,toMoney(0.16*100*100,"CHF"))
	checkEquals(newPosition@contractSize,100)
	checkEquals(newPosition@security@strike,55)
	checkEquals(newPosition@quantity,100)
	checkEquals(newPosition@numberEquities,10000)
	checkEquals(newPosition@security@expiryDate,"06/15/2012")
}


test.shouldConvertOptionOnFxTradeToPosition <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_optionFxTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(newPosition)[[1]],"PositionOpzioni_su_divise")
	checkEquals(class(newPosition@id)[[1]],"IdCharacter")
	checkEquals(newPosition@quantity,toMoney(trade$Quantity,"EUR"))
	checkEquals(newPosition@value,toMoney(as.numeric(trade$Amount),"CHF"))
	checkEquals(newPosition@contractSize,1)
}

test.shouldConvertForwardOnFxTradeToPosition <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_fxForwardTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	positions <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(positions)[[1]],"Positions")
	# check the first leg, the "underlying" leg
	checkEquals(class(positions[[1]])[[1]],"PositionFX_Forward")
	checkEquals(class(positions[[1]]@id)[[1]],"IdCharacter")
	checkEquals(as.character(positions[[1]]@id),"future_fx valuta 27-08-2012 EURCHF 1.1998 EUR 1'000'000.00 leg EUR")
	checkEquals(positions[[1]]@quantity,toMoney(trade$Quantity,"EUR"))

	# check the first leg, the "numeraire" leg
	checkEquals(class(positions[[2]])[[1]],"PositionFX_Forward")
	checkEquals(class(positions[[2]]@id)[[1]],"IdCharacter")
	checkEquals(as.character(positions[[2]]@id),"future_fx valuta 27-08-2012 EURCHF 1.1998 EUR 1'000'000.00 leg CHF")
	checkEquals(positions[[2]]@quantity,toMoney(-trade$Quantity*1.1998,"CHF"))
	
}

