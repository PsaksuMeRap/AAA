# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertEquityTradeToPortfolioPositions <- function() {
	
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-21_12-33-21_Riskmanager_equityTrade_confirmation.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	positions <- tradeToPositionsFactory(newPosition,trade)
	
	checkEquals(sum(positions),toMoney(0,positions[[1]]@security@currency))
	checkEquals(positions[[1]]@quantity,100000)
	checkEquals(as.numeric(positions[[1]]@value@amount),1200000)
}

test.shouldConvertFuturesOnIndexTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-21_12-33-21_Riskmanager_futureEquityIndexTrade_confirmation.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	positions <- tradeToPositionsFactory(newPosition,trade)
	
	checkEquals(positions[[1]]@value,toMoney(10*50*5000,positions[[1]]@security@currency))
	checkEquals(positions[[2]]@value,toMoney(-10*50*6000,positions[[2]]@security@currency))
}

test.shouldConvertBondTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
#	fileName <- "2012-05-21_12-33-21_Riskmanager_bondTrade_confirmation.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
#	trades <- tradesFactory(messageFileName,directory)
#	trade <- trades[[2]]

	trade <- new("Trade",list(
			Trade_number = 1,
			Portfolio = "MULTISTRATEGY",
			Buy_Sell = "Buy",
			Id_Bloomberg = "nesnvx 2 08/05/13 corp",
			Security_name = "NESTLE FINANCE INTL LTD",
			Currency = "CHF",
			Security_type = "Bond",
			Broker = "UBS",
			Quantity = 500000,
			ISIN_Ticker = "CH0049524041",
			Rating = "AA",
			Call_Put_Future = NA,
			Strike = NA,
			Maturity_Expiry = 41491,
			Accrued_interests = 1.2,
			Delivery_date = NA,
			Underlying_ticker = NA,
			Underlying_ISIN = NA,
			Contract_size = NA,
			Future_one_point_value = NA,
			Underlying_price = NA,
			Price = 101.48,
			Order_type = "Market",
			Amount = 507400,
			Limit_price = 101.5,
			Limit_date = 41239,
			Comments = NA,
			Counterparty_risk = "Claudio",
			Fund_type = NA,
			Force_data = "No",
			Security_name_1 = "NESTLE FINANCE INTL LTD",
			Confirmed_quantity = 250000,
			Confirmed_price = 101.2,
			Trade_date = 41239,
			Value_date = 41242.70395,
			Costs = NA,
			Portfolio_number = "L5000600",
			Portfolio_name = "OnCapital - DYNAMIC MULTISTRATEGY")
		)
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	positions <- tradeToPositionsFactory(newPosition,trade)
	
	checkEquals(sum(positions),toMoney(0,positions[[1]]@security@currency))
	nominalValue <- 250000
	accruedInterests <- 1.2
	price <- 101.2
	checkEquals(positions[[1]]@value,toMoney(nominalValue*(price+accruedInterests)/100,positions[[1]]@security@currency))

}

test.shouldConvertFXSpotTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-21_12-33-21_Riskmanager_fxSpotTrade_confirmation.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	positions <- tradeToPositionFactory(newSecurity,trade,blData)
	result <- tradeToPositionsFactory(positions,trade)
	
	checkEquals(positions,result)
	checkEquals(positions[[1]]@value@amount,result[[2]]@value@amount/(-1.5))
}

test.shouldConvertOptionOnEquityTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-21_12-33-21_Riskmanager_optionEquityTrade_confirmation.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	positions <- tradeToPositionsFactory(newPosition,trade)
	
	checkEquals(sum(positions),toMoney(0,positions[[1]]@security@currency))
	checkEquals(positions[[1]]@value,toMoney(100*100*1.20,positions[[1]]@security@currency))
}


test.shouldConvertOptionOnFxTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-21_12-33-21_Riskmanager_optionFxTrade_confirmation.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	positions <- tradeToPositionsFactory(newPosition,trade)
	
	checkEquals(sum(positions),toMoney(0,positions[[1]]@security@currency))
	checkEquals(positions[[1]]@value,toMoney(1000000*0.01,positions[[1]]@security@currency))
}

test.shouldConvertForwardOnFxTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-21_12-33-21_Riskmanager_fxForwardTrade_confirmation.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	positions <- tradeToPositionFactory(newSecurity,trade,blData)
	result <- tradeToPositionsFactory(positions,trade)
	
	checkEquals(positions,result)
	checkEquals(positions[[1]]@value@amount,result[[2]]@value@amount/(-1.5))
	
}

