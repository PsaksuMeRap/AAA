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
#	fileName <- "2012-05-21_12-33-21_Riskmanager_equityTrade_confirmation.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
#	trades <- tradesFactory(messageFileName,directory)
#	trade <- trades[[1]]

	trade <- new("Trade",list(
					Trade_number = 1,
					Portfolio = "FIXED INCOME",
					Buy_Sell = "Buy",
					Id_Bloomberg = "UBSN VX Equity",
					Security_name = "UBS AG-REG",
					Currency = "CHF",
					Security_type = "Equity",
					Broker = "UBS",
					Quantity = 100,
					ISIN_Ticker = "CH0024899483",
					Rating = NA,
					Call_Put_Future = NA,
					Strike = NA,
					Maturity_Expiry = NA,
					Accrued_interests = NA,
					Delivery_date = NA,
					Underlying_ticker = NA,
					Underlying_ISIN = NA,
					Contract_size = NA,
					Future_one_point_value = NA,
					Underlying_price = NA,
					Price = 11.08,
					Order_type = "Limited",
					Amount = 7290,
					Limit_price = 12.5,
					Limit_date = 41239,
					Comments = NA,
					Counterparty_risk = "Claudio",
					Fund_type = NA,
					Force_data = "No",
					Security_name_1 = "NESTLE FINANCE INTL LTD",
					Confirmed_quantity = 100000,
					Confirmed_price = 12,
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
	checkEquals(positions[[1]]@quantity,100000)
	checkEquals(as.numeric(positions[[1]]@value@amount),1200000)
}

test.shouldConvertFuturesOnIndexTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
#	fileName <- "2012-05-21_12-33-21_Riskmanager_futureEquityIndexTrade_confirmation.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
#	trades <- tradesFactory(messageFileName,directory)
#	trade <- trades[[1]]

	trade <- new("Trade",list(
					Trade_number = 1,
					Portfolio = "MULTISTRATEGY",
					Buy_Sell = "Buy to open",
					Id_Bloomberg = "SMM2 Index",
					Security_name = "SWISS MKT IX FUTR Jun12",
					Currency = "CHF",
					Security_type = "Future equity index",
					Broker = "UBS",
					Quantity = 5,
					ISIN_Ticker = NA,
					Rating = NA,
					Call_Put_Future = "Future",
					Strike = NA,
					Maturity_Expiry = NA,
					Accrued_interests = NA,
					Delivery_date = 41449,
					Underlying_ticker = NA,
					Underlying_ISIN = "US81663N2062",
					Contract_size = NA,
					Future_one_point_value = 10,
					Underlying_price = NA,
					Price = 5864,
					Order_type = "Market",
					Amount = 978750,
					Limit_price = NA,
					Limit_date = "11/11/2012",
					Comments = NA,
					Counterparty_risk = "Claudio",
					Fund_type = NA,
					Force_data = "No",
					Security_name_1 = "NESTLE FINANCE INTL LTD",
					Confirmed_quantity = 50,
					Confirmed_price = 6000,
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
	
	checkEquals(positions[[1]]@value,toMoney(10*50*5864,positions[[1]]@security@currency))
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
#	fileName <- "2012-05-21_12-33-21_Riskmanager_fxSpotTrade_confirmation.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
#	trades <- tradesFactory(messageFileName,directory)
#	trade <- trades[[1]]

	trade <- new("Trade", list(
					Trade_number = 1,
					Portfolio = "MULTISTRATEGY",
					Buy_Sell = "Buy",
					Id_Bloomberg = "EURCHF Curncy",
					Security_name = "EUR-CHF X-RATE",
					Currency = "CHF",
					Security_type = "FX spot",
					Broker = "UBS",
					Quantity = 500000,
					ISIN_Ticker = NA,
					Rating = NA,
					Call_Put_Future = NA,
					Strike = NA,
					Maturity_Expiry = NA,
					Accrued_interests = NA,
					Delivery_date = NA,
					Underlying_ticker = "EUR",
					Underlying_ISIN = NA,
					Contract_size = NA,
					Future_one_point_value = NA,
					Underlying_price = NA,
					Price = 1.201,
					Order_type = "Limited",
					Amount = 1806315,
					Limit_price = 1.2015,
					Limit_date = 41239,
					Comments = NA,
					Counterparty_risk = "Claudio",
					Fund_type = NA,
					Force_data = "No",
					Security_name_1 = "",
					Confirmed_quantity = 500000,
					Confirmed_price = 1.5,
					Trade_date = 41239,
					Value_date = 41242.70395,
					Costs = NA,
					Portfolio_number = "L5000600",
					Portfolio_name = "OnCapital - DYNAMIC MULTISTRATEGY")
	)
	
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
#	fileName <- "2012-05-21_12-33-21_Riskmanager_optionEquityTrade_confirmation.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
#	trades <- tradesFactory(messageFileName,directory)
#	trade <- trades[[1]]

	trade <- new("Trade", list(
					Trade_number = 1,
					Portfolio = "MULTISTRATEGY",
					Buy_Sell = "Buy to open",
					Id_Bloomberg = "nesn sw 06/15/12 c55 equity",
					Security_name = "June 12 Calls on NESN VX",
					Currency = "CHF",
					Security_type = "Option equity",
					Broker = "UBS",
					Quantity = 150,
					ISIN_Ticker = NA,
					Rating = NA,
					Call_Put_Future = "Call",
					Strike = 55,
					Maturity_Expiry = 41293,
					Accrued_interests = NA,
					Delivery_date = NA,
					Underlying_ticker = "NESN VX",
					Underlying_ISIN = "CH0038863350",
					Contract_size = 100,
					Future_one_point_value = NA,
					Underlying_price = 58.30,
					Price = 0.16,
					Order_type = "Care",
					Amount = NA,
					Limit_price = NA,
					Limit_date = 41239,
					Comments = NA,
					Counterparty_risk = "Claudio",
					Fund_type = NA,
					Force_data = "No",
					Security_name_1 = "",
					Confirmed_quantity = 120,
					Confirmed_price = 1.5,
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
	checkEquals(positions[[1]]@value,toMoney(100*120*1.50,positions[[1]]@security@currency))
}


test.shouldConvertOptionOnFxTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
#	fileName <- "2012-05-21_12-33-21_Riskmanager_optionFxTrade_confirmation.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
#	trades <- tradesFactory(messageFileName,directory)
#	trade <- trades[[1]]

	trade <- new("Trade", list(
					Trade_number = 1,
					Portfolio = "MULTISTRATEGY",
					Buy_Sell = "Buy to close",
					Id_Bloomberg = "EURCHF 09/12/13 c1.2",
					Security_name = "EURCHF 09/12/13 c1.2",
					Currency = "CHF",
					Security_type = "Option FX",
					Broker = "UBS",
					Quantity = 100000,
					ISIN_Ticker = NA,
					Rating = NA,
					Call_Put_Future = "Call",
					Strike = 1.2,
					Maturity_Expiry = 41529,
					Accrued_interests = NA,
					Delivery_date = NA,
					Underlying_ticker = "EUR",
					Underlying_ISIN = NA,
					Contract_size = NA,
					Future_one_point_value = NA,
					Underlying_price = NA,
					Price = 0.025,
					Order_type = "Limited",
					Amount = 2500,
					Limit_price = 0.0255,
					Limit_date = 41239,
					Comments = NA,
					Counterparty_risk = "Claudio",
					Fund_type = NA,
					Force_data = "No",
					Security_name_1 = "",
					Confirmed_quantity = 1000000,
					Confirmed_price = 0.01,
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
	checkEquals(positions[[1]]@value,toMoney(1000000*0.01,positions[[1]]@security@currency))
}

test.shouldConvertForwardOnFxTradeToPortfolioPositions <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
#	fileName <- "2012-05-21_12-33-21_Riskmanager_fxForwardTrade_confirmation.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
#	trades <- tradesFactory(messageFileName,directory)
#	trade <- trades[[1]]

	trade <- new("Trade",list(
					Trade_number = 1,
					Portfolio = "MULTISTRATEGY",
					Buy_Sell = "Buy to open",
					Id_Bloomberg = "EUR/CHF 082713X112612 BGNL Curncy",
					Security_name = "EUR/CHF R  8/27/2013",
					Currency = "CHF",
					Security_type = "FX forward",
					Broker = "UBS",
					Quantity = 1000000,
					ISIN_Ticker = NA,
					Rating = NA,
					Call_Put_Future = NA,
					Strike = NA,
					Maturity_Expiry = NA,
					Accrued_interests = NA,
					Delivery_date = NA,
					Underlying_ticker = "EUR",
					Underlying_ISIN = NA,
					Contract_size = NA,
					Future_one_point_value = NA,
					Underlying_price = NA,
					Price = 1.2019161,
					Order_type = "Market",
					Amount = 1201916.1,
					Limit_price = NA,
					Limit_date = 41239,
					Comments = NA,
					Counterparty_risk = "Claudio",
					Fund_type = NA,
					Force_data = "No",
					Security_name_1 = "",
					Confirmed_quantity = 1500000,
					Confirmed_price = 1.5,
					Trade_date = 41239,
					Value_date = 41242.70395,
					Costs = NA,
					Portfolio_number = "L5000600",
					Portfolio_name = "OnCapital - DYNAMIC MULTISTRATEGY")
	)
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	positions <- tradeToPositionFactory(newSecurity,trade,blData)
	result <- tradeToPositionsFactory(positions,trade)
	
	checkEquals(positions,result)
	checkEquals(positions[[1]]@value@amount,result[[2]]@value@amount/(-1.5))
	checkEquals(as.numeric(positions[[1]]@value@amount),1500000)	
}

