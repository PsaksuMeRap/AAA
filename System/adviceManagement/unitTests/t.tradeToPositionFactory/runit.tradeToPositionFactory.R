# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertFondi_azionariTradeToPosition <- function() {
	
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_FundEquity_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
#	trades <- tradesFactory(messageFileName,directory)
#	trade <- trades[[1]]
	trade <- new("Trade",list(
					Trade_number = 1,
					Portfolio = "MULTISTRATEGY",
					Buy_Sell = "Sell",
					Id_Bloomberg = "WDIMIXD Equity",
					Security_name = "WORLD INVEST-ABSOL RET-C",
					Currency = "EUR",
					Security_type = "Fund equity",
					Broker = "UBS",
					Quantity = 250,
					ISIN_Ticker = "LU0028583804",
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
					Price = 244.04,
					Order_type = "Market",
					Amount = 62280,
					Limit_price = NA,
					Limit_date = 41239,
					Comments = "Test",
					Counterparty_risk = "Claudio",
					Fund_type = "UCIs",
					Force_data = "No")
	)
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(newPosition)[[1]],"PositionFondi_azionari")
	checkEquals(class(newPosition@id)[[1]],"IdAyrton")
	checkEquals(as.character(newPosition@id@idAAA),"LU0028583804")
	checkEquals(newPosition@value,toMoney(-250*244.04,"EUR"))
	
}

test.shouldConvertFundBondTradeToPosition <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
		
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_FundBond_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
	
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
#	trades <- tradesFactory(messageFileName,directory)
#	trade <- trades[[1]]

	trade <- new("Trade",list(
					Trade_number = 1,
					Portfolio = "MULTISTRATEGY",
					Buy_Sell = "Buy",
					Id_Bloomberg = "lemwbda equity",
					Security_name = "LEMANIK SICAV-FL DUR-CP R",
					Currency = "EUR",
					Security_type = "Fund bond",
					Broker = "UBS",
					Quantity = 500,
					ISIN_Ticker = "LU0146153936",
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
					Price = 1178.911,
					Order_type = "Care",
					Amount = 17462.25,
					Limit_price = NA,
					Limit_date = 41239,
					Comments = NA,
					Counterparty_risk = "Claudio",
					Fund_type = "UCITS",
					Force_data = "No")
	)
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	
	checkEquals(class(newPosition)[[1]],"PositionFondi_obbligazionari")
	checkEquals(newPosition@quantity,500)	
	checkEquals(newPosition@value,toMoney(589455.5,"EUR"))	
	checkEquals(newPosition@accruedInterest,new("AccruedInterest",toMoney(0,"EUR")))	
}



test.shouldConvertEquityTradeToPosition <- function() {
	
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_equityTrade_newAdvice.csv"
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
					Force_data = "No")
	)
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(newPosition)[[1]],"PositionEquity")
	checkEquals(class(newPosition@id)[[1]],"IdAyrton")
	checkEquals(newPosition@value,toMoney(100*11.08,"CHF"))
	
}

test.shouldConvertFuturesOnIndexTradeToPosition <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_futureEquityIndexTrade_newAdvice.csv"
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
					Force_data = "No")
	)

	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(newPosition)[[1]],"PositionFutures_EQ")
	checkEquals(class(newPosition@id)[[1]],"IdAyrton")
	checkEquals(newPosition@value,toMoney(5864*10*5,"CHF"))
	
}

test.shouldConvertBondTradeToPosition <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_bondTrade_newAdvice.csv"
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
					Quantity = 100000,
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
					Price = 102.238,
					Order_type = "Market",
					Amount = 101480,
					Limit_price = NA,
					Limit_date = 41239,
					Comments = NA,
					Counterparty_risk = "Claudio",
					Fund_type = NA,
					Force_data = "No")
	)
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(newPosition)[[1]],"PositionBond")
	checkEquals(class(newPosition@id)[[1]],"IdAyrton")
	checkEquals(newPosition@value,toMoney(0.01*(trade$Price+1.2)*trade$Quantity,"CHF"))
	checkEquals(newPosition@accruedInterest,as(toMoney(0.012*trade$Quantity,"CHF"),"AccruedInterest"))
	
}


test.shouldConvertBondTrade1ToPosition <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
#	fileName <- "2012-10-09_11-46-02_Ortelli_bondTrade1_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	
	# import trades
#	trades <- tradesFactory(messageFileName,directory)
#	trade <- trades[[1]]

	trade <- new("Trade", list(
					Trade_number = 1,
					Portfolio = "MULTISTRATEGY",
					Buy_Sell = "Buy",
					Id_Bloomberg = "xs0842560640 corp",
					Security_name = "F VAN LANSCHOT BANKIERS",
					Currency = "EUR",
					Security_type = "Bond",
					Broker = "UBS",
					Quantity = 150000,
					ISIN_Ticker = "XS0842560640",
					Rating = "BBB+",
					Call_Put_Future = NA,
					Strike = NA,
					Maturity_Expiry = 42660,
					Accrued_interests = 0.0,
					Delivery_date = NA,
					Underlying_ticker = NA,
					Underlying_ISIN = NA,
					Contract_size = NA,
					Future_one_point_value = NA,
					Underlying_price = NA,
					Price = 99.5305,
					Order_type = "Market",
					Amount = 152161.5,
					Limit_price = NA,
					Limit_date = 41239,
					Comments = NA,
					Counterparty_risk = "Claudio",
					Fund_type = NA,
					Force_data = "No")
	)
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	newPosition <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(newPosition)[[1]],"PositionBond")
	checkEquals(class(newPosition@id)[[1]],"IdAyrton")
	checkEquals(newPosition@value,toMoney(0.01*trade$Price*trade$Quantity,"EUR"))
	checkEquals(newPosition@accruedInterest,as(toMoney(0.0,"EUR"),"AccruedInterest"))
}


test.shouldConvertFXSpotTradeToPosition <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_fxSpotTrade_newAdvice.csv"
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
					Force_data = "No")
	)
	
	# create the blRequestHandler required from tradeToSecurityFactory
	blRequestHandler <- create_BloombergRequestHandler()
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)
	
	positions <- tradeToPositionFactory(newSecurity,trade,blData)
	
	checkEquals(class(positions[[1]])[[1]],"PositionConto_corrente")
	checkEquals(class(positions[[2]])[[1]],"PositionConto_corrente")
	checkEquals(class(positions[[1]]@id)[[1]],"IdAyrton")
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
	checkEquals(class(newPosition@id)[[1]],"IdAyrton")
	checkEquals(newPosition@value,toMoney(0.16*100*100,"CHF"))
	checkEquals(newPosition@contractSize,100)
	checkEquals(newPosition@security@strike,55)
	checkEquals(newPosition@quantity,100)
	checkEquals(newPosition@numberEquities,10000)
	checkEquals(newPosition@security@expiryDate,"2012-06-15")
	checkEquals(newPosition@security@name,"100 / Call / Nestle Na / 15-06-12 / Strike 55 / Premio(-1600 CHF) / CH0038863350 / 58.3 / 100")
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
	checkEquals(class(newPosition@id)[[1]],"IdAyrton")
	checkEquals(newPosition@quantity,toMoney(trade$Quantity,"EUR"))
	checkEquals(newPosition@value,toMoney(as.numeric(trade$Amount),"CHF"))
}


test.shouldConvertOptionOnFxSellTradeToPosition <- function() {
	# create the BloombergData
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_optionFxTradeSell_newAdvice.csv"
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
	checkEquals(class(newPosition@id)[[1]],"IdAyrton")
	checkEquals(newPosition@quantity,toMoney(-trade$Quantity,"EUR"))
	checkEquals(newPosition@value,toMoney(as.numeric(-trade$Amount),"USD"))
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

