# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldConvertFundBondTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_FundBond_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Fondi_obbligazionari")
	checkEquals(newSecurity@name,"20201231 - 0% >3Y - LEMANIK SICAV-FL DUR-CP R")	
	checkEquals(newSecurity@maturity,"2020-12-31")	
	checkEquals(newSecurity@currency,new("Currency","EUR"))	
}


test.shouldConvertFundEquityTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_FundEquity_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Fondi_azionari")
	
}


test.shouldConvertEquityTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_equityTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
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
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Futures_EQ")
	
}


test.shouldConvertBondTradeToSecurity <- function() {
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_bondTrade_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trade <- new("Trade",list(
			Trade_number = 1,
			Portfolio = "GLOBAL EQUITY",
			Buy_Sell = "Buy",
			Id_Bloomberg = "nesnvx 2 08/05/13 corp",
			Security_name = "NESTLE FINANCE INTL LTD",
			Currency = "CHF",
			Security_type = "Bond",
			Broker = "UBS",
			Quantity = 200000,
			ISIN_Ticker = "CH0049524041",
			Rating = "AA",
			Maturity_Expiry = 41491,
			Accrued_interests = 0.57777778,
			Delivery_date = NA,
			Underlying_ticker = NA,
			Underlying_ISIN = NA,
			Contract_size = NA,
			Future_one_point_value = NA,
			Underlying_price = NA,
			Price = 101.594,
			Order_type = NA,
			Amount = 203188,
			Limit_price = NA,
			Limit_date = 41227,
			Comments = NA,
			Counterparty_risk = "Claudio",
			Fund_type = NA,
			Force_data = "No")
		)
	#trades <- tradesFactory(messageFileName,directory)
	#trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Bond")	
	
}

test.shouldConvertBondTrade1ToSecurity <- function() {
	# set the fileName from which to import trades
#	fileName <- "2012-10-09_11-46-02_Ortelli_bondTrade1_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
#	trades <- tradesFactory(messageFileName,directory)
#	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Bond")	
	
}


test.shouldParseFxSpotId_Bloomberg <- function() {
	Id_Bloomberg <- "usdchf curncy"
	result <- parseFxSpotId_Bloomberg(Id_Bloomberg)
	
	checkEquals(result[["underlying"]],"USD")	
	checkEquals(result[["numeraire"]],"CHF")
}

test.shouldConvertFxSpotTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_fxSpotTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
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
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
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
	checkEquals(result[["expiryDate"]],"2012-09-13")
	checkEquals(result[["optionType"]],"C")
	checkEquals(result[["strike"]],1.2)
	checkEquals(result[["underlying"]],"EUR")
	checkEquals(result[["numeraire"]],"CHF")
}


test.shouldConvertOptionFxTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_optionFxTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Opzioni_su_divise")	
	checkEquals(newSecurity@expiryDate,"2012-09-13")
	checkEquals(newSecurity@optionType,"C")
	checkEquals(newSecurity@strike,1.2)
	checkEquals(newSecurity@name,"eurchf  09/13/12 c1.2000")
	
}

test.shouldConvertOptionFxTradeSellToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_optionFxTradeSell_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
	trades <- tradesFactory(messageFileName,directory)
	trade <- trades[[1]]
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Opzioni_su_divise")	
	checkEquals(newSecurity@expiryDate,"2013-09-12")
	checkEquals(newSecurity@optionType,"C")
	checkEquals(newSecurity@strike,1.3)
	checkEquals(newSecurity@name,"EURCHF 09/12/13 c1.3")
	
}

test.shouldParseFxForwardName <- function() {
	
	name <- "eurchf 08/27/12"	
	info <- parseFxForwardName(name)
	checkEquals(info[["currencyCodes"]],"EURCHF")
	checkEquals(info[["deliveryDate"]],"08/27/2012")
	checkEquals(info[["underlying"]],"EUR")
	checkEquals(info[["numeraire"]],"CHF")
}


test.shouldConvertFxForwardTradeToSecurity <- function() {
	# set the fileName from which to import trades
	fileName <- "2012-05-09_14-22-24_Ortelli_fxForwardTrade_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
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
