# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldConvertFundBondTradeToSecurity <- function() {
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_FundBond_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
	
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
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
	checkEquals(class(newSecurity)[[1]],"Fondi_obbligazionari")
	checkEquals(newSecurity@name,"20201231 - 0% >3Y - LEMANIK SICAV-FL DUR-CP R")	
	checkEquals(newSecurity@maturity,"2020-12-31")	
	checkEquals(newSecurity@currency,new("Currency","EUR"))	
}


test.shouldConvertFundEquityTradeToSecurity <- function() {
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_FundEquity_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
	
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
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
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Fondi_azionari")
	
}


test.shouldConvertEquityTradeToSecurity <- function() {
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_equityTrade_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
	
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
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
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Equity")
	
}

test.shouldConvertFuturesOnIndexTradeToSecurity <- function() {
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_futureEquityIndexTrade_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
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
#	fileName <- "2012-05-09_14-22-24_Ortelli_fxSpotTrade_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
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
			Quantity = 1500000,
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
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Conto_corrente")	
	
}

test.shouldConvertOptionEquityTradeToSecurity <- function() {
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_optionEquityTrade_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
#	trades <- tradesFactory(messageFileName,directory)
#	trade <- trades[[1]]

	trade <- new("Trade", list(
			Trade_number = 1,
			Portfolio = "MULTISTRATEGY",
			Buy_Sell = "Buy to open",
			Id_Bloomberg = "MSFT US 01/19/13 C26 Equity",
			Security_name = "January 13 Calls on MSFT US",
			Currency = "USD",
			Security_type = "Option equity",
			Broker = "UBS",
			Quantity = 150,
			ISIN_Ticker = NA,
			Rating = NA,
			Call_Put_Future = "Call",
			Strike = 26,
			Maturity_Expiry = 41293,
			Accrued_interests = NA,
			Delivery_date = NA,
			Underlying_ticker = "MSFT US",
			Underlying_ISIN = "US5949181045",
			Contract_size = 100,
			Future_one_point_value = NA,
			Underlying_price = NA,
			Price = 1.73,
			Order_type = "Care",
			Amount = 25950,
			Limit_price = NA,
			Limit_date = 41239,
			Comments = NA,
			Counterparty_risk = "Claudio",
			Fund_type = NA,
			Force_data = "No")
		)
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Opzioni_su_azioni")	
	
}


test.shouldParseOptionFxName <- function() {
	
	name <- "eurchf 09/13/12 c1.2000"
	
	result <- parseOptionFxName(name)
	
	checkEquals(result[["name"]],"eurchf 09/13/12 c1.2000")
	checkEquals(result[["expiryDate"]],"2012-09-13")
	checkEquals(result[["optionType"]],"C")
	checkEquals(result[["strike"]],1.2)
	checkEquals(result[["underlying"]],"EUR")
	checkEquals(result[["numeraire"]],"CHF")
}


test.shouldConvertOptionFxTradeToSecurity <- function() {
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_optionFxTrade_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
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
			Force_data = "No")
		)
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Opzioni_su_divise")	
	checkEquals(newSecurity@expiryDate,"2013-09-12")
	checkEquals(newSecurity@optionType,"C")
	checkEquals(newSecurity@strike,1.2)
	checkEquals(newSecurity@name,"EURCHF 09/12/13 c1.2")
	
}

test.shouldConvertOptionFxTradeSellToSecurity <- function() {
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_optionFxTradeSell_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
	# import trades
#	trades <- tradesFactory(messageFileName,directory)
#	trade <- trades[[1]]

	trade <- new("Trade", list(
					Trade_number = 1,
					Portfolio = "MULTISTRATEGY",
					Buy_Sell = "Sell to close",
					Id_Bloomberg = "EURCHF 09/12/13 c1.3",
					Security_name = "EURCHF 09/12/13 c1.3",
					Currency = "CHF",
					Security_type = "Option FX",
					Broker = "UBS",
					Quantity = 100000,
					ISIN_Ticker = NA,
					Rating = NA,
					Call_Put_Future = "Call",
					Strike = 1.3,
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
					Force_data = "No")
	)
	
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"Opzioni_su_divise")	
	checkEquals(newSecurity@expiryDate,"2013-09-12")
	checkEquals(newSecurity@optionType,"C")
	checkEquals(newSecurity@strike,1.3)
	checkEquals(newSecurity@name,"EURCHF 09/12/13 c1.3")
	
}

test.shouldParseFxForwardName <- function() {
	
	name <- "EUR/CHF 082713X112612 BGNL Curncy"	
	info <- parseFxForwardName(name)
	checkEquals(info[["currencyCodes"]],"EURCHF")
	checkEquals(info[["deliveryDate"]],"08/27/2013")
	checkEquals(info[["underlying"]],"EUR")
	checkEquals(info[["numeraire"]],"CHF")
}


test.shouldConvertFxForwardTradeToSecurity <- function() {
	# set the fileName from which to import trades
#	fileName <- "2012-05-09_14-22-24_Ortelli_fxForwardTrade_newAdvice.csv"
#	messageFileName <- messageFileNameFactory(fileName)
#	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeToSecurityFactory") 
	blRequestHandler <- create_BloombergRequestHandler()
	
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
			Force_data = "No")
		)
	newSecurity <- tradeToSecurityFactory(trade,blRequestHandler)	
	checkEquals(class(newSecurity)[[1]],"FX_Forward")	
	checkEquals(newSecurity@name,"EURCHF 08/27/2013")
	
}
