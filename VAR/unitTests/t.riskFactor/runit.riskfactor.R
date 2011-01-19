# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateRiskFactor <- function() {
	
	subclass <- "currency"
	attributes <- list(currency="usd")
	money <- toMoney(amount=10,currency="usd")
	
	riskFactor <- create_riskFactor(subclass,attributes,money)
	checkEquals(class(riskFactor),"riskFactor")
	checkEquals(riskFactor$factor,"currency")
	checkEquals(riskFactor$attributes,list(currency="usd"))
	checkEquals(riskFactor$money,toMoney(amount=10,currency="usd"))
		
}

test.determine_riskFactor_equity <- function() {
	source("./unitTests/utilities/createInstrumentsForTests.R")	
	source("./lib/riskfactor.R")
	
	# create an equity
	positions <- create_equityTestPositions()
	position <- positions$positions[[1]]

	
	riskFactors <- determine_riskFactors(position)
	
	checkEquals(riskFactors[[1]]$factor,"currency")
	checkEquals(riskFactors[[1]]$money,toMoney(amount=position$amount,currency=position$currency))	
	
	checkEquals(riskFactors[[2]]$factor,"price")
	checkEquals(riskFactors[[2]]$money,toMoney(amount=position$amount,currency=position$currency))

}

test.determine_riskFactor_Conto_corrente <- function() {
	source("./unitTests/utilities/createInstrumentsForTests.R")	
	source("./lib/riskfactor.R")

	# create a Conto_corrente	
	positions <- create_Conto_correnteTestPositions()
    position <- positions$positions[[1]]
	
	
	riskFactors <- determine_riskFactors(position)
	
	
	checkEquals(riskFactors[[1]]$factor,"currency")
	checkEquals(riskFactors[[1]]$money,toMoney(amount=position$amount,currency=position$currency))	
	checkEquals(length(riskFactors),1)
	
}

test.determine_riskFactor_ETF_equity <- function() {
	source("./unitTests/utilities/createInstrumentsForTests.R")	
	source("./lib/riskfactor.R")
	
	# create an equity
	positions <- create_ETF_equityTestPositions()
	position <- positions$positions[[2]]
	
	riskFactors <- determine_riskFactors(position)
	
	checkEquals(riskFactors[[1]]$factor,"currency")
	checkEquals(riskFactors[[1]]$money,toMoney(amount=position$amount,currency=position$currency))	
	
	checkEquals(riskFactors[[2]]$factor,"price")
	checkEquals(riskFactors[[2]]$money,toMoney(amount=position$amount,currency=position$currency))
	
}

test.determine_riskFactor_FX_Forward <- function() {
	source("./unitTests/utilities/createInstrumentsForTests.R")	
	source("./lib/riskfactor.R")
	
	# create a FX_Forward (one leg only)
	
	positions <- create_FX_ForwardTestPositions()
	position <- positions$positions[[2]]
	
	# riskFactors <- determine_riskFactors(position)
	
	# assign the currency risk factor
	subclass <- "currency"
	attributes <- list(currency=position$currency)
	money <- toMoney(amount=position$amount,currency=position$currency)
	
	moneyRiskFactor <- create_riskFactor(subclass,attributes,money)
	
	# assign the interest rate risk factor

	subclass <- "interestRate"
	attributes <- list(currency=position$currency,expiry=position$expiry)
	money <- toMoney(amount=position$amount,currency=position$currency)
	
	interestRateRiskFactor <- create_riskFactor(subclass,attributes,money)
	riskFactors <- list(moneyRiskFactor,interestRateRiskFactor)
	
	checkEquals(riskFactors[[1]]$factor,"currency")
	checkEquals(riskFactors[[2]]$money,toMoney(amount=position$amount,currency=position$currency))	
	
	checkEquals(riskFactors[[2]]$factor,"interestRate")
	checkEquals(riskFactors[[2]]$attributes,list(currency=position$currency,expiry=position$expiry))
	
}

