# TODO: Add comment
# 
# Author: claudio
###############################################################################

create_riskFactor <- function(subclass, attributes=list(),money) {
	riskFactor <- list()
	
	class(riskFactor) <- "riskFactor"
	
	riskFactor$factor <- subclass
	riskFactor$attributes <- attributes
	riskFactor$money <- money
	
	return(riskFactor)
}


determine_riskFactors <- function(position) UseMethod("determine_riskFactors",position)


determine_riskFactors.equity <- function(position) {
	# assign the currency risk factor
	subclass <- "currency"
	attributes <- list(currency=position$money$currency)
	money <- toMoney(amount=position$money$amount,currency=position$money$currency)
	currencyRiskFactor <- create_riskFactor(subclass,attributes,money)
	
	# assign the price risk factor	
	subclass <- "price"
	attributes <- list(ticker=position$ticker)
	money <- toMoney(amount=position$money$amount,currency=position$money$currency)
	priceRiskFactor <- create_riskFactor(subclass,attributes,money)
	
	return(list(currencyRiskFactor,priceRiskFactor))
}

determine_riskFactors.ETF_equity <- function(position) {
	# assign the currency risk factor
	subclass <- "currency"
	attributes <- list(currency=position$money$currency)
	money <- toMoney(amount=position$money$amount,currency=position$money$currency)
	currencyRiskFactor <- create_riskFactor(subclass,attributes,money)
	
	# assign the price risk factor	
	subclass <- "price"
	attributes <- list(ticker=position$ticker)
	money <- toMoney(amount=position$money$amount,currency=position$money$currency)
	priceRiskFactor <- create_riskFactor(subclass,attributes,money)
	
	return(list(currencyRiskFactor,priceRiskFactor))
}


determine_riskFactors.Conto_corrente <- function(position) {
	
	# assign the currency risk factor
	subclass <- "currency"
	attributes <- list(currency=position$money$currency)
	money <- toMoney(amount=position$money$amount,currency=position$money$currency)
	riskFactor <- create_riskFactor(subclass,attributes,money)
	
	return(list(riskFactor))
}


determine_riskFactor.default <- function(position) {
	stop("Method determine_riskFactor not implemented for position of class",class(position)[1])
}

