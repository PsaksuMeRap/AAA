# TODO: Add comment
# 
# Author: claudio
###############################################################################


check <- function(position,criteriumSelection) UseMethod("check",criteriumSelection)

check.amount <- function(position,criteriumSelection) {
	operator <- criteriumSelection$criteriumCheck$operator
	# if criteriumSelection$criteriumCheck$kind=="relative", we assume that 
	# criteriumSelection$criteriumCheck$value has been previously converted from % to absolute, i.e.
	# criteriumSelection$criteriumCheck$value is now of class money!
	
	# exchange the money in the desired currency
	currencyTo <- criteriumSelection$criteriumCheck$value$currency
	money <- repositories$exchangeRates$exchange(position$money,currencyTo)
	
	# excecute the check
	if (operator==">" ) return(money$amount >  criteriumSelection$criteriumCheck$value$amount)
	if (operator==">=") return(money$amount >= criteriumSelection$criteriumCheck$value$amount)
	if (operator=="<" ) return(money$amount <  criteriumSelection$criteriumCheck$value$amount)
	if (operator=="<=") return(money$amount <= criteriumSelection$criteriumCheck$value$amount)
	if (operator=="!=") return(money$amount != criteriumSelection$criteriumCheck$value$amount)
	if (operator=="=" ) {
		if (abs(money$amount-criteriumSelection$criteriumCheck$value$amount) > 0.00001) return(FALSE) else return(TRUE)
	}
	stop (paste("Error: invalid operator",operator))
}

check.instrument <- function(position,criteriumSelection) {
	instruments <- criteriumSelection$values
	instrumentType <- class(position)[1]
	return(any(is.element(instrumentType,instruments)))
}

check.currency <- function(position,criteriumSelection) {
	currencies <- criteriumSelection$values
	instrumentCurrency <- position$money$currency
	return(any(is.element(instrumentCurrency,currencies)))
}

check.default <- function(position,criteriumSelection) {
	stop(paste("Error: check for factor",criteriumSelection$factor,"not implemented yet"))
}

