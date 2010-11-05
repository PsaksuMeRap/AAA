# TODO: Add comment
# 
# Author: claudio
###############################################################################

positionsSelector <- function(criterium,positions) UseMethod("positionsSelector",criterium)

positionsSelector.currency <- function(criterium,positions) {
	if (!is.element("positions",class(positions))) stop("The argument is not of class positions")

	FUNC <- function(position,criterium) {
		is.element(position$currency,criterium$values)	
	}
	
	# apply the function FUNC
	extract <- lapply(positions$positions,FUNC,criterium)
	extract <- unlist(extract)

	return(extract)
}

positionsSelector.instrument <- function(criterium,positions) {
	if (!is.element("positions",class(positions))) stop("The argument is not of class positions")
	# the identifier is at position length(classes)-1
	FUNC <- function(position,criterium) {
		classes <- class(position)
		# the identifier of the position is at "length(classes)-1"
		index <- length(classes)-1
		isElement <- is.element(classes[index],criterium$values)
		return(any(isElement))
	}
	
	# apply the function FUNC
	extract <- lapply(positions$positions,FUNC,criterium)
	extract <- unlist(extract)
	
	return(extract)
}

positionsSelector.amount <- function(criterium,positions) {
	if (!any(is.element("criteriumSelection",class(criterium)))) stop("Selector.amount: the argument criterium is not of class criteriumSelection")
	if (class(positions)!="positions") stop("Selector.amount: the argument positions is not of class positions")
	
	criteriumNew <- criterium

	if (criterium$criteriumCheck$kind=="relative") {
		totalValue <- positions$sum()
		percentageValue <- criterium$criteriumCheck$value
		criteriumNew$criteriumCheck$value <- toMoney(percentageValue*totalValue$amount,totalValue$currency)
	}
	
	# apply the function FUNC
	extract <- lapply(positions$positions,FUN=check,criteriumNew)
	extract <- unlist(extract)
	
	return(extract)
}

positionsSelector.default <- function(criterium) {
	print(criterium)
}


check <- function(position,criterium) UseMethod("check",criterium)

check.amount <- function(position,criterium) {
	operator <- criterium$criteriumCheck$operator
	# if criterium$criteriumCheck$kind=="relative", we assume that 
	# criterium$criteriumCheck$value has been previously converted from % to absolute, i.e.
	# criterium$criteriumCheck$value is now of class money!
		
	# exchange the money in the desired currency
	currencyTo <- criterium$criteriumCheck$value$currency
	moneyToExchange <- toMoney(position$amount,position$currency)
	money <- repositories$exchangeRates$exchange(moneyToExchange,currencyTo)

	# excecute the check
	if (operator==">" ) return(money$amount >  criterium$criteriumCheck$value$amount)
	if (operator==">=") return(money$amount >= criterium$criteriumCheck$value$amount)
	if (operator=="<" ) return(money$amount <  criterium$criteriumCheck$value$amount)
	if (operator=="<=") return(money$amount <= criterium$criteriumCheck$value$amount)
	if (operator=="!=") return(money$amount != criterium$criteriumCheck$value$amount)
	if (operator=="=" ) {
		if (abs(money$amount-criterium$criteriumCheck$value$amount) > 0.00001) return(FALSE) else return(TRUE)
	}
	stop (paste("Error: invalid operator",operator))
}

check.instrument <- function(position,criterium) {
	
	instruments <- criterium$values
	instrumentType <- class(position)[1]
	return(any(is.element(instrumentType,instruments)))
}

check.currency <- function(position,criterium) {
	
	currencies <- criterium$values
	instrumentCurrency <- position$currency
	return(any(is.element(instrumentCurrency,currencies)))
}

check.default <- function(position,criterium) {
	stop(paste("Error: check for factor",criterium$factor,"not implemented yet"))
}



# analizza i nomi e guarda se funzionano correttamente. crea un repository per le
# criteriumClass con i rispettivi valori? Esempio
# instruments: equity, bond, ...
# currency: usd, chf, eur
# geograficArea: EU, USA, Canada, Asia, Japan



