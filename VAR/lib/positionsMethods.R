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
	
	if (class(criterium$values)!="money") stop("Selector.amount: the argument is not of class money")
	
	FUNC <- function(position,criterium) {
		# exchange the money in the desired currency
		currencyTo <- criterium$values$currency
		moneyToExchange <- toMoney(position$amount,position$currency)
		money <- repositories$exchangeRates$exchange(moneyToExchange,currencyTo)
		if (criterium$type==">")  return(money$amount >  criterium$values$amount)
		if (criterium$type==">=") return(money$amount >= criterium$values$amount)
		if (criterium$type=="<")  return(money$amount <  criterium$values$amount)
		if (criterium$type=="<=") return(money$amount <= criterium$values$amount)
		if (criterium$type=="==") return(money$amount == criterium$values$amount)
		if (criterium$type=="!=") return(money$amount != criterium$values$amount)
	}
	
	# apply the function FUNC
	extract <- lapply(positions$positions,FUNC,criterium)
	extract <- unlist(extract)
	
	return(extract)
}

positionsSelector.default <- function(criterium) {
	print(criterium)
}

# analizza i nomi e guarda se funzionano correttamente. crea un repository per le
# criteriumClass con i rispettivi valori? Esempio
# instruments: equity, bond, ...
# currency: usd, chf, eur
# geograficArea: EU, USA, Canada, Asia, Japan



