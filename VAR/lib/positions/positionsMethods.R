# TODO: Add comment
# 
# Author: claudio
###############################################################################

positionsSelector <- function(criteriumSelection,positions) UseMethod("positionsSelector",criteriumSelection)

positionsSelector.currency <- function(criteriumSelection,positions) {
	if (!is.element("positions",class(positions))) stop("The argument is not of class positions")

	FUNC <- function(position,criteriumSelection) {
		is.element(position$currency,criteriumSelection$values)	
	}
	
	# apply the function FUNC
	extract <- lapply(positions$positions,FUNC,criteriumSelection)
	extract <- unlist(extract)

	return(extract)
}

positionsSelector.instrument <- function(criteriumSelection,positions) {
	if (!is.element("positions",class(positions))) stop("The argument is not of class positions")
	# the identifier is at position length(classes)-1
	FUNC <- function(position,criteriumSelection) {
		classes <- class(position)
		# the identifier of the position is at "length(classes)-1"
		index <- length(classes)-1
		isElement <- is.element(classes[index],criteriumSelection$values)
		return(any(isElement))
	}
	
	# apply the function FUNC
	extract <- lapply(positions$positions,FUNC,criteriumSelection)
	extract <- unlist(extract)
	
	return(extract)
}

positionsSelector.amount <- function(criteriumSelection,positions) {
	if (!any(is.element("criteriumSelection",class(criteriumSelection)))) stop("Selector.amount: the argument criteriumSelection is not of class criteriumSelection")
	if (class(positions)!="positions") stop("Selector.amount: the argument positions is not of class positions")
	
	criteriumNew <- criteriumSelection

	if (criteriumSelection$criteriumCheck$kind=="relative") {
		totalValue <- positions$sum()
		percentageValue <- criteriumSelection$criteriumCheck$value
		criteriumNew$criteriumCheck$value <- toMoney(percentageValue*totalValue$amount,totalValue$currency)
	}
	
	# apply the function FUNC
	extract <- lapply(positions$positions,FUN=check,criteriumNew)
	extract <- unlist(extract)
	
	return(extract)
}

positionsSelector.default <- function(criteriumSelection) {
	print(criteriumSelection)
}


check <- function(position,criteriumSelection) UseMethod("check",criteriumSelection)

check.amount <- function(position,criteriumSelection) {
	operator <- criteriumSelection$criteriumCheck$operator
	# if criteriumSelection$criteriumCheck$kind=="relative", we assume that 
	# criteriumSelection$criteriumCheck$value has been previously converted from % to absolute, i.e.
	# criteriumSelection$criteriumCheck$value is now of class money!
		
	# exchange the money in the desired currency
	currencyTo <- criteriumSelection$criteriumCheck$value$currency
	moneyToExchange <- toMoney(position$amount,position$currency)
	money <- repositories$exchangeRates$exchange(moneyToExchange,currencyTo)

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
	instrumentCurrency <- position$currency
	return(any(is.element(instrumentCurrency,currencies)))
}

check.default <- function(position,criteriumSelection) {
	stop(paste("Error: check for factor",criteriumSelection$factor,"not implemented yet"))
}


filterByCriteriaLogicalAnd <- function(criteria,positions) {
	# criteria: a list of criteriumSelection
	# positions: a variable of class positions
	
	result <- lapply(criteria,positionsSelector,positions)
	
	if (length(result)==0) return(result)
	if (length(result)==1) return(result[[1]])
	
	x <- result[[1]]
	for (r in result[-1]) x <- x & r
	return(x)
}


filterByCriteriaLogicalOr <- function(unionOfBlocksOfCriteria,positions) {
	# criteria: a list of criteriumSelection
	# positions: a variable of class positions
	
	result <- lapply(unionOfBlocksOfCriteria,filterByCriteriaLogicalAnd,positions)
	
	if (length(result)==0) return(result)
	if (length(result)==1) return(result[[1]])
	
	x <- result[[1]]
	for (r in result[-1]) x <- x | r
	return(x)
}


extractFromSelectionString <- function(selectionString,positions) {
	# selectionString: a string of type "instrument:bond,equity & currency:USD + amount:>5%"
	parser <- create_parserSelectionCriteria()

	unionOfBlocksOfCriteria <- parser$splitSelectionString(selectionString)
	result <- filterByCriteriaLogicalOr(unionOfBlocksOfCriteria,positions)
	
	# crea la lista delle posizioni
	positionsFiltered <- create_positions()
	lapply(positions$positions[result],positionsFiltered$addPosition)
	return(positionsFiltered)
}


# analizza i nomi e guarda se funzionano correttamente. crea un repository per le
# criteriumClass con i rispettivi valori? Esempio
# instruments: equity, bond, ...
# currency: usd, chf, eur
# geograficArea: EU, USA, Canada, Asia, Japan



