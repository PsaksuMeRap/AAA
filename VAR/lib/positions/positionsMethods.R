# TODO: Add comment
# 
# Author: claudio
###############################################################################

positionsSelector <- function(criteriumSelection,positions,...) UseMethod("positionsSelector",criteriumSelection)

positionsSelector.currency <- function(criteriumSelection,positions) {
	if (!is.element("positions",class(positions))) stop("The argument is not of class positions")

	FUNC <- function(position,criteriumSelection) {
		check <- is.element(position$money$currency,criteriumSelection$values)
		if (criteriumSelection$negation) return(!check)
		return(check)
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
		check <- is.element(classes[index],criteriumSelection$values)
		if (criteriumSelection$negation) return(!check)
		return(check)
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
		percentageValue <- criteriumSelection$criteriumCheck$value/100
		criteriumNew$criteriumCheck$value <- toMoney(percentageValue*totalValue$amount,totalValue$currency)
	}
	
	# apply the function FUNC
	extract <- lapply(positions$positions,FUN=check,criteriumNew)
	extract <- unlist(extract)
	
	return(extract)
}

positionsSelector.maturityHorizon <- function(criteriumSelection,positions,...) {
	# extract the baseDate from the ... arguments
	x <- list(...)
	if (length(x)==0) baseDate = Sys.Date() else baseDate=as.Date(x[[1]])
	
	FUNC <- function(position,criteriumSelection,baseDate) {
		
		values <- criteriumSelection$values
		
		# attenzione: poiché una position accruedInterest è anche bond
		# occorre testare prima accruedInterest
		if (is.element("accruedInterest",class(position))) {
			if (values=="<3Y") return(TRUE) else return(FALSE)
		}
		
		if (is.element("Fondi_mercato_monetario",class(position))) {
			if (values=="<3Y") return(TRUE) else return(FALSE)
		}
		
		if (is.element("bond",class(position))) {
			bondMaturity <- as.Date(position$getMaturity())
			maturityInYears <- as.integer(bondMaturity - baseDate)/365
			if (maturityInYears <= 3) maturityHorizon <- "<3Y"
			if (maturityInYears > 3) maturityHorizon <- ">3Y"
			if (maturityHorizon == values[1]) return(TRUE) else return(FALSE)
		}

		if (is.element("Fondi_obbligazionari",class(position))) {
			if (grepl("<3Y",x=position$name)) {
				averageHorizon = "<3Y"
			} else {
				if (grepl(">3Y",x=position$name)) {
					averageHorizon = ">3Y"
				} else {
					stop("errore il fondo obbligazionario non ha un indice di durata <3Y o >3Y")
				}
			}
			
			if (averageHorizon == values[1]) return(TRUE) else return(FALSE)
		}
		
		if (is.element("Strutturati_FI",class(position))) {
			if (position$underlyingHorizon == values[1]) return(TRUE) else return(FALSE)
		}		
		return(NA)
	}
	# apply the function FUNC
	extract <- lapply(positions$positions,FUN=FUNC,criteriumSelection,baseDate)
	extract <- unlist(extract)
	
	# convert NA to FALSE
	extract[is.na(extract)] <- FALSE
	
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



extractPositionsFromSelectionString <- function(selectionString,positions) {

	# selectionString: a string of type "instrument:bond,equity & currency:USD + amount:>5%"
	parser <- create_parserSelectionCriteria()

	unionOfBlocksOfCriteria <- parser$splitSelectionString(selectionString)
	result <- filterByCriteriaLogicalOr(unionOfBlocksOfCriteria,positions)
	
	# crea la lista delle posizioni
	positionsFiltered <- create_positions()
	lapply(positions$positions[result],positionsFiltered$addPosition)
	return(positionsFiltered)
}

checkCheckStringOnPositions <- function(checkString,positions,logFile,refCurrency) {
	# calcola il valore totale delle posizioni
	if (missing(refCurrency)) positionsValue <- positions$sum() else positionsValue <- positions$sum(refCurrency)

	# parsa ed estrai le posizioni soddisfacenti i criteri di selezione
	parser <- create_parserSelectionCriteria()
	parsed <- parser$splitCheckString(checkString)
	extractedPositions <- extractPositionsFromSelectionString(parsed[["selectionString"]],positions)
	
	# crea il criterio di selezione per la verifica del vincolo finale
	criteriumSelection <- create_criteriumSelection(factor="amount",
			criteriumCheck=parsed[["criteriumCheck"]]
	)
	
	if (criteriumSelection$criteriumCheck$kind=="relative") {
		percentageValue <- criteriumSelection$criteriumCheck$value/100
		criteriumSelection$criteriumCheck$value <- toMoney(percentageValue*positionsValue$amount,positionsValue$currency)
	}
	
	if (missing(refCurrency)) extractedPositionsValue <- extractedPositions$sum() else extractedPositionsValue <- extractedPositions$sum(refCurrency)
	
	fakePosition <- create_position()
	fakePosition$create(name="fake",currency=extractedPositionsValue$currency,
			amount=extractedPositionsValue$amount) 
	
	checkResult <- check(fakePosition,criteriumSelection)
	if (!missing(logFile)) {

		cat(paste("check:",checkResult,"->", checkString),
				file=logFile,sep="\n",append=TRUE)
		
		positionsToBePrinted  <- extractedPositions$toString()
	
		for (p in positionsToBePrinted) {	
			cat(paste("      ",p), file=logFile, sep="\n",append=TRUE)
		}
	
		#for (position in extractedPositions$positions) {	
		#	cat(paste("      ",position$toString()),file=logFile,
		#			sep="\n",append=TRUE)
		#}
		
		effectivePercentage <- extractedPositionsValue$divide(positionsValue)*100
		effectivePercentage <- paste(formatC(effectivePercentage,digits=2,
						format="f"),"%",sep="")
		
		cat(paste("Total:",extractedPositionsValue$toString(), "over", 
						positionsValue$toString(), 
						"(",effectivePercentage,")","\n"),file=logFile,sep="\n",append=TRUE)
	}
	return( checkResult )
	
}

identifyFundsToExplode <- function(fundData,positions) {
	# fundData: una lista con tre campi: nomeFondo, numeroValore, owner
	# una variabile di classe positions
	
	# return a vector with the positions matching the numeroValore field
	
	nbPositions <- length(positions$positions)
	if (nbPositions==0) return (logical(0))
	
	isFundToExplode <- function(position,fundData) {
		
		return(fundData[["numeroValore"]] == position$origin[["NumeroValore"]])
	}
	
	result <- sapply(positions$positions,isFundToExplode,fundData)
	return(result)		
}


identifyCB_Accent_Lux_sicav_FIXED_INCOME_oacc <- function(positions) {

	nbPositions <- length(positions$positions)
	if (nbPositions==0) return (logical(0))
	
	# identify CB-Accent Lux sicav - fixed Income
	isAccentLuxFixedIncome <- identifyFundsToExplode(list("numeroValore"="2490099"),positions)
	
	# identify accrued interest
	isAccruedInterest <- function(position) {is.element("accruedInterest",class(position))}
	result <- sapply(positions$positions,isAccruedInterest) & isAccentLuxFixedIncome
	
	return(result)
}


weightPositions <- function(positions,weight) {
	return(invisible(lapply(positions$positions,weightPosition,weight)))
}

areConsistent <- function(positions) {
	isConsistent <- function(position) {
		return(position$isConsistent())
	}
	
	areConsistent <- lapply(positions$positions,isConsistent)
	return(areConsistent)
}

# analizza i nomi e guarda se funzionano correttamente. crea un repository per le
# criteriumClass con i rispettivi valori? Esempio
# instruments: equity, bond, ...
# currency: usd, chf, eur
# geograficArea: EU, USA, Canada, Asia, Japan

