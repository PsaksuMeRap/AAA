# TODO: Add comment
# 
# Author: claudio
###############################################################################


filterByCriteriaLogicalAnd <- function(criteria,positions) {
	# criteria: a list of criteriumSelection
	# positions: a variable of class positions
	
	if (length(positions$positions)==0) return(vector(mode = "logical"))
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
	
	if (length(positions$positions)==0) return(vector(mode = "logical"))
	
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
#E qui che va modificata la funzione filterByCriteria	
	result <- filterByCriteriaLogicalOr(unionOfBlocksOfCriteria,positions)
	
	# crea la lista delle posizioni
	positionsFiltered <- create_positions()
	if (length(positions$positions)>0) lapply(positions$positions[result],positionsFiltered$addPosition)
	return(positionsFiltered)
}

checkCheckStringOnPositions <- function(checkString,positions,logFile,refCurrency) {
	# calcola il valore totale delle posizioni
	if (missing(refCurrency)) positionsValue <- positions$sum() else positionsValue <- positions$sum(refCurrency)

	# parsa ed estrai le posizioni soddisfacenti i criteri di selezione
	parser <- create_parserSelectionCriteria()
	parsed <- parser$splitCheckString(checkString)
	
	# utilizza la directiveString
	positions <- applyDirectiveString(parsed[["directiveString"]],positions)
	
	extractedPositions <- extractPositionsFromSelectionString(parsed[["selectionString"]],positions)
	
	# crea il criterio di selezione per la verifica del vincolo finale
	criteriumSelection <- create_criteriumSelection(factor="amount",
			criteriumCheck=parsed[["criteriumCheck"]]
	)
	
	# crea il valore assoluto da verificare se il check è relativo mentre se il tipo
	# di vincolo è assoluto crea il valore limite percentuale da stampare assieme 
	# a quello effettivo nel summary
	if (criteriumSelection$criteriumCheck$kind=="relative") {
		percentageValue <- criteriumSelection$criteriumCheck$value/100
		criteriumSelection$criteriumCheck$value <- toMoney(percentageValue*positionsValue$amount,positionsValue$currency)
	} else {
	# in questo caso criteriumSelection$criteriumCheck$value è una variabile di tipo money
		percentageValue <- criteriumSelection$criteriumCheck$value$divide(positionsValue)
	}
	percentageValue <- paste(formatC(percentageValue*100,digits=2,
					format="f"),"%",sep="")
	
	
	if (missing(refCurrency)) extractedPositionsValue <- extractedPositions$sum() else extractedPositionsValue <- extractedPositions$sum(refCurrency)
	
	fakePosition <- create_position()
	fakePosition$create(name="fake",currency=extractedPositionsValue$currency,
			amount=extractedPositionsValue$amount) 
	
	actualPercentage <- extractedPositionsValue$divide(positionsValue)*100
	actualPercentage <- paste(formatC(actualPercentage,digits=2,
					format="f"),"%",sep="")
	
	checkResult <- check(fakePosition,criteriumSelection)
	result <- list()
	result$checkString <- checkString
	result$checkResult <- checkResult
	result$percentageValue <- percentageValue
	result$actualPercentage <- actualPercentage
	
	if (!missing(logFile)) {

		cat(paste("check:",checkResult,"->", checkString),
				file=logFile,sep="\n",append=TRUE)
		
		positionsToBePrinted  <- extractedPositions$toString()
		result$positions <- positionsToBePrinted
		
		for (p in positionsToBePrinted) {	
			cat(paste("      ",p), file=logFile, sep="\n",append=TRUE)
		}
		
		cat(paste("Total:",extractedPositionsValue$toString(), "over", 
						positionsValue$toString(), 
						"(",actualPercentage,")","\n"),file=logFile,sep="\n",append=TRUE)
	}

	# return( checkResult )
	return( result )
}

identifyFundsToExplode <- function(fundData,positions) {
	# fundData: una lista con 4 campi: nomeFondo, instrumentClass, id, owner
	# una variabile di classe positions
	
	# return a vector with the positions matching the id field

	nbPositions <- length(positions$positions)
	if (nbPositions==0) return (logical(0))
	
	isFundToExplode <- function(position,fundData) {
		
		id <- position$id
		if (is.null(id)) return(FALSE)
		if (is.na(id))   return(FALSE)

		return(fundData[["id"]]==id & fundData[["instrumentClass"]]==class(position)[1])
	}
	
	result <- sapply(positions$positions,isFundToExplode,fundData)
	return(result)		
}


identifyCB_Accent_Lux_sicav_FIXED_INCOME_oacc <- function(positions) {

	nbPositions <- length(positions$positions)
	if (nbPositions==0) return (logical(0))

	# identify accrued interest
	identifyOacc <- function(position) {
		id <- position$id
		if (is.null(id)) return(FALSE)
		if (is.na(id))   return(FALSE)
		isOk <- is.element("accruedInterest",class(position)) & id==825
		return(isOk)
	}
	result <- sapply(positions$positions,identifyOacc)
	
	return(result)
}


weightPositions <- function(positions,weight) {
	return(invisible(lapply(positions$positions,weightPosition,weight)))
}

areConsistent <- function(positions) {
	isConsistent <- function(position) {
		return(position$isConsistent())
	}
	
	areConsistent <- sapply(positions$positions,isConsistent)
	return(areConsistent)
}


applyDirectiveString <- function(directiveString,positions)
{
	if (class(positions)!="positions") stop("Function 'applyDirectiveString: positions argument not of class positions")
	
	if (is.na(directiveString)) return(positions)
	if (directiveString!="explode:Fondi_misti") stop("directiveString other than explode:Fondi_misti not implemented yet.")
	
	# create newPositions
	newPositions <- create_positions()
	
	for (position in positions$positions) {
		newPositions$add(explode(position))
	}
	return(newPositions)
}


# analizza i nomi e guarda se funzionano correttamente. crea un repository per le
# criteriumClass con i rispettivi valori? Esempio
# instruments: equity, bond, ...
# currency: usd, chf, eur
# geograficArea: EU, USA, Canada, Asia, Japan

