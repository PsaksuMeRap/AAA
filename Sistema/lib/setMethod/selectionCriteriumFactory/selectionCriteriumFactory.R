# TODO: Add comment
# 
# Author: claudio
###############################################################################


selectionCriteriumFactory <- function(parsedFactorString) {
	
	if (identical(parsedFactorString@criterium,"security")) {
		values <- unlist(strsplit(parsedFactorString@values,","))
		values <- removeStartEndSpaces(values)
		selectionCriterium <- new("SecuritySelectionCriterium",
				values=values,
				negation=parsedFactorString@negation
		)
		return(selectionCriterium)
	}
	
	if (identical(parsedFactorString@criterium,"amount")) {
		constraint <- constraintFactory(parsedFactorString@values)
		selectionCriterium <- new("AmountSelectionCriterium",
				negation=parsedFactorString@negation,
				constraint=constraint
		)		
		return(selectionCriterium)
	}
	
	if (identical(parsedFactorString@criterium,"currency")) {
		values <- unlist(strsplit(parsedFactorString@values,","))
		values <- removeStartEndSpaces(values)
		selectionCriterium <- new("CurrencySelectionCriterium",
				values=values,
				negation=parsedFactorString@negation
		)
		return(selectionCriterium)
	}
	
	if (identical(parsedFactorString@criterium,"maturityHorizon")) {
		values <- unlist(strsplit(parsedFactorString@values,","))
		values <- removeStartEndSpaces(values)
		selectionCriterium <- new("MaturityHorizonSelectionCriterium",
				value=values,
				negation=parsedFactorString@negation
		)
		return(selectionCriterium)
	}	
	
	message <- "Error: invalid parsedFactorString: no method for parsedFactorString of criterium"
	message <- paste(message," of kind '",parsedFactorString@criterium,"'",sep="")
	stop(message)
}
