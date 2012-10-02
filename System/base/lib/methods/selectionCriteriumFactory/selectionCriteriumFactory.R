# TODO: Add comment
# 
# Author: claudio
###############################################################################


selectionCriteriumFactory <- function(factorStringParsed) {
	
	if (identical(factorStringParsed@criterium,"security")) {
		values <- unlist(strsplit(factorStringParsed@values,","))
		values <- removeStartEndSpaces(values)
		selectionCriterium <- new("SecuritySelectionCriterium",
				values=values,
				negation=factorStringParsed@negation
		)
		return(selectionCriterium)
	}
	
	if (identical(factorStringParsed@criterium,"amount")) {
		constraint <- constraintFactory(factorStringParsed@values)
		selectionCriterium <- new("AmountSelectionCriterium",
				negation=factorStringParsed@negation,
				constraint=constraint
		)		
		return(selectionCriterium)
	}
	
	if (identical(factorStringParsed@criterium,"currency")) {
		values <- unlist(strsplit(factorStringParsed@values,","))
		values <- removeStartEndSpaces(values)
		selectionCriterium <- new("CurrencySelectionCriterium",
				values=values,
				negation=factorStringParsed@negation
		)
		return(selectionCriterium)
	}
	
	if (identical(factorStringParsed@criterium,"maturityHorizon")) {
		values <- unlist(strsplit(factorStringParsed@values,","))
		values <- removeStartEndSpaces(values)
		selectionCriterium <- new("MaturityHorizonSelectionCriterium",
				value=values,
				negation=factorStringParsed@negation
		)
		return(selectionCriterium)
	}	
	
	message <- "Error: invalid factorStringParsed.\nNo method for factorStringParsed of criterium"
	message <- paste(message," of kind '",factorStringParsed@criterium,"'",sep="")
	stop(message)
}
