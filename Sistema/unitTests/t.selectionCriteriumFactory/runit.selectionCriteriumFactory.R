# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.selectionCriteriumFactory <- function() {
	
	selectionCriteriumFactory <- function(parsedFactorString) {
		
		if (identical(parsedFactorString@criterium,"amount")) {
			constraint <- constraintFactory(parsedFactorString@values)
			selectionCriterium <- new("AmountSelectionCriterium",
					values=values,
					negation=negation,
					constraint=constraint
			)		
			
			return(selectionCriterium)
		}
		
		if (identical(parsedFactorString@criterium,"currency")) {
			selectionCriterium <- new("CurrencySelectionCriterium",
					values=parsedFactorString@values,
					negation=negation
			)
			return(selectionCriterium)
		}
		
		if (identical(parsedFactorString@criterium,"maturityHorizon")) {
			selectionCriterium <- new("MaturityHorizonSelectionCriterium",
					value=parsedFactorString@values,
					negation=negation
			)
			return(selectionCriterium)
		}	
		
		if (identical(parsedFactorString@criterium,"security")) {
			selectionCriterium <- new("SecuritySelectionCriterium",
					values=parsedFactorString@values,
					negation=negation
			)
			return(selectionCriterium)
		}	
	}
	
	# crea una checkString e verifica i 4
	checkString = paste(" instrument:bond,equity + currency:JPY,EUR,USD +",
			"+ amount:<=100.3CHF")
	checkString <- new("CheckString",checkString)
	result <- split(checkString)
	
	checkEquals(unclass(result[["constraintString"]]),NA_character_)
	checkEquals(unclass(result[["selectionString"]]),"instrument:bond & currency:JPY + instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	checkEquals(unclass(result[["directiveString"]]),NA_character_)
	
}
