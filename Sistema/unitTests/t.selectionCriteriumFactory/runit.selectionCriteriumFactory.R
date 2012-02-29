# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.createSelectionCriterium <- function() {
	
	selectionCriteriumFactory <- function(parsedFactorString) {
		if (identical(parsedFactorString@criterium"amount")) {
			criteriumCheck <- parser$constructCriteriumCheck(values)
			criteriumSelection <- create_criteriumSelection(
					factor=factor,
					values=values,
					criteriumCheck=criteriumCheck
			)
		} else {
			criteriumSelection <- create_criteriumSelection(
					factor=factor,
					values=values,
					negation=negation
			)		
		}
		
		
	}
	
}
