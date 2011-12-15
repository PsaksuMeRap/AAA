# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.criteriumSelectionDefinition <- function() {
	
	criterium <- create_criteriumSelection()
	
	criterium1 <- create_criteriumSelection(factor="instrument",values="equity")
	
	checkEquals(class(criterium),"criteriumSelection")
	checkEquals(names(criterium),c("factor","values","negation","criteriumCheck"))
	
	checkEquals(class(criterium1),c("instrument","criteriumSelection"))
	checkEquals(criterium1$factor,"instrument")
	checkEquals(criterium1$values,"equity")
	checkEquals(criterium1$negation,FALSE)
	checkEquals(criterium1$criteriumCheck,NA)
	
}


