# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.selectionCriteriumDefinition <- function() {

	criterium <- create_selectionCriterium()
	
	criterium1 <- create_selectionCriterium(factor="instrument",values="equity")
	
	checkEquals(class(criterium),"selectionCriterium")
	checkEquals(names(criterium),c("factor","values"))
	
	checkEquals(class(criterium1),c("instrument","selectionCriterium"))
	checkEquals(criterium1$factor,"instrument")
	checkEquals(criterium1$value,"equity")
	
}

