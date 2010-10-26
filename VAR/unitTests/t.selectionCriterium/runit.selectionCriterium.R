# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.selectionCriteriumDefinition <- function() {

	criterium <- create_selectionCriterium()
	
	criterium1 <- create_selectionCriterium(factor="instrument",values="equity")
	
	checkEquals(class(criterium),"selectionCriterium")
	checkEquals(names(criterium),c("factor","values","type"))
	
	checkEquals(class(criterium1),c("instrument","selectionCriterium"))
	checkEquals(criterium1$factor,"instrument")
	checkEquals(criterium1$values,"equity")
	checkEquals(criterium1$type,NA_character_)
	
}

