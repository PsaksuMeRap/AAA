# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.criteriumCheckDefinition <- function() {
	
	criterium <- create_criteriumCheck()
	
	criterium1 <- create_criteriumCheck(operator=">",value=44.5,kind="absolute")
	
	checkEquals(class(criterium),"criteriumCheck")
	checkEquals(names(criterium),c("operator","value","kind"))
	
	checkEquals(class(criterium1),"criteriumCheck")
	checkEquals(criterium1$operator,">")
	checkEquals(criterium1$value,44.5)
	checkEquals(criterium1$kind,"absolute")
	
}
