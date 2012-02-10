# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldCreateAyrtonPositions <- function() {
	
	a <- list("uno","due",tre=1:10)
	
	checkEquals(is(new("AyrtonPositions",a),"AyrtonPositions"),TRUE)
}
