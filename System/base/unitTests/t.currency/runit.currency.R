# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldFailOnInvalidCurrency <- function() {
	
	checkException(new("Currency","a"))
	
}

test.shouldConvertAs.character <- function() {
	a <- new("Currency","USD")
	
	checkEquals(identical(as.character(a),"USD"),TRUE)
	
}