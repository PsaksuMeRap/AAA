# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldFailOnMultipleCurrency <- function() {

	checkException(new("Currency",c("a","b")))

}

test.shouldFailOnInvalidCurrency <- function() {
	
	checkException(new("Currency","a"))
	
}

test.shouldConvertAs.character <- function() {
	a <- new("Currency","USD")
	
	checkEquals(identical(as.character(a),"USD"),TRUE)
	
}