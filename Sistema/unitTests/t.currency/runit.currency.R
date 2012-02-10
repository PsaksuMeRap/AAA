# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldRefuseMultipleCurrency <- function() {

	checkException(new("Currency",c("a","b")))

}

test.shouldFailOnInvalidCurrency <- function() {
	
	checkException(new("Currency","a"))
	
}