# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.derive.symbol <- function()  {
	
	# check 1: derive constant
	a1 <- create_symbol(name="a1",power=4)
	result <- derive(a1)
	checkEquals(result,create_monomial(number=0))
	
	a1 <- create_symbol(name="L",power=4)
	result <- derive(a1)
	checkEquals(result$number,4)
	checkEquals(result$symbols[[1]]$name,"L")
	checkEquals(result$symbols[[1]]$power,3)
}