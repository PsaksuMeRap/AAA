# TODO: Add comment
# 
# Author: claudio
###############################################################################


derive <- function(x,wrt="L") UseMethod("derive",x)

derive.symbol <- function(x,wrt="L") {
	# x: a symbol
	# wrt: "with respect to", i.e. the variable name wrt derive
	
	if (x$name!=wrt) return(NULL)
	
	checkEquals(symbol$name,"a")
	checkEquals(symbol$power,1)
	
}

	symbol <- create_symbol(name="L",power=3)


}