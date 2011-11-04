# TODO: Add comment
# 
# Author: ortellic
###############################################################################


create_randomVariable <- function(name="epsilon",timeIndex=0,power=1) {
	randomVariable <- create_symbol(name=name,power=power)
	randomVariable$timeIndex = timeIndex
	
	
	class(randomVariable) <- c("RV",class(randomVariable))
	return(randomVariable)
}

