# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.pseudoTrueValuesMA_AR <- function() {
	
	# check when MA(0)
	maCoeff <- c(2,1)
	arOrder <- 2
	
	result <- pseudoTrueValues(maCoeff,p=arOrder)
	checkEquals(result$pseudoTrueValues,c(2,0,0))
	checkEquals(result$b,c(1,0,0))
	
	# check when MA(0) e AR(0)
	maCoeff <- c(2,1)
	arOrder <- 0
	
	result <- pseudoTrueValues(maCoeff,p=arOrder)
	checkEquals(result$pseudoTrueValues,c(2))
	checkEquals(result$b,c(1))
	
	checkEquals(TRUE,FALSE) # da continuare qui
	# check when MA(1) e AR(1)
	maCoeff <- c(2,1)
	arOrder <- 0
	
	result <- pseudoTrueValues(maCoeff,p=arOrder)
	checkEquals(result$pseudoTrueValues,c(2))
	checkEquals(result$b,c(1))
	
	# check when MA(1) e AR(2)
	maCoeff <- c(2,1)
	arOrder <- 0
	
	result <- pseudoTrueValues(maCoeff,p=arOrder)
	checkEquals(result$pseudoTrueValues,c(2))
	checkEquals(result$b,c(1))
}
