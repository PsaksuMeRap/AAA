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
	checkEquals(result$f,as.matrix(c(1,0,0),ncol=1))
	
	# check when MA(0) e AR(0)
	maCoeff <- c(2,1)
	arOrder <- 0
	
	result <- pseudoTrueValues(maCoeff,p=arOrder)
	checkEquals(result$pseudoTrueValues,c(2))
	checkEquals(result$f,c(1))
		
	# check when MA(1) e AR(1)
	maCoeff <- c(2,1,0.4)
	arOrder <- 1
	
	result <- pseudoTrueValues(maCoeff,p=arOrder)
	# x_t = c + d*x_{t-1} + u_t
	d <- 0.4/(1+0.16)
	c <- 2*(1-d)
	checkEquals(result$pseudoTrueValues,c(c,d))
	
	# check when MA(1) e AR(2)
	maCoeff <- c(2,1,0.4)
	arOrder <- 2
	pseudoCoeff <- solve(matrix(c(1.16,0.4,0.4,1.16),ncol=2))%*%c(0.4,0)
	result <- pseudoTrueValues(maCoeff,p=arOrder)
	
	checkEquals(result$pseudoTrueValues,c(2*(1-sum(pseudoCoeff)),pseudoCoeff))

}
