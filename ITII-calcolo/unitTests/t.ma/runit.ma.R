# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.simulate_ma <- function() {
	
	nbObs <- 20
	e_t <- rnorm(20)
	
	
	# test 1: the constant and e_t, no lags
	e_t <- rnorm(10)
	checkEquals(simulate_ma(e_t=e_t),1+e_t)

	
	# test 2: the constant and e_t, no lags
	constant <- 2.5; e_t <- rnorm(10)
	checkEquals(simulate_ma(const=constant,e_t=e_t),2.5+e_t)
	
	# test 2: the constant and 2 lags
	e_t <- c(-5,2,-4,4,2,-5,-2,-3,5,0)
	maPar <- c(0.5,0.3)
	result <- simulate_ma(const=1,maPar,e_t=e_t)
	checkEquals(result[10],2.6)
}

