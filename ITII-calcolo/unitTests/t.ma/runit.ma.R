# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.simulate_ma <- function() {
	
	maPar <- c(1.0,1.0,0.5)
	nbObs <- 20
	e_t <- rnorm(20)
	
	
	# test 1: the constant and e_t, no lags
	e_t <- rnorm(10)
	checkEquals(simulate_ma(e_t=e_t),1+e_t)

	
	# test 2: the constant and e_t, no lags
	constant <- 2.5; e_t <- rnorm(10)
	checkEquals(simulate_ma(const=constant,e_t=e_t),2.5+e_t)
	
	
	# test 3: the constant and e_t, with one lag
	constant=5; maPar <- c(-1); e_t <- rnorm(10)
	checkEquals(simulate_ma(maPar,e_t=e_t),rep(1,10))
	
}

test.simulate_ma_abs_garch <- function() {
	
	maConstant <- 1.0
	maPar <- c(1.0,1.0,0.5)
	nbObs <- 20
	e_t <- rnorm(20)
	
	constant <- 1.0
	maCoeff <- c(0.1)
	arCoeff <- c(0.2)
	
	e <- simulate_abs_garch(nbObs=6e+05,constant,maCoeff,arCoeff)
	
	# test 1: default values
	checkEquals(simulate_ma(),rep(0,10))
	
	# test 2: the constant only
	maPar <- c(1.0,0)
	checkEquals(simulate_ma(maPar),rep(1,10))
	
	# test 3: the constant and e_t, no lags
	maPar <- c(1.0,1.0); e_t <- rnorm(10)
	checkEquals(simulate_ma(maPar,e_t=e_t),1+e_t)
	
	
	# test 4: the constant and e_t, wit one lag
	maPar <- c(1.0,1.0,-1); e_t <- rnorm(10)
	checkEquals(simulate_ma(maPar,e_t=e_t),rep(1,10))
	
}