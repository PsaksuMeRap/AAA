# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.simulate_absoluteValueGarch <- function() {
		
	constant <- 1.0
	maCoeff <- c(0.1)
	arCoeff <- c(0.2)
	
	e <- simulate_abs_garch(nbObs=6e+05,constant,maCoeff,arCoeff)
	deviation <- abs(mean(abs(e)/sqrt(2/pi)) - constant/(1-sqrt(2/pi)*sum(maCoeff)-sum(arCoeff)))
	checkEquals(deviation<0.01,TRUE)
}
