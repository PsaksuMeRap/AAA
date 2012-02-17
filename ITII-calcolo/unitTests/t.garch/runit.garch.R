# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.simulate_absoluteValueGarch <- function() {
		
	constant <- 1.0
	maCoeff <- c(0.1)
	arCoeff <- c(0.2)
	
	# only garch(1,1) allowed
	checkException(simulate_abs_garch(constant,maCoeff=c(1,2),arCoeff,nbObs=10))
	# foo few observations
	checkException(simulate_abs_garch(constant,maCoeff=c(1,2),arCoeff,nbObs=2))	
	# no nbObs or z_t vector specified
	checkException(simulate_abs_garch(constant,maCoeff,arCoeff))
	
	# correct simulation
	z_t <- c(1,0.5,2,-0.5,-3,2,-1,0.1,0.3,-2)
	should <- -2.60520185726637
	result <- simulate_abs_garch(constant,maCoeff,arCoeff,z_t=z_t)
	checkEquals(result[10],should)
	
	# length(z_t) < nbObs
	result <- simulate_abs_garch(constant,maCoeff,arCoeff,nbObs=12,z_t=z_t)
	checkEquals(length(result),12)
}
