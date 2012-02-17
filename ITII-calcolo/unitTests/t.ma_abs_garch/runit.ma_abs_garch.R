# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.simulate_ma_abs_garch <- function() {
	
	maConstant <- 1.0
	maCoeff <- c(2,3)
	
	garchConstant <- 1.0
	maPar <- c(0.2)
	arCoeff <- c(0.3)
	
	z_t <- c(-2,2,4,-1,2,-2,0,-4,2,-5,0,-2,5,-5)
	
	result <- simulate_ma_abs_garch(maConstant=maConstant,
			maCoeff=maCoeff,
			garchConstant=garchConstant,
			garchMaCoeff=maPar,
			garchArCoeff=arCoeff,
			nbObs=length(z_t),
			z_t=z_t
	)
	should <- -9.5381502308563
	# test 1: default values
	checkEquals(result[14],should)
	
}
