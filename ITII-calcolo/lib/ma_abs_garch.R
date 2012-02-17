# TODO: Add comment
# 
# Author: ortellic
###############################################################################


simulate_ma_abs_garch <- function(maConstant=0,maCoeff=0,garchConstant=1,garchMaCoeff=0,garchArCoeff=0,nbObs,z_t) {
	if (missing(z_t)) {
		if (missing(nbObs)) {
			stop("At least one argument between nbObs and z_t must be specified")
		} else {
			z_t <- rnorm(nbObs)
		}
	}
	
	if (missing(nbObs)) nbObs <- length(z_t)
	
	absGarch <- simulate_abs_garch(constant=garchConstant,maCoeff=garchMaCoeff,arCoeff=garchArCoeff,nbObs=nbObs,z_t=z_t)

	maAbsGarch <- simulate_ma(constant=maConstant,maCoeff=maCoeff,e_t=absGarch)
	return(maAbsGarch)
}


