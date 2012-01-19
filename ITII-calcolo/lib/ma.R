# TODO: Add comment
# 
# Author: ortellic
###############################################################################


simulate_ma <- function(constant=1.0,maPar=numeric(0),nbObs=10,e_t=rnorm(nbObs)) {
	## constant: the constant
	## maPar: il vettore con i parametri a_i del modello ma 
	##        x_t = c + e_t + a_1*e_{t-1} + ... + a_k*e_{t-k}
	## nbObs: the number of observations
	## e_t:   the vector of innovations
	
	nbMaPar <- length(maPar)
	
	if (nbMaPar>0) e_t <- c(rep(0,nbMaPar),e_t)
	
	x_t <- constant + e_t
	
	if (nbMaPar==0) return(x_t)
	
	for (i in 1:nbMaPar) x_t <- x_t + maPar[i]*e_t
	
	return(x_t[-(1:nbMaPar)])
	
}

