# TODO: Add comment
# 
# Author: ortellic
###############################################################################


simulate_abs_garch <- function(constant,maCoeff,arCoeff,nbObs,z_t) {
	## this function sumulates a garch(p,q) process e_t = h_t * z_t
	## h_t = c + b_1*|e_{t-1}| + ... + b_q*|e_{t-q}| + a_1*|h_{t-1}| + ... + a_p*|h_{t-p}|
	
	q <- length(maCoeff)
	p <- length(arCoeff)
	
	if (missing(nbObs)) {
		if (missing(z_t)) {
			stop ("At least nbObs or the vector z_t must be specified")
		} else {
			nbObs <- length(z_t)
		}
	}
	
	if (q!=1 | p!=1) stop("You don't need garch processes other than garch(1,1).")
	
	if (nbObs<3) stop("Too few observations ...")
	
	## create the iid N(0,1) innovations
	if (missing(z_t)) {
		z_t <- rnorm(nbObs)
	} else {
		if (length(z_t)<nbObs) z_t <- c(rnorm(nbObs-length(z_t)),z_t)
	}
	
	## compute the abs of z_t
	w_t <- abs(z_t)
	
	## compute the expectation of h_t and use it as starting point for h_t
	E_h <- constant/(1-sqrt(2/pi)*sum(maCoeff)-sum(arCoeff))
	h_t <- rep(E_h,nbObs)
	
	# simulate the series of h_t
	d <- maCoeff*w_t + arCoeff
	h_t[2:nbObs] <- constant
	
	for (i in 2:nbObs) {
		h_t[i] <- constant + d[i-1]*h_t[i-1]
	}
	
	return(h_t*z_t)
}

