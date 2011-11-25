# TODO: Add comment
# 
# Author: ortellic
###############################################################################


E_w <- function(power) {
	## questa funzione restituisce il momento E(w^power), w=|z| dove z ~ N(0,1)
	if (!power) return(1.0) # se power == 0 return 1.0
	
	if (power==1) return(sqrt(2/pi))
	
	moments <- 1.0:power
	moments[1:2] <- c(sqrt(2/pi),1)
	
	for (i in moments[-(1:2)]) {
		moments[i] <- (i-1) * moments[i-2]
	}
	
	return(moments[power])
}

E_z <- function(power) {
	## questa funzione restituisce il momento E(z^power) dove z ~ N(0,1)
	if (!power) return(1.0) # se power == 0 return 1.0
	
	if (power==1) return(0.0)
	
	moments <- 1.0:power
	moments[1:2] <- c(0.0,1.0)
	
	for (i in moments[-(1:2)]) {
		moments[i] <- (i-1) * moments[i-2]
	}
	
	return(moments[power])
}


