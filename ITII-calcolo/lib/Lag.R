# TODO: Add comment
# 
# Author: ortellic
###############################################################################

# questa funzione implementa l'operatore Lag
Lag <- function(x,lags=1) UseMethod("Lag",x)

Lag.monomial <- function(monomial,lags=1) {
	nbRandomVariables <- length(monomial$randoms)
	if (nbRandomVariables==0) return(monomial)
	x <- monomial
	for (i in 1:nbRandomVariables) x$randoms[[i]]$lag <- x$randoms[[i]]$lag + lags
	return(x)
}

Lag.monomials <- function(monomials,lags=1) {
	if (length(monomials)==0) return(monomials)
	result <- lapply(monomials,Lag,lags=lags)
	class(result) <- "monomials"
	return(result)
}


# questa funzione restituisce il ritardo massimo della variabile aleatoria di nome randomVariableName
maxLag <- function(x,randomVariableName) UseMethod("maxLag",x)

maxLag.monomial <- function(x,randomVariableName) {
	if (length(x$randoms)>0) {
		nomi <- extractFromList(x$randoms,"name")
		isOk <- is.element(nomi,randomVariableName)
		if (any(isOk)) {
			lags <- extractFromList(x$randoms,"lag")
			return(max(lags[isOk]))
		}
	}
	
	return(NA_integer_)
}

maxLag.monomials <- function(x,randomVariableName) {
	if (length(x)>0) {
		return(sapply(x,maxLag,randomVariableName=randomVariableName))
	}
	
	return(numeric(0))
}


# questa funziona restituisce il ritardo minimo della variabile aleatoria di nome randomVariableName
minLag <- function(x,randomVariableName) UseMethod("minLag",x)

minLag.monomial <- function(x,randomVariableName) {
	if (length(x$randoms)>0) {
		nomi <- extractFromList(x$randoms,"name")
		isOk <- is.element(nomi,randomVariableName)
		if (any(isOk)) {
			lags <- extractFromList(x$randoms,"lag")
			return(min(lags[isOk]))
		}
	}
	
	return(NA_integer_)
}

minLag.monomials <- function(x,randomVariableName) {
	if (length(x)>0) {
		return(sapply(x,minLag,randomVariableName=randomVariableName))
	}
	
	return(numeric(0))
}
