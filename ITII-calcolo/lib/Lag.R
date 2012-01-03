# TODO: Add comment
# 
# Author: ortellic
###############################################################################

# questa funzione implementa l'operatore Lag
Lag <- function(x,power=1) UseMethod("Lag",x)

Lag.monomial <- function(monomial,power=1) {
	nbRandomVariables <- length(monomial$randoms)
	if (nbRandomVariables==0) return(monomial)
	x <- monomial
	for (i in 1:nbRandomVariables) x$randoms[[i]]$lag <- x$randoms[[i]]$lag + power
	return(x)
}

Lag.monomials <- function(monomials,power=1) {
	if (length(monomials)==0) return(monomials)
	result <- lapply(monomials,Lag,power=power)
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
	
	return(NA_real_)
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
	
	return(NA_real_)
}

minLag.monomials <- function(x,randomVariableName) {
	if (length(x)>0) {
		return(sapply(x,minLag,randomVariableName=randomVariableName))
	}
	
	return(numeric(0))
}


# questa funzione estrae i coefficienti del lag desiderato
extractLagCoeff <- function(x,power=1) UseMethod("extractLagCoeff",x)

extractLagCoeff.monomial <- function(x,power=1) {
	symbols <- compact(x$symbols)
	if (power==0) {
		areL <- sapply(symbols,function(x){return(x$name=="L")})
		if (any(areL)) {
			areL <- sapply(symbols,function(x,power){return(x$name=="L" & x$power==power)},power)
			if (any(areL)) {
				tmp <- symbols[!areL]
				class(tmp) <- "symbols"
				return(create_monomial(number=x$number,symbols=tmp,randoms=x$randoms))
			} else {
				return(NULL)
			}
		} else {
			return(x)
		}
	}
	areL <- sapply(symbols,function(x,power){return(x$name=="L" & x$power==power)},power)
	if (any(areL)) {
		tmp <- symbols[!areL]
		class(tmp) <- "symbols"
		return(create_monomial(number=x$number,symbols=tmp,randoms=x$randoms))
	}
	
	return(NULL)
}

