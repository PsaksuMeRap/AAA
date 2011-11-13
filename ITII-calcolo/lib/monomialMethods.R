# TODO: Add comment
# 
# Author: ortellic
###############################################################################

explode <- function(what,where,with) UseMethod("explode",what)

explode.randomVariable <- function(what,where,with) {
	# what: the random variable to be injected in "where"
	# where: a monomial possibly containing what
	# with: the monomials "replacing" what 
	
	lengthRandoms = length(where$randoms) 
	if (lengthRandoms==0) return(create_monomials(where))
	
	# is the randomVariable "what" in the "where" monomial?
	areEquals <- sapply(where$randoms,"==",what)
	
	nbWhat <- sum(areEquals)
	# if no match return a monomials with "where"
	if (nbWhat==0) return(create_monomials(where))
	
	# remove "what" from where$randoms
	where$randoms[areEquals] <- NULL
	
	# create a temporary copy of with
	tmp <- with
	# if more than one "what" was found in "where" multiply tmp accordingly 
	if (nbWhat>1) for (i in 2:nbWhat) tmp <- tmp * with
	
	result <- create_monomials()
	for (i in tmp) {
		result <- result + where * i 
	}
	
	return(result)
}

explode.symbol <- function(what,where,with) {
	# what: a symbol to be injected in "where"
	# where: a monomial possibly containing what
	# with: the monomials "replacing" what 
	
	lengthSymbols = length(where$symbols) 
	if (lengthSymbols==0) return(create_monomials(where))
	
	# is the symbol "what" in the "where" monomial?
	areEquals <- sapply(where$symbols,"==",what)
	
	nbWhat <- sum(areEquals)
	# if no match return a monomials with "where"
	if (nbWhat==0) return(create_monomials(where))
	
	# remove "what" from where$randoms
	where$symbols[areEquals] <- NULL
	
	# create a temporary copy of with
	tmp <- with
	# if more than one "what" was found in "where" multiply tmp accordingly 
	if (nbWhat>1) for (i in 2:nbWhat) tmp <- tmp * with
	
	result <- create_monomials()
	for (i in tmp) {
		result <- result + where * i 
	}
	
	return(result)
}


isFirstRandomAnOddPower <- function(x,randomName) UseMethod("isFirstRandomAnOddPower",x)
# questa funzione restituisce TRUE se nella parte randoms la variabile aleatoria
# più vicina nel tempo fra tutte quelle con nome "randomName" ha potenza dispari. 

isFirstRandomAnOddPower.monomial <- function(x,randomName) {
	randoms <- x$randoms
	if (length(randoms)==0) return(FALSE)

	# identifica se c'è una variabile con nome randomName tra le variabili randoms
	isDesiredRandom <- extractFromList(randoms,"name") == randomName
	if (any(isDesiredRandom)) {
		randoms <- randoms[isDesiredRandom]
		class(randoms) <- "randomVariables"
		randoms <- sort(randoms)
		if (randoms[[1]]$power %% 2) TRUE else FALSE
	} else {
		return(FALSE)
	}
}


isFirstRandomAnOddPower.monomials <- function(x,randomName) {
	
	result <- sapply(x,isFirstRandomAnOddPower,randomName)
	return(result)
}


dropWhereFirstRandomIsOddPower <- function(x,randomName) {

	isOdd <- sapply(x,isFirstRandomAnOddPower,randomName)
	result <- x[!isOdd]
	class(result) <- "monomials"
	return(result)
}
