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



