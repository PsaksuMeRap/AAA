# TODO: Add comment
# 
# Author: ortellic
###############################################################################


create_monomial <- function(number=1,symbols=NULL,randoms=NULL) {
	monomial <- list()
	class(monomial) <- "monomial"
	
	monomial$number <- number
	
	if (is.null(symbols)) {
		symbols <- list(); class(symbols) <- "symbols"
	}
	
	monomial$symbols <- symbols
	
	if (is.null(randoms)) {	
		randoms <- list(); class(randoms) <- "randoms"
	}
	
	monomial$randoms <- randoms
	return(monomial)
}


create_polynomial <- function(monomial=NULL) {
	
	polynomial <- list()
	class(polynomial) <- "polynomial"
	
	if (is.null(monomial)) monomial <- create_monomial()
	polynomial[[1]] <- monomial
	return(polynomial)
}

