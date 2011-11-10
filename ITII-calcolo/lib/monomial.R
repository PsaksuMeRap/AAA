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
		randoms <- list(); class(randoms) <- "randomVariables"
	}
	
	monomial$randoms <- randoms
	return(monomial)
}




"+.monomial" <- function(a,b) {
	if (a$symbols==b$symbols & a$randoms==b$randoms) {
		a$number=a$number+b$number
		return(create_monomials(a))
	}	
	c <- create_monomials(a)
	c[[2]] <- b
	return(c)
	
	stop("Error in function '*.monomial': entered arguments are not valid monomial")
}



"*.monomial" <- function(a,b) {
	
	number <- a$number * b$number
	if (number==0) {
		monomial <- create_monomial(number=0)
		return(create_monomials(monomial))
	}
	symbols <- a$symbols * b$symbols
	randoms <- a$randoms * b$randoms
	
	c <- create_monomials(create_monomial(number,symbols,randoms))
	return(c)
	
	stop("Error in function '*.monomial': entered arguments are not valid monomial")
}


toString.monomial <- function(monomial) {
	if (length(monomial$symbols)==0) stringSymbols <- "" else stringSymbols <- paste("*",toString(monomial$symbols),sep="")
	if (length(monomial$randoms)==0) stringRandoms <- "" else stringRandoms <- paste("*",toString(monomial$randoms),sep="")
	result = paste(monomial$number,stringSymbols,stringRandoms,sep="")
	return(result)
}
