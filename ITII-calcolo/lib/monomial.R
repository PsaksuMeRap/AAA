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
		if (a$number==0) return(create_monomials()) else return(create_monomials(a))
	}	
	c <- create_monomials(a)
	c[[2]] <- b
	return(c)
}

# eval(parse(text=toString(monomial)))

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


specialMultiplicationPaste <- function(x,y) {
	if (x=="") return(y)
	if (y=="") return(x)
	return(paste(x,"*",y,sep=""))
}


toString.monomial <- function(monomial) {
	lengthSymbols <- length(monomial$symbols)
	lengthRandoms <- length(monomial$randoms)
	
	
	if (monomial$number==1) {
		if (lengthSymbols + lengthRandoms == 0) stringNumber <- "1" else stringNumber <- ""
	} else {
		stringNumber <- as.character(monomial$number)
	}
	
	result <- specialMultiplicationPaste(toString(monomial$symbols),toString(monomial$randoms))
	
	if (result=="") {
		return(stringNumber) 
	} else {
		specialMultiplicationPaste(stringNumber,result)
	}
}


sort.monomial <- function(x) {
	# x: a monomial whose symbols and randoms must be ordered
	result <- x
	if (length(x$symbols)!=0) result$symbols <- sort(x$symbols)
	if (length(x$randoms)!=0) result$randoms <- sort(x$randoms)
	return(result)
}

