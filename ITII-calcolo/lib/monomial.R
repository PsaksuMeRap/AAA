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


"+.monomials" <- function(a,b) {
	
	if (length(b)==0) return(a)
	if (length(a)==0) return(b)
	
	tmp <- list()
	for(mon_b in b) {
		copy <- TRUE
		for (i in 1:length(a)) {
			if (mon_b$symbol==a[[i]]$symbol & mon_b$randoms==a[[i]]$randoms) {
				a[[i]]$number=a[[i]]$number+mon_b$number
				copy <- FALSE
				break
			}
		}
		if (copy) tmp[[length(tmp)+1]] <- mon_b
	}
	
	if (length(tmp)>0) { a <- c(a,tmp); class(a) <- "monomials" }
	return(a)
	
	stop("Error in function '*.monomials': entered arguments are not valid monomials")
}


create_monomials<- function(monomial=NULL) {
	
	monomials <- list()
	class(monomials) <- "monomials"
	
	if (is.null(monomial)) monomial <- create_monomial()
	monomials[[1]] <- monomial
	return(monomials)
}

