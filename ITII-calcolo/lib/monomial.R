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


create_monomials<- function(monomial=NULL) {
	
	monomials <- list()
	class(monomials) <- "monomials"
	
	if (is.null(monomial)) monomial <- create_monomial()
	monomials[[1]] <- monomial
	return(monomials)
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


compact <- function(a) UseMethod("compact",a)

compact.monomials <- function(a) {

	la <- length(a)
	if (la <= 1) return(a)
	
	tmp <- list()
	tmp[[1]] <- a[[1]]
	for(monomial in a[-1]) {
		copy <- TRUE
		for (i in 1:length(tmp)) {
			if (monomial$symbol==tmp[[i]]$symbol & monomial$randoms==tmp[[i]]$randoms) {
				tmp[[i]]$number=tmp[[i]]$number+monomial$number
				copy <- FALSE
				break
			}
		}
		if (copy) tmp[[length(tmp)+1]] <- monomial
	}
	
	class(tmp) <- "monomials"
	return(tmp)

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


"*.monomials" <- function(a,b) {
	
	la <- length(a)
	lb <- length(b)
	if (lb == 0) return(b)
	if (la == 0) return(a)
	
	cartProd.df <- expand.grid(a,b)
	result <- mapply("*",cartProd.df[[1]],cartProd.df[[2]])
	class(result) <- "monomials"
	result <- compact(result)
	return(result)

}

