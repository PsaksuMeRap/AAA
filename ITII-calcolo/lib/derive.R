# TODO: Add comment
# 
# Author: claudio
###############################################################################


derive <- function(x,wrt="L") UseMethod("derive",x)

derive.monomial <- function(x,wrt="L") {
	# x: a monomial
	# wrt: "with respect to", i.e. the variable name wrt derive

	symbols <- x$symbols
	l.symbols <- length(symbols)
	
	if (l.symbols==0) return(create_monomial(number=0))
	
	if (l.symbols==1) {
		if (symbols[[1]]$name!=wrt) {
			return(create_monomial(number=0))		
		} else {
			power <- symbols[[1]]$power
			if (power==0) return(create_monomial(number=0))
			
			if (power==1) {
				symbols[[1]] <- NULL
				return(create_monomial(number=x$number,symbols=symbols,randoms=x$randoms))
			}
			number <- x$number*power
			symbols[[1]]$power <- power-1
			result <- create_monomial(number=number,symbols=symbols,randoms=x$randoms)
			return(result)
		}
	}
	
	number <- x$number
	symbols <- compact(x$symbols)
	areDerived <- sapply(symbols,function(x,wrt){return(x$name==wrt)},wrt)
	if (any(areDerived)) {
		symbolsToDerive <- symbols[areDerived]
		symbols[areDerived] <- NULL
		tmpMonomial <- create_monomial(number=x$number,symbols=symbols,randoms=x$randoms)
		tmpToDerive <- create_monomial(number=1,symbols=symbolsToDerive)
		result <- tmpMonomial*derive(tmpToDerive,wrt)
		return(result[[1]])
	} else {
		return(create_monomial(number=0))
	}
	
}

derive.monomials <- function(monomials,wrt="L") {
	# x: a monomials
	# wrt: "with respect to", i.e. the variable name wrt derive
	
	l.monomials <- length(monomials)
	if (l.monomials==0) return(monomials)
	result <- lapply(monomials,derive,wrt)
	class(result) <- "monomials"
	return(result)
}

