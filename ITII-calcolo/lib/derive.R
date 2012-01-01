# TODO: Add comment
# 
# Author: claudio
###############################################################################


derive <- function(x,wrt="L") UseMethod("derive",x)

derive.symbol <- function(x,wrt="L") {
	# x: a symbol
	# wrt: "with respect to", i.e. the variable name wrt derive
	
	# returns a list with three slots: number, name and power
	if (x$name!=wrt) return(create_monomial(number=0))
	tmp <- create_symbol(name="L",power=x$power-1)
	tmp1 <- create_symbols(tmp)
	tmp2 <- create_monomial(number=x$power,symbols=tmp1)
	return(tmp2)
	
}


derive.symbols <- function(x,wrt="L") {
	# x: a symbols
	# wrt: "with respect to", i.e. the variable name wrt derive
	??? x <- compact(x)
	# returns a list with three slots: number, name and power
	if (x$name!=wrt) return(create_monomial(number=0))
	tmp <- create_symbol(name="L",power=x$power-1)
	tmp1 <- create_symbols(tmp)
	tmp2 <- create_monomial(number=x$power,symbols=tmp1)
	return(tmp2)
	
}


