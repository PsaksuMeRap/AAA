# TODO: Add comment
# 
# Author: ortellic
###############################################################################


constructMonomial <- function() {

	# create 2*a^2*b^3*Z_t^3
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables <- create_randomVariables(create_randomVariable("Z",0,3))
	a <- create_monomial(2,symbols,randomVariables)
	
	# create 4*c^3
	symbols <- create_symbols(create_symbol(name="c",power=3))
	randomVariables <- create_randomVariables()
	b <- create_monomial(4,symbols,randomVariables)
	
	# create -4*c^3
	symbols <- create_symbols(create_symbol(name="c",power=3))
	randomVariables <- create_randomVariables()
	c <- create_monomial(-4,symbols,randomVariables)
	
	return(list(a,b,c))
}
