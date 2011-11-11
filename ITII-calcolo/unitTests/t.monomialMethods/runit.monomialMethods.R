# TODO: Add comment
# 
# Author: ortellic
###############################################################################

test.explode.randomVariable <- function() {

	# create the random variable epsilon_t^2, the "what"
	what <- create_randomVariable(power=2)
	
	# create the where monomial given by "Z_{t}^3*Z_{t-1}^2*epsilon_{t}^2"
	Zt3 <- create_randomVariable("Z",lag=0,power=3)
	Zt_1_2 <- create_randomVariable("Z",lag=1,power=2)
	randoms <- Zt3 * Zt_1_2 * create_randomVariables(what)
	where <- create_monomial(randoms=randoms)
	
	
	# Test numero 1
	# x1="2*a^2*b^3*Z_{t}^3", x2="4*c^3" and x3=3*Z_{t-1}^2
	x <- constructListOfMonomial()
	randoms <- create_randomVariables(create_randomVariable("Z",lag=1,power=2))
	n3_Zt_1_2 <- create_monomials(create_monomial(3,randoms=randoms))
	with = x[[1]] + x[[2]] + n3_Zt_1_2
	
	# with is equal to "2*a^2*b^3*Z_{t}^3 + 4*c^3 + 3*Z_{t-1}^2"
	checkEquals(toString(explode.randomVariable(what,where,with)),"2*a^2*b^3*Z_{t}^6*Z_{t-1}^2 + 4*c^3*Z_{t}^3*Z_{t-1}^2 + 3*Z_{t}^3*Z_{t-1}^4")

	# Test numero 2
	# replace with an empty monomials (is zero and therefore the result is an empty list)
	with <- create_monomials()
	checkEquals(toString(explode.randomVariable(what,where,with)),"")
	
	# Test numero 3
	# replace with a monomials containing 1 only
	with <- create_monomials(create_monomial())
	originalWhere <- where
	where$randoms[[3]] <- NULL
	checkEquals(explode(what,originalWhere,with),create_monomials(where))
	
}

test.explode.symbol <- function() {

	# create the symbol alpha^2, the "what"
	what <- create_symbol(name="alpha",power=2)
	
	# create the where monomial given by "alpha^2*Z_{t}^3*Z_{t-1}
	Zt3 <- create_randomVariable("Z",lag=0,power=3)
	Zt_1_2 <- create_randomVariable("Z",lag=1,power=2)
	randoms <- Zt3 * Zt_1_2
	symbols <- create_symbols(what)
	where <- create_monomial(symbols=symbols) * create_monomial(randoms=randoms)
	where <- where[[1]]
	
	# Test numero 1
	# x1="2*a^2*b^3*Z_{t}^3", x2="4*c^3" and x3=3*Z_{t-1}^2
	x <- constructListOfMonomial()
	randoms <- create_randomVariables(create_randomVariable("Z",lag=1,power=2))
	n3_Zt_1_2 <- create_monomials(create_monomial(3,randoms=randoms))
	with = x[[1]] + x[[2]] + n3_Zt_1_2
	
	# with is equal to "2*a^2*b^3*Z_{t}^3 + 4*c^3 + 3*Z_{t-1}^2"
	checkEquals(toString(explode(what,where,with)),"2*a^2*b^3*Z_{t}^6*Z_{t-1}^2 + 4*c^3*Z_{t}^3*Z_{t-1}^2 + 3*Z_{t}^3*Z_{t-1}^4")
	
	# Test numero 2
	# replace with an empty monomials (is zero and therefore the result is an empty list)
	with <- create_monomials()
	checkEquals(toString(explode(what,where,with)),"")
	
	# Test numero 3
	# replace with a monomials containing 1 only
	with <- create_monomials(create_monomial())
	originalWhere <- where
	where$symbols[[1]] <- NULL
	checkEquals(explode(what,originalWhere,with),create_monomials(where))
	
}

test.isFirstRandomAnOddPowers.monomial <- function() {
	
	# Test numero 1
	x <- create_monomial()
	result <- isFirstRandomAnOddPowers(x,"Pippo")
	checkEquals(result,FALSE)
	
	# Test numero 2
	# x1="2*a^2*b^3*Z_{t}^3", x2="4*c^3" and x3=3*Z_{t-1}^2
	x <- constructListOfMonomial()
	
	result <- isFirstRandomAnOddPowers.monomial(x[[1]],"Z")
	checkEquals(result,TRUE)
	
	# terminare i test
	checkEquals(FALSE,TRUE)
	
}