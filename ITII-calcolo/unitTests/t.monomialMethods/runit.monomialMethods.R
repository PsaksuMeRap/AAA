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



test.isFirstRandomAnOddPower.monomial <- function() {
	
	# Test numero 1
	x <- create_monomial()
	result <- isFirstRandomAnOddPower(x,"Pippo")
	checkEquals(result,FALSE)
	
	# Test numero 2
	# x1="2*a^2*b^3*Z_{t}^3", x2="4*c^3" and x3="-4*c^3"
	x <- constructListOfMonomial()
	
	result <- isFirstRandomAnOddPower.monomial(x[[1]],"Z")
	checkEquals(result,TRUE)
	
	result <- isFirstRandomAnOddPower.monomial(x[[3]],"Z")
	checkEquals(result,FALSE)
	
	# test numero 3
	y <- x[[1]] * x[[3]]
	result <- isFirstRandomAnOddPower.monomial(y[[1]],"Z")
	checkEquals(result,TRUE)

}



test.isFirstRandomAnOddPower.monomials <- function() {
	# x1="2*a^2*b^3*Z_{t}^3", x2="4*c^3" and x3="-4*c^3"
	x <- constructListOfMonomial()
	class(x) <- "monomials"
	
	# Test numero 1
	y <- create_monomial()
	z <- create_monomials(y)
	z <- z + x
	
	result <- isFirstRandomAnOddPower(z,"Z")
	checkEquals(result,c(FALSE,TRUE,FALSE,FALSE))
	
	# Test numero 2
	z <- create_monomials()
	result <- isFirstRandomAnOddPower(z,"Z")
	checkEquals(result,list())
}



test.dropWhereFirstRandomIsOddPower <- function() {
		
	# x1="2*a^2*b^3*Z_{t}^3", x2="4*c^3" and x3="-4*c^3"
	x <- constructListOfMonomial()
	class(x) <- "monomials"
	
	# Test numero 1
	y <- create_monomial()
	z <- create_monomials(y)
	z <- z + x
	should <- z[-2]
	class(should) <- "monomials"

	result <- dropWhereFirstRandomIsOddPower(z,"Z")
	checkEquals(result,should)
	
	# Test numero 2
	z <- create_monomials()
	should <- list()
	class(should) <- "monomials"

	result <- dropWhereFirstRandomIsOddPower(z,"Z")
	checkEquals(result,should)
	
}

test.shiftToZero.monomial <- function() {
	
	# Test1
	# x1="2*a^2*b^3*Z_{t}^3", x2="4*c^3" and x3="-4*c^3"
	x <- constructListOfMonomial()
	class(x) <- "monomials"
	x1 <- x[[1]]
	x2 <- x[[2]]
	
	a <- shiftToZero(x1)
	checkEquals(a,x1)
	
	b <- shiftToZero(x2)
	checkEquals(b,x2)
	
	# Test2
	# x1="2*a^2*b^3*Z_{t}^3", x2="4*c^3" and x3=3*Z_{t-1}^2
	x <- constructListOfMonomial()
	randoms <- create_randomVariables(create_randomVariable("Z",lag=1,power=2))
	n3_Zt_1_2 <- create_monomial(3,randoms=randoms)
	x1 = x[[1]] * n3_Zt_1_2

	a <- shiftToZero(x1[[1]])
	checkEquals(a,x1[[1]])
	
	# Test3
	# x1="2*a^2b^3*Y_{t-5}^3*Z_{t-1}^2
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randoms <- create_randomVariables(create_randomVariable("Y",lag=5,power=3))
	a <- create_monomial(2,symbols=symbols,randoms=randoms)
	randoms <- create_randomVariables(create_randomVariable("Z",lag=1,power=2))
	b <- create_monomial(1,randoms=randoms)
	x1 = a * b
	
	a <- shiftToZero(x1[[1]])
	x1[[1]]$randoms[[1]]$lag <- 4
	x1[[1]]$randoms[[2]]$lag <- 0
	checkEquals(a,x1[[1]])
	
}


test.shiftToZero.monomials <- function() {	
	
	x <- create_monomials()
	
	checkEquals(shiftToZero(x),x)
	
	# Test2
	# x1="2*a^2b^3*Y_{t-5}^3*Z_{t-1}^2
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randoms <- create_randomVariables(create_randomVariable("Y",lag=5,power=3))
	a <- create_monomial(2,symbols=symbols,randoms=randoms)
	randoms <- create_randomVariables(create_randomVariable("Z",lag=1,power=2))
	b <- create_monomial(1,randoms=randoms)
	x1 <- a * b
	y  <- constructListOfMonomial()
	z <- x1 + create_monomials(y[[1]])
			

	x1[[1]]$randoms[[1]]$lag <- 4
	x1[[1]]$randoms[[2]]$lag <- 0
	z[[1]] <- x1
	a <- shiftToZero(z)
	checkEquals(a,z)
	
}

test.compactMonomials <- function() {	
	
	compactMonomials <- function(x) {
		if (class(x)!="monomials") stop("Error compactSum: argument is not of class monomials.")
		# x: a monomials whose terms must be compacted
		if (length(x)<=1) return(x)	
		tmp <- create_monomials()
		for (y in x) tmp <- tmp + create_monomials(y)
		return(tmp)
	}
	
	# Test1: empty monomials
	x <- create_monomials()
	
	checkEquals(compactMonomials(x),x)
	
	# Test2: atomic monomials
	# x1="2*a^2b^3*Y_{t-5}^3
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randoms <- create_randomVariables(create_randomVariable("Y",lag=5,power=3))
	x <- create_monomials(create_monomial(2,symbols=symbols,randoms=randoms))
	
	checkEquals(compactMonomials(x),x)
	
	# Test2: atomic monomials
	# x1="2*a^2b^3*Y_{t-5}^3
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randoms <- create_randomVariables(create_randomVariable("Y",lag=5,power=3))
	x <- create_monomials(create_monomial(2,symbols=symbols,randoms=randoms))
	
	checkEquals(compactMonomials(x),x)
	
}


