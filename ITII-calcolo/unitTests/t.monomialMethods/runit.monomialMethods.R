# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.explode.monomial <- function() {

	# create the random variable epsilon_t^2, the "what"
	what <- create_randomVariable(power=2)
	
	# create the where monomial given by "Z_{t}^3*Z_{t-1}^2*epsilon_{t}^2"
	where <- monomialFromString("Z_{t}^3*Z_{t-1}^2*epsilon_{t}^2")
	
	# Test numero 1
	# x1="2*a^2*b^3*Z_{t}^3", x2="4*c^3" and x3=3*Z_{t-1}^2
	x <- constructListOfMonomial()
	randoms <- create_randomVariables(create_randomVariable("Z",lag=1,power=2))
	n3_Zt_1_2 <- create_monomials(create_monomial(3,randoms=randoms))
	with = x[[1]] + x[[2]] + n3_Zt_1_2
	
	# with is equal to "2*a^2*b^3*Z_{t}^3 + 4*c^3 + 3*Z_{t-1}^2"
	checkEquals(toString(explode.monomial(where,what,with)),"2*a^2*b^3*Z_{t}^6*Z_{t-1}^2 + 4*c^3*Z_{t}^3*Z_{t-1}^2 + 3*Z_{t}^3*Z_{t-1}^4")

	# Test numero 2
	# replace with an empty monomials (is zero and therefore the result is an empty list)
	with <- create_monomials()
	checkEquals(toString(explode.monomial(where,what,with)),"")
	
	# Test numero 3
	# replace with a monomials containing 1 only
	with <- create_monomials(create_monomial())
	originalWhere <- where
	where$randoms[[3]] <- NULL
	checkEquals(explode.monomial(originalWhere,what,with),create_monomials(where))

	# Test numero 4
	# generate an error
	checkException(explode.monomial(originalWhere,what="Z",with))
	
	########## now check the symbol part of the command
	# create the symbol alpha^2, the "what"
	what <- create_symbol(name="alpha",power=2)
	
	# create the where monomial given by "alpha^2*Z_{t}^3*Z_{t-1}^2"
	where <- monomialFromString("alpha^2*Z_{t}^3*Z_{t-1}^2")
	
	
	# Test numero 1
	# x1="2*a^2*b^3*Z_{t}^3", x2="4*c^3" and x3="3*Z_{t-1}^2"
	
	x1 <- monomialFromString("2*a^2*b^3*Z_{t}^3")
	x2 <- monomialFromString("4*c^3")
	x3 <- monomialFromString("3*Z_{t-1}^2")
	x3 <- create_monomials(x3)
	with = x1 + x2 + x3
	
	# with is equal to "2*a^2*b^3*Z_{t}^3 + 4*c^3 + 3*Z_{t-1}^2"
	checkEquals(toString(explode.monomial(where,what,with)),"2*a^2*b^3*Z_{t}^6*Z_{t-1}^2 + 4*c^3*Z_{t}^3*Z_{t-1}^2 + 3*Z_{t}^3*Z_{t-1}^4")
	
	# Test numero 2
	# replace with an empty monomials (is zero and therefore the result is an empty list)
	with <- create_monomials()
	checkEquals(toString(explode.monomial(where,what,with)),"")
	
	# Test numero 3
	# replace with a monomials containing 1 only
	with <- create_monomials(create_monomial())
	originalWhere <- where
	where$symbols[[1]] <- NULL
	checkEquals(explode.monomial(originalWhere,what,with),create_monomials(where))
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
	
	# Test1
	x <- create_monomials()
	checkEquals(shiftToZero(x),x)
	
	# Test2
	# z="2*a^2*b^3"
	z <-  create_monomials(monomialFromString("2*a^2*b^3"))
	checkEquals(shiftToZero(z),z)
	
	# Test3
	z <- create_monomials( monomialFromString("2*a^2*b^3*Z_t^3"))
	checkEquals(shiftToZero(z),z)
	
	# Test4
	# x1="2*a^2*b^3*Y_{t-5}^3*Z_{t-1}^4"
	a <- monomialFromString("2*a^2*b^3*Y_{t-5}^3*Z_{t-1}^2")
	z <- a + monomialFromString("2*a^2*b^3*Z_{t}^3")

	a <- shiftToZero(z)
	z[[1]]$randoms[[1]]$lag <- 4
	z[[1]]$randoms[[2]]$lag <- 0

	checkEquals(a,z)
	
}

test.compactMonomials <- function() {	
	
	# Test1: empty monomials
	x <- create_monomials()
	
	checkEquals(compactMonomials(x),x)
	
	# Test2: atomic monomials
	# x="2*a^2*b^3*Y_{t-5}^3"
	x <- monomialFromString("2*a^2*b^3*Y_{t-5}^3")
	x <- create_monomials(x)
	
	checkEquals(compactMonomials(x),x)
	
	# Test3: a monomial
	# x1="2*a^2*b^3*Y_{t-5}^3"
	x1 <- monomialFromString("2*a^2*b^3*Y_{t-5}^3")
	
	# x2="2*a^2*b^3*Y_{t-5}^3"
	x2 <- monomialFromString("2*a^2*b^3*Y_{t-5}^3")
	
	# x3="2*a^2*b^3*Y_{t-5}^3"
	x3 <- monomialFromString("2*a^2*b^3*Z_t")
	
	x <- create_monomials()
	x[[1]] <- x1
	x[[2]] <- x2
	x[[3]] <- x3
	
	# costruisci risultato per verifica
	y1 <- monomialFromString("4*a^2*b^3*Y_{t-5}^3")
	y2 <- monomialFromString("2*a^2*b^3*Z_t")
	y <- create_monomials()
	y[[1]] <- y1
	y[[2]] <- y2
	
 	checkEquals(compactMonomials(x),y)

}


test.shiftToZeroAndCompact <- function() {	
	
	# Test1: empty monomials
	x <- create_monomials()
	
	checkEquals(shiftToZeroAndCompact(x),x)
	
	# Test2: atomic monomials
	# x="2*a^2*b^3*Y_{t-5}^3"
	x <- monomialFromString("2*a^2*b^3*Y_{t-5}^3")
	x <- create_monomials(x)
	y <- x
	y[[1]]$randoms[[1]]$lag <- 0
	checkEquals(shiftToZeroAndCompact(x),y)
	
	# Test3: a monomial
	# x1="2*a^2*b^3*Y_{t-5}^3"
	x1 <- monomialFromString("2*a^2*b^3*Y_{t-5}")
	
	# x2="2*a^2*b^3*Y_{t-5}^3"
	x2 <- monomialFromString("2*a^2*b^3*Y_{t-5}")
	
	# x3="2*a^2*b^3*Y_{t-5}^3"
	x3 <- monomialFromString("2*a^2*b^3*Y_t")
	
	x <- create_monomials()
	x[[1]] <- x1
	x[[2]] <- x2
	x[[3]] <- x3
	
	# costruisci risultato per verifica
	y1 <- monomialFromString("6*a^2*b^3*Y_t")
	y <- create_monomials()
	y[[1]] <- y1
	
	checkEquals(shiftToZeroAndCompact(x),y)
}




test.create.h_t.expansion <- function() {
	
	create.h_t.expansion <- function(toLag=1) {
		ht <- monomialFromString("b0") + monomialFromString("b2*h_{t-1}")
		ht <- ht + create_monomials(monomialFromString("b1*w_{t-1}*h_{t-1}"))
		ht.lagged <- Lag(ht)
		ht <- lapply()
	}
	checkEquals(TRUE,FALSE)
}