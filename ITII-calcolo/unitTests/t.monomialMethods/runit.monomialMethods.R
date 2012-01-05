# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.explode.monomial <- function() {

	# create the where monomial given by "Z_{t}^3*Z_{t-1}^2*epsilon_{t}^2"
	where <- monomialFromString("Z_{t}^3*Z_{t-1}^2*epsilon_{t}^2")
	
	# create the random variable epsilon_t^2, the "what"
	what <- create_randomVariable(power=2)
	
	# Test numero 1
	with <- monomialsFromString("2*a^2*b^3*Z_{t}^3 + 4*c^3 + 3*Z_{t-1}^2")
	
	checkEquals(toString(explode.monomial(where,what,with)),
			"2*a^2*b^3*Z_{t}^6*Z_{t-1}^2 + 4*c^3*Z_{t}^3*Z_{t-1}^2 + 3*Z_{t}^3*Z_{t-1}^4")

	# Test numero 2
	# replace with an empty monomials (is zero and therefore the result is an empty list)
	with <- create_monomials()
	checkEquals(toString(explode.monomial(where,what,with)),"")
	
	# Test numero 3
	# replace with a monomials containing 1 only
	with <- monomialsFromString("1")
	originalWhere <- where
	where$randoms[[1]] <- NULL
	checkEquals(explode.monomial(originalWhere,what,with),create_monomials(where))

	# Test numero 4
	# generate an error
	checkException(explode.monomial(originalWhere,what="Z",with))
	
	#----------------- now check the symbol part of the command ----------------------------
 	
	# create the where monomial given by "alpha^2*Z_{t}^3*Z_{t-1}^2"
	where <- monomialFromString("alpha^2*Z_{t}^3*Z_{t-1}^2")
	
	# create the symbol alpha^2, the "what"
	what <- create_symbol(name="alpha",power=2)
	
	# Test numero 1
	with <- monomialsFromString("2*a^2*b^3*Z_{t}^3 + 4*c^3 + 3*Z_{t-1}^2")
	checkEquals(toString(explode.monomial(where,what,with)),
			"2*a^2*b^3*Z_{t}^6*Z_{t-1}^2 + 4*c^3*Z_{t}^3*Z_{t-1}^2 + 3*Z_{t}^3*Z_{t-1}^4")
	
	# Test numero 2
	# replace with an empty monomials (is zero and therefore the result is an empty list)
	with <- create_monomials()
	checkEquals(toString(explode.monomial(where,what,with)),"")
	
	# Test numero 3
	# replace with a monomials containing 1 only
	with <- monomialsFromString("1")
	originalWhere <- where
	where$symbols[[1]] <- NULL
	checkEquals(explode.monomial(originalWhere,what,with),create_monomials(where))
}

test.explode.monomials <- function() {
		
	# create the where monomial given by "Z_{t}^3*Z_{t-1}^2*epsilon_{t}^2"
	where <- monomialsFromString("Z_{t}^3 + Z_{t-1}^2 + a^2")
	
	# create the random variable Z_t^3, the "what"
	what <- create_randomVariable(name="Z",power=3)
	

	# Test numero 1
	with <- monomialsFromString("2*a^2*Z_{t}^3 + 4*c^3")
	checkEquals(toString(explode.monomials(where,what,with)),
			"2*a^2*Z_{t}^3 + 4*c^3 + Z_{t-1}^2 + a^2")
	
	# Test numero 2: replace with an empty monomials
	with <- create_monomials()
	checkEquals(toString(explode.monomials(where,what,with)),"Z_{t-1}^2 + a^2")
	
	# Test numero 3: replace with a monomials containing 1 only
	where <- monomialsFromString("Z_{t}^3 + a^2*Z_{t}^3 + a^2")
	what <- create_randomVariable(name="Z",power=3)
	with <- monomialsFromString("1")

	checkEquals(explode.monomials(where,what,with),
			monomialsFromString("1 + a^2 + a^2"))
	
	# Test numero 4
	# generate an error
	checkException(explode.monomials(where,what="Z",with))
	
	#----------------- now check the symbol part of the command ----------------------------
	
	# create the where monomial given by "alpha^2*Z_{t}^3*Z_{t-1}^2"
	where <- monomialsFromString("a^2*Z_{t}^3 + Z_{t-1}^2")
	
	# create the symbol alpha^2, the "what"
	what <- create_symbol(name="a",power=2)
	
	# Test numero 1
	with <- monomialsFromString("2*a^2*b^3*Z_{t}^3 + 4*c^3 + 3*Z_{t-1}^2")

	checkEquals(toString(explode.monomials(where,what,with)),
			"2*a^2*b^3*Z_{t}^6 + 4*c^3*Z_{t}^3 + 3*Z_{t}^3*Z_{t-1}^2 + Z_{t-1}^2")
	
	# Test numero 2: replace with an empty monomials (it is considered 0)
	with <- create_monomials()
	checkEquals(toString(explode.monomials(where,what,with)),"Z_{t-1}^2")
	
	# Test numero 3
	# replace with a monomials containing 1 only
	with <- monomialsFromString("1")
	checkEquals(explode.monomials(where,what,with),
			monomialsFromString("Z_{t}^3 + Z_{t-1}^2"))
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

	# test1: crea il default
	should <- "b0 + b1*h_{t-1}*w_{t-1} + b2*h_{t-1}"
	checkEquals(create.h_t.expansion(),monomialsFromString(should))
	
	
	# Test2: crea un'errore
	checkException(create.h_t.expansion(0,0))
	
	# test3: crea l'espansione quando fromLag=0 e toLag=2
	should <- "b0 + b0*b1*w_{t-1} + b1^2*w_{t-1}*w_{t-2}*h_{t-2} + b1*b2*w_{t-1}*h_{t-2} + b0*b2 + b1*b2*w_{t-2}*h_{t-2} + b2^2*h_{t-2}"
	checkEquals(create.h_t.expansion(0,2),monomialsFromString(should))
	
	# test4: crea l'espansione quando fromLag=1 e toLag=3
	should <- "b0 + b0*b1*w_{t-1} + b1^2*w_{t-1}*w_{t-2}*h_{t-2} + b1*b2*w_{t-1}*h_{t-2} + b0*b2 + b1*b2*w_{t-2}*h_{t-2} + b2^2*h_{t-2}"
	checkEquals(create.h_t.expansion(1,3),Lag(monomialsFromString(should),1))
	
	# test4: crea l'espansione quando fromLag=1 e toLag=4        
	should <- "b0 + b0*b1*w_{t-2} + b0*b1^2*w_{t-2}*w_{t-3} + b1^3*h_{t-4}*w_{t-2}*w_{t-3}*w_{t-4} + b1^2*b2*h_{t-4}*w_{t-2}*w_{t-3} + b0*b1*b2*w_{t-2} + b1^2*b2*h_{t-4}*w_{t-2}*w_{t-4} + b1*b2^2*h_{t-4}*w_{t-2} + b0*b2 + b0*b1*b2*w_{t-3} + b1^2*b2*h_{t-4}*w_{t-3}*w_{t-4} + b1*b2^2*h_{t-4}*w_{t-3} + b0*b2^2 + b1*b2^2*h_{t-4}*w_{t-4} + b2^3*h_{t-4}"
	checkEquals(create.h_t.expansion(1,4),monomialsFromString(should))
}



test.disaggregateRandomVariable <- function() {
	
	# verifica quando la potenza è 1
	testRV <- create_randomVariable(name="a",lag=2)
	result <- disaggregate(testRV)
	
	checkEquals(toString(result),"a_{t-2}")
	
	# verifica quando la potenza è 3
	testRV <- create_randomVariable(name="a",lag=2,power=3)
	result <- disaggregate(testRV,name="a",lag=2)
	
	checkEquals(toString(result),"a_{t-2}*a_{t-2}*a_{t-2}")

	# verifica quando la potenza è 3 ma lag non corretto
	testRV <- create_randomVariable(name="a",lag=2,power=3)
	result <- disaggregate(testRV,name="a",lag=1)
	
	checkEquals(toString(result),"a_{t-2}^3")
	
}


test.disaggregateRandomVariables <- function() {
	
	# con empty random_variables
	randomVariables <- create_randomVariables()
	result <- disaggregate(randomVariables,"a",2)
	
	checkEquals(result,randomVariables)
	
	# verifica quando la potenza è 3
	testRV1 <- create_randomVariable(name="a",lag=2,power=3)
	testRV2 <- create_randomVariable(name="b",lag=3,power=4)
	randomVariables1 <- create_randomVariables(testRV1)
	randomVariables2 <- create_randomVariables(testRV2)
	randomVariables <- randomVariables1 * randomVariables2
	
	result <- disaggregate(randomVariables,"a",2)
	checkEquals(toString(result),"a_{t-2}*a_{t-2}*a_{t-2}*b_{t-3}^4")
	
}

test.disaggregateMonomial <- function() {
	
	# 
	monomial <- monomialFromString("4*a*b^3*h_t^2*g_{t-2}^3")
	rv <- create_randomVariable(name="h",lag=0,power=2)
	
	result <- disaggregate(monomial,rv)
	checkEquals(toString(result),"4*a*b^3*g_{t-2}^3*h_{t}*h_{t}")
}

test.disaggregateMonomials <- function() {
	
	# 
	monomial <- monomialsFromString("2*a*z_t + h_t^2*g_t^3")
	rv <- create_randomVariable(name="h",lag=0,power=2)
	
	result <- disaggregate(monomial,rv)
	checkEquals(toString(result),"2*a*z_{t} + g_{t}^3*h_{t}*h_{t}")
}

test.removeZero <- function() {
	
	# return all monomial
	monomials <- monomialsFromString("3.5*a + 5*b - 3*c")
	result <- removeZero(monomials)
	checkEquals(result,monomials)
	
	# remove one monomial
	monomials <- monomialsFromString("3.5*a + 0*b - 3*c")
	result <- removeZero(monomials)
	monomials[[2]] <- NULL
	checkEquals(result,monomials)
	
	# remove all monomials
	monomials <- monomialsFromString("0 + 0*b - 0*c")
	result <- removeZero(monomials)
	checkEquals(result,create_monomials())

	# remove from empty monomials
	monomials <- create_monomials()
	result <- removeZero(monomials)
	checkEquals(result,create_monomials())
}


test.maxPower.monomial <- function() {
	
	# check no random Variables
	a <- monomialFromString("3*a")
	result <- maxPower(a,"g")
	checkEquals(result,-Inf)
	
	# check no desired random variable
	a <- monomialFromString("3*a*z_t^2*g_{t-3}^2*g_{t-2}^14")
	result <- maxPower(a,"k")
	checkEquals(result,-Inf)
	
	# check desired random variable
	a <- monomialFromString("3*a*z_t^2*g_{t-3}^2*g_{t-2}^14")
	result <- maxPower(a,"g")
	checkEquals(result,14)

	# check no symbols
	a <- monomialFromString("3*a_t")
	result <- maxPower(a,"g","symbol")
	checkEquals(result,-Inf)
	
	# check no desired symbol
	a <- monomialFromString("3*a*z_t^2*g_{t-3}^2*g_{t-2}^14")
	result <- maxPower(a,"k","symbol")
	checkEquals(result,-Inf)
	
	# check desired symbol
	a <- monomialFromString("3*a*qq^3*beta^12*z_t^2*g_{t-3}^2*g_{t-2}^14")
	result <- maxPower(a,"beta","symbol")
	checkEquals(result,12)
}


test.maxPower.monomial <- function() {
	
	# check no random Variables
	a <- monomialsFromString("3*a + z_t^2*g_{t-3}^2*g_{t-2}^14")
	result <- maxPower(a,"g")
	checkEquals(result,c(-Inf,14))
	
	# check no desired random variable
	a <- create_monomials()
	result <- maxPower(a,"k")
	checkEquals(result,-Inf)
	
}


