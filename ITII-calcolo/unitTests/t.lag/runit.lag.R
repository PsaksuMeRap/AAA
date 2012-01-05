# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.Lag.monomial <- function() {
	
	# Test 1
	x <- monomialFromString("4")
	checkEquals(Lag(x),x)
	
	# Test 2
	x <- monomialFromString("4*Z_t")
	checkEquals(Lag(x),monomialFromString("4*Z_{t-1}"))

	# Test 3
	x <- monomialFromString("4*Z_t")
	checkEquals(Lag(x,power=1),monomialFromString("4*Z_{t-1}"))
	
	# Test 4
	x <- monomialFromString("4*Z_t")
	checkEquals(Lag(x,power=3),monomialFromString("4*Z_{t-3}"))
	
	# Test 5
	x <- monomialFromString("4*a*c^2*Z_t*e_{t+1}")
	checkEquals(Lag(x),monomialFromString("4*a*c^2*Z_{t-1}*e_{t}"))
	
}


test.Lag.monomials <- function() {
	
	# Test 1
	x <- create_monomials()
	checkEquals(Lag(x),x)
	
	# Test 2
	x <- create_monomials(monomialFromString("4*Z_t"))
	checkEquals(Lag(x),create_monomials(monomialFromString("4*Z_{t-1}")))
	
	# Test 3
	x1 <- monomialFromString("4*a*c^2*Z_t*e_{t+1}")
	x2 <- monomialFromString("2*Z_{t-4}")
	x <- x1 + x2
	should <- monomialFromString("4*a*c^2*Z_{t+2}*e_{t+3}") + monomialFromString("2*Z_{t-2}")
	checkEquals(Lag(x,power=-2),should)
	
}

test.maxLag.monomial <- function() {
	# questa funzione restituisce il ritardo massimo della variabile aleatoria di nome randomVariableName
	
	# Test 1
	x <- monomialFromString("4")
	checkEquals(maxLag(x,"pippo"),-Inf)
	
	# Test 2
	x <- monomialFromString("4*Z_t")
	checkEquals(maxLag(x,"Z"),0)
	
	# Test 3
	x <- monomialFromString("4*Z_{t+1}")
	checkEquals(maxLag(x,"Z"),-1)
	
	# Test 4
	x <- monomialFromString("4*H_t*Z_t*Z_{t-100}*Z_{t+1}")
	checkEquals(maxLag(x,"Z"),100)
	
	# Test 5
	x <- monomialFromString("4*H_{t+1}*Z_t*Z_{t-100}*Z_{t+1}")
	checkEquals(maxLag(x,"H"),-1)
}

test.maxLag.monomials <- function() {
	
	# Test 1
	x <- monomialFromString("4") + monomialFromString("3*a*pippo_t")
	checkEquals(maxLag(x,"pippo"),c(-Inf,0))
	
	# Test 2
	x <- monomialFromString("4*Z_t") + monomialFromString("Z_{t+1}")
	checkEquals(maxLag(x,"Z"),c(0,-1))
	
	# Test 3
	x <- create_monomials()
	checkEquals(maxLag(x,"Z"),-Inf)
	
}

test.minLag.monomial <- function() {
	# questa funzione restituisce il ritardo minimo della variabile aleatoria di nome randomVariableName
	
	# Test 1
	x <- monomialFromString("4")
	checkEquals(minLag(x,"pippo"),Inf)
	
	# Test 2
	x <- monomialFromString("4*Z_t")
	checkEquals(minLag(x,"Z"),0)
	
	# Test 3
	x <- monomialFromString("4*Z_{t+1}")
	checkEquals(minLag(x,"Z"),-1)
	
	# Test 4
	x <- monomialFromString("4*H_t*Z_t*Z_{t-100}*Z_{t+1}")
	checkEquals(minLag(x,"Z"),-1)
	
	# Test 5
	x <- monomialFromString("4*H_{t+1}*Z_t*Z_{t-100}*Z_{t+1}")
	checkEquals(minLag(x,"H"),-1)
}

test.minLag.monomials <- function() {
	
	# Test 1
	x <- monomialFromString("4") + monomialFromString("3*a*pippo_t")
	checkEquals(minLag(x,"pippo"),c(Inf,0))
	
	# Test 2
	x <- monomialFromString("4*Z_t") + monomialFromString("Z_{t+1}")
	checkEquals(minLag(x,"Z"),c(0,-1))
	
	# Test 3
	x <- create_monomials()
	checkEquals(minLag(x,"Z"),Inf)
}

test.extractLagCoeff.monomial <- function() {

	# check a zero Lag monomial
	monomial <- monomialFromString("4*a^2*b^3")
	result <- extractLagCoeff(monomial,power=0)
	checkEquals(result,monomial)
	
	# check a monomial with L^k, k>0
	monomial <- monomialFromString("4*a^2*b^3*L^2")
	result <- extractLagCoeff(monomial,power=0)
	checkEquals(result,NULL)
	
	# check a 2 Lag monomial
	monomial <- monomialFromString("4*a^2*b^3")
	result <- extractLagCoeff(monomial,power=2)
	checkEquals(result,NULL)
	
	# check a monomial with L^k, k>0
	monomial <- monomialFromString("4*a^2*b^3*L^2")
	result <- extractLagCoeff(monomial,power=2)
	checkEquals(result,monomialFromString("4*a^2*b^3"))
	
	# check a monomial with L^k, k>0
	monomial <- monomialFromString("L^2")
	result <- extractLagCoeff(monomial,power=2)
	checkEquals(result,monomialFromString("1"))
	
	# check a monomial with L^k, k>0
	monomial <- monomialFromString("L^2")
	result <- extractLagCoeff(monomial,power=3)
	checkEquals(result,NULL)
	
	# check a monomial with L^k, k>0
	monomial <- monomialFromString("L^0")
	result <- extractLagCoeff(monomial,power=0)
	checkEquals(result,monomialFromString("1"))
	
	# check a monomial with L^k, k=0
	monomial <- monomialFromString("6*a^3*b*L^0")
	result <- extractLagCoeff(monomial,power=0)
	checkEquals(result,monomialFromString("6*a^3*b"))
	
}

test.extractLagCoeff.monomials <- function() {	
	
	# check an empty monomials
	monomials <- create_monomials()
	result <- extractLagCoeff(monomials,power=0)
	checkEquals(result,monomials)
	
	# check a monomials with a NULL return
	monomials <- monomialsFromString("4*a^2*L + abc*d*L^2")
	result <- extractLagCoeff(monomials,power=1)
	checkEquals(result,monomialsFromString("4*a^2"))
	
	# check a monomial with three valid returns
	monomials <- monomialsFromString("4*a^2*L + abc*d*L+5*L")
	result <- extractLagCoeff(monomials,power=1)
	checkEquals(result,monomialsFromString("4*a^2 + abc*d+5"))
}
