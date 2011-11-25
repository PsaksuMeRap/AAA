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
	checkEquals(maxLag(x,"pippo"),NA_integer_)
	
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
	checkEquals(maxLag(x,"pippo"),c(NA_integer_,0))
	
	# Test 2
	x <- monomialFromString("4*Z_t") + monomialFromString("Z_{t+1}")
	checkEquals(maxLag(x,"Z"),c(0,-1))
	
	# Test 3
	x <- create_monomials()
	checkEquals(maxLag(x,"Z"),numeric(0))
	
}

test.minLag.monomial <- function() {
	# questa funzione restituisce il ritardo minimo della variabile aleatoria di nome randomVariableName
	
	# Test 1
	x <- monomialFromString("4")
	checkEquals(minLag(x,"pippo"),NA_integer_)
	
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
	checkEquals(minLag(x,"pippo"),c(NA_integer_,0))
	
	# Test 2
	x <- monomialFromString("4*Z_t") + monomialFromString("Z_{t+1}")
	checkEquals(minLag(x,"Z"),c(0,-1))
	
	# Test 3
	x <- create_monomials()
	checkEquals(minLag(x,"Z"),numeric(0))
}