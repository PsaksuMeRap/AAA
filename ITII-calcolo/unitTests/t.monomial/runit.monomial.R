# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.create_monomial <- function() {
	
	monomial <- create_monomial()
	
	checkEquals(class(monomial),"monomial")
	checkEquals(monomial$number,1)
	checkEquals(class(monomial$symbols),"symbols")
	checkEquals(class(monomial$randoms),"randoms")
	checkEquals(length(monomial$symbols),0)
	checkEquals(length(monomial$randoms),0)

}

test.create_polymonomial <- function() {
	
	
	polynomial <- create_polynomial()

	checkEquals(class(polynomial),"polynomial")
	checkEquals(polynomial[[1]],monomial)
	
}

test.sum_two_monomials <- function() {
	
	# necessito la funzione sort
	# ... mon1 <- create_monomial(number=1,symbols=NULL,randoms=NULL)
	
	
}