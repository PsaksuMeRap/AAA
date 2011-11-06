# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.create_monomial <- function() {
	
	# create empty monomial
	monomial <- create_monomial()
	
	checkEquals(class(monomial),"monomial")
	checkEquals(monomial$number,1)
	checkEquals(class(monomial$symbols),"symbols")
	checkEquals(class(monomial$randoms),"randomVariables")
	checkEquals(length(monomial$symbols),0)
	checkEquals(length(monomial$randoms),0)

	# create 2*a^2*b^3Z_t^3
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables <- create_randomVariables(create_randomVariable("Z",0,3))
	monomial <- create_monomial(2,symbols,randomVariables)
	checkEquals(monomial$number,2)
	checkEquals(class(monomial$symbols),"symbols")
	checkEquals(class(monomial$randoms),"randomVariables")
	checkEquals(length(monomial$symbols),2)
	checkEquals(length(monomial$randoms),1)
}


test.sum_two_monomial <- function() {
	
	# sum two empty monomial
	monomial <- create_monomial()
	
	a <- monomial + monomial
	checkEquals(class(a),"monomials")
	checkEquals(length(a),1)	
	checkEquals(a[[1]]$number,2)

	
	# sum two different monomial
	# create 2*a^2*b^3Z_t^3
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables <- create_randomVariables(create_randomVariable("Z",0,3))
	a <- create_monomial(2,symbols,randomVariables)
	# create 2*c^3
	symbols <- create_symbols(create_symbol(name="c",power=3))
	randomVariables <- create_randomVariables()
	b <- create_monomial(4,symbols,randomVariables)
	
	c <- a + b 
	
	checkEquals(class(c),"monomials")
	checkEquals(length(c),2)	
	checkEquals(c[[1]],a)
	checkEquals(c[[2]],b)
	
}


test.sum_two_monomials <- function() {	
	
	# sum three empty monomial
	monomial <- create_monomial()
	
	a <- monomial + monomial + create_monomials(monomial)
	checkEquals(class(a),"monomials")
	checkEquals(length(a),1)	
	checkEquals(a[[1]]$number,3)
	
	
	# sum three different monomials
	# create 2*a^2*b^3Z_t^3
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables <- create_randomVariables(create_randomVariable("Z",0,3))
	a <- create_monomial(2,symbols,randomVariables)
	# create 2*c^3
	symbols <- create_symbols(create_symbol(name="c",power=3))
	randomVariables <- create_randomVariables()
	b <- create_monomial(4,symbols,randomVariables)
	
	c <- create_monomials(a) + create_monomials(b) + create_monomials(monomial)
	
	checkEquals(class(c),"monomials")
	checkEquals(length(c),3)	
	checkEquals(c[[1]],a)
	checkEquals(c[[2]],b)
	checkEquals(c[[3]],monomial)
	
	# sum two identical monomials
	# create 2*a^2*b^3Z_t^3
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables <- create_randomVariables(create_randomVariable("Z",0,3))
	a <- create_monomial(2,symbols,randomVariables)
	
	c <- create_monomials(a) + create_monomials(a)
	
	checkEquals(class(c),"monomials")
	checkEquals(length(c),1)	
	checkEquals(c[[1]]$number,4)
	checkEquals(c[[1]]$symbols,symbols)
	checkEquals(c[[1]]$randoms,randomVariables)
}


test.multiply_two_monomial <- function() {
	
	
	"*.monomial" <- function(a,b) {
		
		number <- a$number * b$number
		if (number==0) {
			monomial <- create_monomial(number=0)
			return(create_monomials(monomial))
		}
		symbols <- a$symbols * b$symbols
		randoms <- a$randoms * b$randoms
		
		c <- create_monomials(create_monomial(number,symbols,randoms))
		return(c)
		
		stop("Error in function '*.monomial': entered arguments are not valid monomial")
	}
	
	
	# multiply two empty monomial
	monomial <- create_monomial()
	
	a <- monomial * monomial
	checkEquals(class(a),"monomials")
	checkEquals(length(a),1)	
	checkEquals(a[[1]]$number,1)
	
	
	# multiply two different monomial
	# create 2*a^2*b^3Z_t^3
	symbols1 <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables1 <- create_randomVariables(create_randomVariable("Z",0,3))
	a <- create_monomial(2,symbols1,randomVariables1)
	# create 2*c^3
	symbols2 <- create_symbols(create_symbol(name="c",power=3))
	randomVariables2 <- create_randomVariables()
	b <- create_monomial(4,symbols2,randomVariables2)
	
	c <- a * b 
	
	checkEquals(class(c),"monomials")
	checkEquals(length(c),1)	
	checkEquals(c[[1]]$number,8)
	checkEquals(c[[1]]$symbols,symbols1*symbols2)
	checkEquals(c[[1]]$randoms,randomVariables1*randomVariables2)
}


test.create_monomials <- function() {
	
	# create monomials with unity as the only monomial
	monomials <- create_monomials()

	checkEquals(class(monomials),"monomials")
	checkEquals(monomials[[1]],create_monomial())
	
	# create monomials with  2*a^2*b^3Z_t^3
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables <- create_randomVariables(create_randomVariable("Z",0,3))
	monomial <- create_monomial(2,symbols,randomVariables)
	
	monomials <- create_monomials(monomial)
	
	checkEquals(class(monomials),"monomials")
	checkEquals(monomials[[1]],monomial)
}

test.sum_two_monomials <- function() {
	DEACTIVATED()
	# necessito la funzione sort
	# ... mon1 <- create_monomial(number=1,symbols=NULL,randoms=NULL)
	
	
}