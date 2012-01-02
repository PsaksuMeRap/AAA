# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.derive.monomial <- function()  {
	
	# check 1: deriva un semplice numero
	monomial <- create_monomial(number=4)
	result <- derive(monomial)
	checkEquals(result,create_monomial(number=0))
	
	# check 2: deriva un semplice monomio senza "L"
	monomial <- monomialFromString("5*a1^3")
	result <- derive(monomial)
	checkEquals(result,create_monomial(number=0))
	
	# check 3: deriva un semplice monomio con "L"
	monomial <- monomialFromString("5*L^3")
	result <- derive(monomial)
	checkEquals(result,monomialFromString("15*L^2"))
	
	# check 4: deriva monomio lungo senza "L"
	monomial <- monomialFromString("5*a1^3*b2^2")
	result <- derive(monomial)
	checkEquals(result,create_monomial(number=0))
	
	# check 5: deriva monomio lungo con "L"
	monomial <- monomialFromString("5*a1^3*b2^2*L^12")
	result <- derive(monomial)
	checkEquals(result,monomialFromString("60*a1^3*b2^2*L^11"))

	# check 6: deriva monomio semplice con "L^1"
	monomial <- monomialFromString("5*L^1")
	result <- derive(monomial)
	checkEquals(result,monomialFromString("5"))
	
	# check 7: deriva monomio lungo con "L^1"
	monomial <- monomialFromString("5*a1^3*b2^2*L^1")
	result <- derive(monomial)
	checkEquals(result,monomialFromString("5*a1^3*b2^2"))
	
	# check 7: deriva monomio lungo con wrt = "a"
	monomial <- monomialFromString("5*a1^3*a^2*b*L^1")
	result <- derive(monomial,wrt="a")
	checkEquals(toString(result),"10*a*a1^3*b*L")
	
}


test.derive.monomials <- function()  {
	
	# check 1: deriva un empty monomials
	monomials <- create_monomials()
	result <- derive(monomials)
	checkEquals(result,create_monomials())	
	
	# check 2: deriva un monomials
	monomials <- monomialsFromString("3*a+2*B")
	result <- derive(monomials)
	checkEquals(toString(result),"0 + 0")
	
	# check 3: deriva tre monomials con "L"
	monomials <- monomialsFromString("-2*b*c*d*L^15+3*a*L+5*ab*L^2")
	result <- derive(monomials)
	checkEquals(toString(result),"-30*b*c*d*L^14 + 3*a + 10*ab*L")
	
	# check 4: deriva tre monomials wrt "a"
	monomials <- monomialsFromString("-2*a^2*b*c*d*L^15+3*a*L+5*ab*L^2")
	result <- derive(monomials,wrt="a")
	checkEquals(toString(result),"-4*a*b*c*d*L^15 + 3*L + 0")

}

