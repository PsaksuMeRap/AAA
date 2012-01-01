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
}


