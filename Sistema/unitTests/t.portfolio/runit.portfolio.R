# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shuldJoinTwoPortfolios <- function() {

	positions1 <- new("Positions",list(1,"uno"))
	positions2 <- new("Positions",list(2,"due"))
	
	refCur1 <- new("Currency","USD")
	refCur2 <- new("Currency","CHF")
	
	portfolio1 <- new("Portfolio",owner="Reto",referenceCurrency=refCur1,positions1)
	portfolio2 <- new("Portfolio",owner="Claudio",referenceCurrency=refCur2,positions2)
	
	# check with or without newOwner and refCurrency
	result <- join(portfolio1,portfolio2)
	checkEquals(result@owner,"Reto_Claudio")
	checkEquals(result@.Data,list(1,"uno",2,"due"))
	checkEquals(result@referenceCurrency,refCur1) 
	
	refCur <- new("Currency","EUR")
	result <- join(portfolio1,portfolio2,newOwner="Attilio",newReferenceCurrency=refCur)
	checkEquals(result@owner,"Attilio")
	checkEquals(result@referenceCurrency,refCur)
}