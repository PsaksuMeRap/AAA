# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shuldExtractPositionsFromPortfolio <- function() {
	
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$equity1
	p2 <- repo$bond1
	p3 <- repo$strutturati_FI
	p4 <- repo$equity2
	p5 <- repo$fondiObbligazionari
	positions <- new("Positions",list(p1,p2,p3,p4,p5))
	
	refCur1 <- new("Currency","USD")
	
	portfolio <- new("Portfolio",owner="Reto",referenceCurrency=refCur1,positions)
	
	# check with or without newOwner and refCurrency
	checkEquals(portfolio[[1]],p1)
	checkEquals(portfolio[2:3],new("Positions",list(p2,p3)))
	extract <- c(FALSE,TRUE,FALSE,FALSE,FALSE)
	checkEquals(portfolio[extract],new("Positions",list(p2)))	
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
}


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

test.shuldCountNumberPositionsInPortfolio <- function() {
	
	p1 <- new("Positions",list(1,"uno","mammamia"))
	
	refCur <- new("Currency","USD")
	portfolio <- new("Portfolio",owner="Reto",referenceCurrency=refCur,p1)
		
	# check with or without newOwner and refCurrency

	checkEquals(length(portfolio),3)
	
}