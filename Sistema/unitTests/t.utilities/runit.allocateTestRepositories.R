# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.shouldAllocateTestExchangeRates <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("exchangeRates")
	
	checkEquals(repositories$exchangeRates$rates[["ATS"]],0.0973132896811843)
	checkEquals(repositories$exchangeRates$rates[["EUR"]],1.33853808)
	
	deallocateTestRepositories("exchangeRates")
}


test.shouldAllocateTestInstruments <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("instruments")
	
	checkEquals(repositories$instruments$getInstrumentName(1),"Equity")
	checkEquals(repositories$instruments$getId("Obbligazioni_convertibili"),11)
	
	deallocateTestRepositories("instruments")
}

test.shouldAllocateTestPoliticaInvestimento <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("politicaInvestimento")
	
	owner <- "pippo47"
	isDesiredOwner <- repositories$politicaInvestimento$politicaInvestimento.df[,"ID"] == owner
	refCurrency <- repositories$politicaInvestimento$politicaInvestimento.df[isDesiredOwner,"MonetaInvestimento"]	
	checkEquals(refCurrency,"USD")
	
	owner <- "pippo165"
	isDesiredOwner <- repositories$politicaInvestimento$politicaInvestimento.df[,"ID"] == owner
	refCurrency <- repositories$politicaInvestimento$politicaInvestimento.df[isDesiredOwner,"MonetaInvestimento"]
	checkEquals(refCurrency,"EUR")
	
	owner <- "pippo66"
	isDesiredOwner <- repositories$politicaInvestimento$politicaInvestimento.df[,"ID"] == owner
	refCurrency <- repositories$politicaInvestimento$politicaInvestimento.df[isDesiredOwner,"MonetaInvestimento"]	
	checkEquals(refCurrency,"CHF")
	
	deallocateTestRepositories("politicaInvestimento")
}
