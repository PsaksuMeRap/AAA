# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldAllocateTestInterestRates <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")

	allocateTestRepositories("interestRates")
	
	checkEquals(repositories$interestRates$getMonthTicker(12),"1Y")
	checkEquals(repositories$interestRates$getMonthTicker(1),"1M")
	checkEquals(repositories$interestRates$getMonthTicker(0.25),"1W")
	
	deallocateTestRepositories("interestRates")
}


test.shouldAllocateTestExchangeRates <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("exchangeRates")
	
	checkEquals(repositories$exchangeRates$rates[["ATS"]],0.0973132896811843)
	checkEquals(repositories$exchangeRates$rates[["EUR"]],1.33853808)
	
	deallocateTestRepositories("exchangeRates")
}



test.shouldAllocateTestEquities <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("equities")
	
	checkEquals(repositories$equities$equities.df[3,"equity"],"AVENTIS")
	checkEquals(repositories$equities$equities.df[9,"ticker"],"MIRZn.S")
	
	deallocateTestRepositories("equities")
}

test.shouldAllocateTestInstruments <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("instruments")
	
	checkEquals(repositories$instruments$getInstrumentName(1),"equity")
	checkEquals(repositories$instruments$getId("Obbligazioni convertibili"),11)
	
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

