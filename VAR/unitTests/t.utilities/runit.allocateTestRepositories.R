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
	
	checkEquals(repositories$exchangeRates$rates[["ATS"]],0.0972753559152053)
	checkEquals(repositories$exchangeRates$rates[["EUR"]],1.33853808)
	
	deallocateTestRepositories("exchangeRates")
}



test.shouldAllocateTestEquities <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("equities")
	
	checkEquals(repositories$equities$equities.df[3,"equity"],"AVENTIS")
	checkEquals(repositories$equities$equities.df[9,"numeroValore"],"1009374CH")
	
	deallocateTestRepositories("equities")
}

test.shouldAllocateTestInstruments <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("instruments")
	
	checkEquals(repositories$instruments$getInstrumentName(1),"equity")
	checkEquals(repositories$instruments$getId("Obbligazioni convertibili"),11)
	
	deallocateTestRepositories("instruments")
}


