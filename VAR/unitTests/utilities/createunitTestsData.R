# TODO: Add comment
# 
# Author: claudio
###############################################################################


createUnitTestsData <- function() {
	instruments <- create_repositoryInstruments()
	save("instruments",file="./unitTests/data/instrumentsRepo_RData")
	
	equities <- create_repositoryEquities()
	save("equities",file="./unitTests/data/equitiesRepo_RData")
	
	interestRates <- create_repositoryInterestRates()
	save("interestRates",file="./unitTests/data/interestRatesRepo_RData")
	
	discountFactors <- create_repositoryDiscountFactors()
	save("discountFactors",file="./unitTests/data/discountFactorsRepo_RData")
	
	exchangeRates <- create_repositoryExchangeRates()
	save("exchangeRates",file="./unitTests/data/exchangeRatesRepo_RData")
}

