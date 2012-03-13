# TODO: Add comment
# 
# Author: claudio
###############################################################################


# questa parte di test riguarda i fondi nostri da esplodere 
# nei portafogli cliente
test.shouldIdentifyFundsToExplode <- function() {

	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$globalEconomy
	p2 <- repo$globalEquity
	p3 <- repo$fixedIncome	
	positions <- new("Positions",list(p1,p2,p3))
	
	fundsDb <- create_fundsDB()
	
	# identify GLOBAL EQUITY
	fundData <- as.list(fundsDb[1,,drop=FALSE])
	result <- identifyFundsToExplode(fundData,positions)
	checkEquals(result,c(FALSE,TRUE,FALSE))
	
	# identify FIXED INCOME fund (without accruedInterests)
	fundData <- as.list(fundsDb[3,,drop=FALSE])
	result <- identifyFundsToExplode(fundData,positions)
	checkEquals(result,c(FALSE,FALSE,TRUE))
	
	# identify GLOBAL ECONOMY
	fundData <- as.list(fundsDb[2,,drop=FALSE])
	result <- identifyFundsToExplode(fundData,positions)
	checkEquals(result,c(TRUE,FALSE,FALSE))
	
	# identify nothing
	fundData <- as.list(fundsDb[1,,drop=FALSE])
	fundData["id"] <- -13949
	result <- identifyFundsToExplode(fundData,positions)
	checkEquals(result,c(FALSE,FALSE,FALSE))
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
}
