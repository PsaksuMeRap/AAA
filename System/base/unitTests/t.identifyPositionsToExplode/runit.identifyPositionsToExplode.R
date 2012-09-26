# TODO: Add comment
# 
# Author: claudio
###############################################################################


# questa parte di test riguarda i fondi nostri da esplodere 
# nei portafogli cliente
test.shouldIdentifyPositionsToExplode <- function() {

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
	
	p1 <- repo$globalEconomy
	p2 <- repo$globalEquity
	p3 <- repo$fixedIncome	
	positions <- new("Positions",list(p1,p2,p3))
	
	fundsDb <- create_fundsDB()
	
	# identify GLOBAL EQUITY
	fundData <- fundsDb[[1]]
	result <- identifyPositionsToExplode(fundData,positions)
	checkEquals(result,c(FALSE,TRUE,FALSE))
	
	# identify FIXED INCOME fund (without accruedInterests)
	fundData <- fundsDb[[3]]
	result <- identifyPositionsToExplode(fundData,positions)
	checkEquals(result,c(FALSE,FALSE,TRUE))
	
	# identify GLOBAL ECONOMY
	fundData <- fundsDb[[2]]
	result <- identifyPositionsToExplode(fundData,positions)
	checkEquals(result,c(TRUE,FALSE,FALSE))
	
	# identify nothing
	fundData <- fundsDb[[1]]
	fundData@id <- "sxlkdfksdfj"
	result <- identifyPositionsToExplode(fundData,positions)
	checkEquals(result,c(FALSE,FALSE,FALSE))
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
}
