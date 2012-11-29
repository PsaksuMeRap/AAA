# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldExplodePositionFondi_misti <- function() {
	
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
	
	p1 <- repo$Fondi_misti # CHF
	
	result <- explode(p1)
	
	checkEquals(result[[1]]@security@name,"Fondo misto: 70-30 UBS Strategy Fund Yield CHF")
	checkEquals(result[[1]]@value@amount,p1@value@amount*0.3)
	checkEquals(result[[2]]@value@amount,p1@value@amount*0.7)
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
}
