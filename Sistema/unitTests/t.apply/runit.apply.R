# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldApplyDirectiveString <- function() {
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
	
	p1 <- repo$Fondi_misti # CHF
	p2 <- repo$equity1
	p3 <- repo$bond1
	positions <- new("Positions",list(p1,p2,p3))
	directiveString <- new("DirectiveString","explode:Fondi_misti")
	
	result <- apply(X=directiveString,positions=positions)
	
	
	checkEquals(is(result[[3]]@security,"Fondi_azionari"),TRUE)
	checkEquals(is(result[[4]]@security,"Fondi_obbligazionari"),TRUE)
	checkEquals(identical(result[[1]],positions[[2]]),TRUE)
	checkEquals(identical(result[[2]],positions[[3]]),TRUE)
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
}
