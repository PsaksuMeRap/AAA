# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldGroupTwoPositionsWithSameSecurityId <- function() {
	
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
	
	
	# test PositionEquity
	x <- repo$equity1
	y <- repo$equity1
	
	res <- groupBySecurityId(x,y)
	checkEquals(res@id,x@id)
	checkEquals(res@security,x@security)
	checkEquals(res@quantity,2*x@quantity)
	checkEquals(res@value,2*x@value)
	
	# test PositionBond
	x <- repo$bond1
	y <- repo$bond1
	
	res <- groupBySecurityId(x,y)
	checkEquals(res@accruedInterest,2*x@accruedInterest)
	checkEquals(res@spRating,x@spRating)
	checkEquals(res@id,x@id)
	checkEquals(res@security,x@security)
	checkEquals(res@quantity,2*x@quantity)
	checkEquals(res@value,2*x@value)
	
	# test PositionFondi_azionari
	x <- repo$Fondi_azionari1
	y <- repo$Fondi_azionari1
	
	res <- groupBySecurityId(x,y)
	checkEquals(res@id,x@id)
	checkEquals(res@security,x@security)
	checkEquals(res@quantity,2*x@quantity)
	checkEquals(res@value,2*x@value)
	
	# test PositionFondi_misti
	x <- repo$Fondi_misti
	y <- repo$Fondi_misti
	
	res <- groupBySecurityId(x,y)
	checkEquals(res@id,x@id)
	checkEquals(res@security,x@security)
	checkEquals(res@quantity,2*x@quantity)
	checkEquals(res@value,2*x@value)	
	
	# test PositionFondi_obbligazionari
	x <- repo$fondiObbligazionariNoAC
	y <- repo$fondiObbligazionariNoAC
	
	res <- groupBySecurityId(x,y)
	checkEquals(res@accruedInterest,2*x@accruedInterest)
	checkEquals(res@spRating,x@spRating)
	checkEquals(res@id,x@id)
	checkEquals(res@security,x@security)
	checkEquals(res@quantity,2*x@quantity)
	checkEquals(res@value,2*x@value)
	
	# test PositionFondi_obbligazionariOC
	x <- repo$fondiObbligazionari
	y <- repo$fondiObbligazionari
	
	res <- groupBySecurityId(x,y)
	checkEquals(res@accruedInterest,2*x@accruedInterest)
	checkEquals(res@id,x@id)
	checkEquals(res@security,x@security)
	checkEquals(res@quantity,2*x@quantity)
	checkEquals(res@value,2*x@value)	
	
	checkEquals(TRUE,FALSE)
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
}

