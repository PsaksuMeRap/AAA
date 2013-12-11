# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldExplodePositionFundGlobalEquity <- function() {
	
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
	
	# check the identity when the fund is not global Equity fund
	p0 <- repo$Fondi_misti
	result0 <- explodeFundGlobalEquity(p0,list(Eq=0.1,Fi=0.3))
	checkEquals(p0,result0)
	
	# check the split when the fund is global Equity fund
	p1 <- repo$globalEquity # CHF
	result <- explodeFundGlobalEquity(p1,list(Eq=0.85,Fi=0))
	
	checkEquals(length(result),2)
	checkEquals(result[[1]]@security@name,"Eq. part of: OnCapital Global Equity Fund Cap B")
	checkEquals(result[[1]]@security@id,p1@security@id)
	checkEquals(result[[1]]@id,p1@id)
	checkEquals(is(result[[1]]@security,"Fondi_azionari"),TRUE)
	checkEquals(is(result[[2]]@security,"Conto_corrente_fittizio"),TRUE)
	checkEquals(result[[1]]@value@amount,p1@value@amount*0.85)
	checkEquals(result[[2]]@value@amount,p1@value@amount*(1-0.85))
	
	# check the split when the fund is global Equity fund
	p1 <- repo$globalEquity # CHF
	result <- explodeFundGlobalEquity(p1,list(Eq=0.4,Fi=0.4))
	
	checkEquals(length(result),3)
	checkEquals(result[[1]]@security@name,"Eq. part of: OnCapital Global Equity Fund Cap B")
	checkEquals(result[[1]]@security@id,p1@security@id)
	checkEquals(result[[1]]@id,p1@id)
	checkEquals(is(result[[1]]@security,"Fondi_azionari"),TRUE)
	checkEquals(is(result[[2]]@security,"Fondi_obbligazionari"),TRUE)
	checkEquals(is(result[[3]]@security,"Conto_corrente_fittizio"),TRUE)
	checkEquals(result[[1]]@value@amount,p1@value@amount*0.4)
	checkEquals(result[[2]]@value@amount,p1@value@amount*0.4)	
	checkEquals(result[[3]]@value@amount,p1@value@amount*0.2)
	
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
}
