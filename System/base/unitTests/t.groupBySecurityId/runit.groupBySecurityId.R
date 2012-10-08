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
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value	
	checkEquals(res,x)	
	
	# test PositionBond
	x <- repo$bond1
	x@accruedInterest <- new("AccruedInterest",toMoney(15.2,"EUR"))
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@accruedInterest <- 2*x@accruedInterest
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value	
	checkEquals(res,x)	
	
	# test PositionFondi_azionari
	x <- repo$Fondi_azionari1
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value	
	checkEquals(res,x)	
	
	# test PositionFondi_misti
	x <- repo$Fondi_misti
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value	
	checkEquals(res,x)	
	
	# test PositionFondi_obbligazionari
	x <- repo$fondiObbligazionariNoAC
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@accruedInterest <- 2*x@accruedInterest
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value	
	checkEquals(res,x)
	
	# test PositionFondi_obbligazionariOC
	x <- repo$fondiObbligazionari
	x@accruedInterest <- new("AccruedInterest",toMoney(15.2,"EUR"))
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@accruedInterest <- 2*x@accruedInterest
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value	
	checkEquals(res,x)
	
	# test PositionAnticipi_fissi
	x <- repo$Anticipi_fissi1
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@accruedInterest <- 2*x@accruedInterest
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value	
	checkEquals(res,x)	
	
	
	# test PositionConto_corrente_fittizio
	x <- repo$Conto_corrente_fittizio
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value	
	checkEquals(res,x)

	# test PositionConto_corrente
	x <- repo$Conto_corrente1
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value	
	checkEquals(res,x)
	
	# test PositionDepositi_a_termine
	x <- repo$Deposito_a_termine1
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@accruedInterest <- 2*x@accruedInterest
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value	
	checkEquals(res,x)	
	
	# test PositionPositionFutures_EQ
	x <- repo$Futures_EQ1
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value	
	checkEquals(res,x)	
	
	# test PositionFX_forward
	x <- repo$FX_forward
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value	
	checkEquals(res,x)		
	
	# test PositionOpzioni_su_azioni
	x <- repo$Opzioni_su_azioni1
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@numberEquities <- 2*x@numberEquities
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value
	x@security@name <- "-200 / Call / Syngenta AG / 17-02-12 / Strike 290 / Premio(11000 CHF) / CH0011027469 / 337.9 / 10"
	checkEquals(res,x)	
	
	# test PositionObbligazioni_convertibili
	x <- repo$Obbligazioni_convertibili 
	x@accruedInterest <- new("AccruedInterest",toMoney(5.5,"CHF"))
	x@positionCall <- repo$Opzioni_su_azioni1
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@positionCall <- groupBySecurityId(x@positionCall,y@positionCall)
	x@accruedInterest <- 2*x@accruedInterest
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value	
	checkEquals(res,x)
	
	# test PositionOpzioni_su_divise
	x <- repo$Opzioni_su_divise1
	y <- x
	
	res <- groupBySecurityId(x,y)
	x@quantity <- 2*x@quantity
	x@value <- 2*x@value
	security <- x@security
	security@name <- "P/2012-08-17/Strike 1.295/EUR 250000/Premium -16587.5 USD"
	x@security <- security
	checkEquals(res,x)	
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
}

