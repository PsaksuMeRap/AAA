# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldGetPositionOpzioni_su_azioniParameters <- function() {

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
	
	## test Opzioni_su_azioni
	origin <- repo$Opzioni_su_azioni1
	
	info <- getOptionParameters(origin)
	## "-100 / Call / Syngenta AG / 17-02-12 / Strike 290 / Premio(5500 CHF) / CH0011027469 / 337.90 / 10"
	checkEquals(info[["quantity"]],-100)
	checkEquals(info[["optionType"]],"C")	
	checkEquals(info[["name"]],"Syngenta AG")	
	checkEquals(info[["expiryDate"]],"2012-02-17")
	checkEquals(info[["strike"]],290)
	checkEquals(info[["premium"]],"Premio(5500 CHF)")	
	checkEquals(info[["isin"]],"CH0011027469")	
	checkEquals(info[["underlyingPrice"]],337.90)
	checkEquals(info[["contractSize"]],10)
	

	## test Opzioni_su_divise
	origin <- repo$Opzioni_su_divise1
	
	info <- getOptionParameters(origin)
	## "PUT 17-08-12 Strike 1.295 EUR 125000 Premio(-8293.75 USD)"
	checkEquals(info[["optionType"]],"P")
	checkEquals(info[["expiryDate"]],"2012-08-17")
	checkEquals(info[["strike"]],1.295)
	checkEquals(info[["underlying"]],"EUR")	
	checkEquals(info[["amount"]],125000)
	checkEquals(info[["premium"]],-8293.75)		
	checkEquals(info[["numeraire"]],"USD")
	
}