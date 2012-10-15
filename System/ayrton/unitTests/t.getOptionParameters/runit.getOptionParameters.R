# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldGetPositionOptionsParameters <- function() {
	## uses a default method
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	repository <- createRepositoryAyrtonPositions()
	
	## test Opzioni_su_azioni
	origin <- repository$Opzioni_su_azioni1
	
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
	origin <- repository$Opzioni_su_divise1
	
	info <- getOptionParameters(origin)
	## "PUT 17-08-12 Strike 1.295 EUR 125000 Premio(-8293.75 USD)"
	checkEquals(info[["optionType"]],"P")
	checkEquals(info[["expiryDate"]],"2012-08-17")
	checkEquals(info[["strike"]],1.295)
	checkEquals(info[["underlying"]],"EUR")	
	checkEquals(info[["amount"]],125000)
	checkEquals(info[["premium"]],-8293.75)		
	checkEquals(info[["numeraire"]],"USD")
	
	## test Opzioni_su_obbligazioni
	origin <- repository$opzioni_su_obbligazioni
	
	info <- getOptionParameters(origin)
	## "PUT 17-08-12 Strike 103.5 EUR 125000 Premio(-345.45 EUR) EU0011027469 "
	checkEquals(info[["optionType"]],"P")
	checkEquals(info[["expiryDate"]],"2012-08-17")
	checkEquals(info[["strike"]],103.5)
	checkEquals(info[["underlyingCurrency"]],"EUR")	
	checkEquals(info[["amount"]],125000)
	checkEquals(info[["premium"]],-345.45)		
	checkEquals(info[["numeraire"]],"EUR")
	checkEquals(info[["isin"]],"EU0011027469")	
	
}