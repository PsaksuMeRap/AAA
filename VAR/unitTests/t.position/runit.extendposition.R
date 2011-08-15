# TODO: Add comment
# 
# Author: claudio
###############################################################################
test.shouldFailWithExtendPositionDefault <- function() {
	source("./lib/position/position.R")
	
	position <- create_position()
	position$create(name="Siemens N eur",
			currency="EUR",
			amount=131376.0
	)
	class(position) <- c("test")
	
	# check the new ticker field
	checkException(extendPosition(position))
}


test.shouldExtendPositionFondiMisti <- function() {
	source("./lib/position/position.R")
	source("./unitTests/utilities/allocateTestRepositories.R")

	allocateTestRepositories("instruments")
	
	position <- create_position()
	position$create(name="70-30 UBS Strategy Fund Yield CHF",
			currency="CHF",
			amount=131376.0
	)
	class(position) <- c("Fondi_misti",class(position))
	
	# check the percentage invested
	extendPosition(position,origin=list(ID_AAA=879))

	checkEquals(position$quotaEquities,70)
	checkEquals(position$quotaBonds,30)
	
	# use a differente percentage
	position$name <- "0.5-99.5 UBS Strategy Fund Yield CHF"
	extendPosition(position)
	
	checkEquals(position$quotaEquities,0.5)
	checkEquals(position$quotaBonds,99.5)
	
	# restore initial conditions
	deallocateTestRepositories("instruments")
	
}

test.shouldFailToExtendPositionFondiMisti <- function() {
	source("./lib/position/position.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("instruments")
	
	position <- create_position()
	position$create(name="70 -30 UBS Strategy Fund Yield CHF",
			currency="CHF",
			amount=131376.0
	)
	class(position) <- c("Fondi_misti",class(position))
	
		
	# generate an error because there is a space in 70-30
	checkException(extendPosition(position))
	
	# restore initial conditions
	deallocateTestRepositories("instruments")
	
}

test.shouldExtendPositionEquity <- function() {
	source("./lib/position/position.R")
    source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
		
	position <- create_position()
	position$create(name="Siemens N eur",
			currency="EUR",
			amount=131376.0
	)
	
	# the class assignment is executed in the position_parser function
	class(position) <- c("equity",class(position))

	# check the new ticker field
	extendPosition(position,origin=list(ID_AAA=879,NumeroValore="CH1234494pippo"))
	
	checkEquals(position$ticker,"SIE.XE")
	
	# check position$fieldsToPrint
	width=c(empty=TRUE)
	fields <- position$fieldsToPrintDefault(width) # giusto e già testato
	fields$ticker <- position$ticker
	
	fieldsResult <- position$fieldsToPrint() 
	checkEquals(fieldsResult,fields)
	
	# restore initial conditions
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
}

test.shouldExtendPositionAccruedInterest <- function() {
	source("./lib/position/position.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	record <- list(
			ID_strumento = 2,
			Nome="20111130 - 2.5% E.ON 30-11-11 Pro-rata",
			Moneta="EUR",
			ValorePosizione=4342.4658203125,
			ID_AAA=500,
			Strumento="Oacc"
	)
	
	parser <- create_parserPosition()
	position <- parser$parse(record)
	
	# create the accruedInterest
	money <- toMoney(4342.4658203125,"EUR")
	date <- "2011-11-30"
	accruedInterest <- create_accruedInterest(money,date)
    rm(money,date)
	
	# check the new fields and their properties
	checkEquals(class(position),c("accruedInterest","bond","position"))
	checkEquals(position$name,"20111130 - 2.5% E.ON")
	checkEquals(position$accruedInterest$paymentDate,"2011-11-30")
	checkEquals(position$accruedInterest,accruedInterest)
	
	# check position$fieldsToPrint
	width=c(empty=TRUE)
	fields <- position$fieldsToPrintDefault(width) # giusto e già testato
	fields$accruedInterest <- "Accrued interest"
	
	fieldsResult <- position$fieldsToPrint() 
	checkEquals(fieldsResult,fields)
	
	# restore initial conditions
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("exchangeRates")
}


test.shouldExtendPositionBond <- function() {
	source("./lib/position/position.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	record <- list(
			ID_strumento = 2,
			Nome="20110615 - 4.375% Carrefour 15-06-11",
			Moneta="EUR",
			ValorePosizione=203040.00,
			ID_AAA=500,
			Strumento="O"
	)
	
	parser <- create_parserPosition()
	position <- parser$parse(record)	
	
	checkEquals(class(position),c("bond","position"))
	checkEquals(position$name,"20110615 - 4.375% Carrefour 15-06-11")
	checkEquals(position$getMaturity(),"2011-06-15")

	
	# restore initial conditions
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("exchangeRates")
}

test.shouldFailToExtendPositionBond <- function() {
	source("./lib/position/position.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	record <- list(
			ID_strumento = 2,
			Nome="20110615 - 4.375% Carrefour 15-06-11 B",
			Moneta="EUR",
			ValorePosizione=203040.00,
			ID_AAA=500,
			Strumento="O"
	)
	
	parser <- create_parserPosition()
	position <- parser$parse(record)
	checkException(position$getMaturity())	
	
	
	# restore initial conditions
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("exchangeRates")
}

test.shouldExtendPositionStructuredProductFixedIncome <- function() {
	source("./lib/position/position.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	
	record <- list(
			ID_strumento = 49,
			Nome="20130521 - <3Y - Floored Floares with Cap 1.75%-4.625% p.a. On CS",
			Moneta="EUR",
			ValorePosizione=399892.3,
			ID_AAA=98,
			Strumento="Strutturati_FI"
	)
	
	parser <- create_parserPosition()
	position <- parser$parse(record)
	
#	name <- position$name
	# check if it is a short term fixed income position
	
#	if (grepl("<3Y",x=name)) position$underlyingHorizon = "<3Y"
#	if (grepl(">3Y",x=name)) position$underlyingHorizon = ">3Y"
#	year <- substr(name,1,4)
#	month <- substr(name,5,6)
#	day <- substr(name,7,8)
#	position$expiryDate = paste(year,month,day,sep="-")
	extendPosition(position)
	
	checkEquals(class(position),c("Strutturati_FI","position"))
	checkEquals(position$name,"20130521 - <3Y - Floored Floares with Cap 1.75%-4.625% p.a. On CS")
	checkEquals(position$underlyingHorizon,"<3Y")
	checkEquals(position$expiryDate,"2013-05-21")
	
	# restore initial conditions
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("exchangeRates")
}



test.shouldExtendPositionFX_Forward <- function() {
	source("./lib/position/position.R")
	source("./unitTests/utilities/allocateTestRepositories.R")

	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	
	record <- list(
			ID_strumento = 22,
			Nome="EUR -5,000,000.00 Valuta 01-02-2011",
			Moneta="EUR",
			ValorePosizione=203040.00,
			ID_AAA=0,
			Strumento=""
	)
	
	parser <- create_parserPosition()
	position <- parser$parse(record)	
	
	checkEquals(class(position),c("FX_Forward","position"))
	checkEquals(position$name,"EUR -5,000,000.00 Valuta 01-02-2011")
	checkEquals(position$expiry,"2011-02-01")
	
	# restore initial conditions
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("exchangeRates")
}

