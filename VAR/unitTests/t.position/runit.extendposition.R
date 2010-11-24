# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldExtendPositionEquity <- function() {
	source("./lib/position/position.R")
    source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
		
	position <- create_position()
	position$create(name="Siemens N eur",
			currency="EUR",
			amount=131376.0,
			origin=list(ID_AAA=879)
	)
	class(position) <- c("equity",class(position))

	extendPosition(position)
	checkEquals(position$ticker,"SIE.XE")
	
	# restore initial conditions
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	
}

test.shouldExtendPositionBond <- function() {
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
	
	checkEquals(class(position),c("accruedInterest","bond","position"))
	checkEquals(position$name,"20111130 - 2.5% E.ON")
	checkEquals(position$accruedInterest$paymentDate,"2011-11-30")
	checkEquals(position$accruedInterest,accruedInterest)
	
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
			Strumento="Strutturati FI"
	)
	
	parser <- create_parserPosition()
	position <- parser$parse(record)
	
	name <- position$name
	# check if it is a short term fixed income position
	
	if (grepl("<3Y",x=name)) position$underlyingHorizon = "<3Y"
	if (grepl(">3Y",x=name)) position$underlyingHorizon = ">3Y"
	
	
	checkEquals(class(position),c("accruedInterest","bond","position"))
	checkEquals(position$name,"20111130 - 2.5% E.ON")
	checkEquals(position$accruedInterest$paymentDate,"2011-11-30")
	checkEquals(position$accruedInterest,accruedInterest)
	
	# restore initial conditions
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("exchangeRates")
}
