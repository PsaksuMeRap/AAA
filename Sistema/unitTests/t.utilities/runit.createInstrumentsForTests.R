# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_equityTest <- function() {
	source("./unitTests/utilities/createInstrumentsForTests.R")

	positions.l <- create_equityTestPositions()

	checkEquals(positions.l[["equityCHF1"]]$name,"Credit Suisse Group Na")
	checkEquals(positions.l[["equityEUR1"]]$money$amount,98)
}

test.create_Conto_correnteTestPositions <- function() {
	source("./unitTests/utilities/createInstrumentsForTests.R")
	
	positions.l <- create_Conto_correnteTestPositions()
	
	checkEquals(positions.l[[2]]$money$currency,"USD")
	checkEquals(positions.l[[1]]$money$amount,120)
	checkEquals(positions.l[[3]]$money$currency,"EUR")
}

test.create_ETF_EquityTestPositions <- function() {
	source("./unitTests/utilities/createInstrumentsForTests.R")
	
	positions.l <- create_ETF_equityTestPositions()
	
	checkEquals(positions.l[[1]]$money$amount,100)
	checkEquals(positions.l[[2]]$money$currency,"USD")
	checkEquals(positions.l[[3]]$money$currency,"CHF")
}

test.create_FX_ForwardTestPositions <- function() {
	source("./unitTests/utilities/createInstrumentsForTests.R")
	
	positions.l <- create_FX_ForwardTestPositions()
	
	checkEquals(positions.l[[1]]$money$amount,120)
	checkEquals(positions.l[[1]]$money$currency,"CHF")
	checkEquals(positions.l[[2]]$money$currency,"EUR")
}
