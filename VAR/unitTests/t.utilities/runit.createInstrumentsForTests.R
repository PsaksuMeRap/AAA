# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_equityTest <- function() {
	source("./unitTests/utilities/createInstrumentsForTests.R")

	positions <- create_equityTestPositions()

	checkEquals(positions$positions[[1]]$name,"Converium N")
	checkEquals(positions$positions[[3]]$amount,98)
}

test.create_Conto_correnteTestPositions <- function() {
	source("./unitTests/utilities/createInstrumentsForTests.R")
	
	positions <- create_Conto_correnteTestPositions()
	
	checkEquals(positions$positions[[2]]$currency,"USD")
	checkEquals(positions$positions[[1]]$amount,120)
	checkEquals(positions$positions[[3]]$currency,"EUR")
}

test.create_ETF_EquityTestPositions <- function() {
	source("./unitTests/utilities/createInstrumentsForTests.R")
	
	positions <- create_ETF_equityTestPositions()
	
	checkEquals(positions$positions[[1]]$amount,100)
	checkEquals(positions$positions[[2]]$currency,"USD")
	checkEquals(positions$positions[[3]]$currency,"CHF")
}

test.create_FX_ForwardTestPositions <- function() {
	source("./unitTests/utilities/createInstrumentsForTests.R")
	
	positions <- create_FX_ForwardTestPositions()
	
	checkEquals(positions$positions[[1]]$amount,120)
	checkEquals(positions$positions[[1]]$currency,"CHF")
	checkEquals(positions$positions[[2]]$currency,"EUR")
}
