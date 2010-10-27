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

