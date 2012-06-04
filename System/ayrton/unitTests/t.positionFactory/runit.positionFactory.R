# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateDefaultPosition <- function() {
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$unclassified1
	
	position <- positionFactory(origin)
	
	checkEquals(position@quantity,100.3)
	checkEquals(position@value,toMoney(123.55,new("Currency","CHF")))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}
