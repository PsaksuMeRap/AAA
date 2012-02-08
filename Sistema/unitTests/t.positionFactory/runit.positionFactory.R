# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateDefaultPosition <- function() {
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryForOriginTestData.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origin
	repository <- createRepositoryForOriginTestData()
	origin <- repository$unclassified1
	
	position <- positionFactory(origin)
	
	checkEquals(position@quantity,100.3)
	checkEquals(position@value,toMoney(123.55,"CHF"))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}
