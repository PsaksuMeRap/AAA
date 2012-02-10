# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldCreateUnclassifiedPosition <- function() {	
	
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryForOriginTestData.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")

	# create the origin
	repository <- createRepositoryForOriginTestData()
	origin <- repository$unclassified1
	
	unclassified <- securityFactory(origin)
	
	position <- createPosition(unclassified,origin)
	
	checkEquals(position@id,10.2)	
	checkEquals(position@quantity,100.3)
	checkEquals(position@value,toMoney(123.55,new("Currency","CHF")))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldBeNullPositionFromAccruedInterest <- function() {	
	
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryForOriginTestData.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origin
	repository <- createRepositoryForOriginTestData()
	origin <- repository$proRata1
	
	security <- securityFactory(origin)
	
	position <- createPosition(security,origin)
	
	checkEquals(position,NULL)
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}