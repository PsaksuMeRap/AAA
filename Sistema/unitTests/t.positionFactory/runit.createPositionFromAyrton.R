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
	
	position <- createPositionFromAyrton(unclassified,origin)
	
	checkEquals(position$id,10.2)
	checkEquals(position@owner,"pippo13")	
	checkEquals(position@quantity,100.3)
	checkEquals(position@value,toMoney(123.55,"CHF"))
	
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
	
	position <- createPositionFromAyrton(security,origin)
	
	checkEquals(position,NULL)
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}