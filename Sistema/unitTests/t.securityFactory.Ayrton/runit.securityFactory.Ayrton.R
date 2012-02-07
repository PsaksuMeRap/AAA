# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldFailWithNonAyrton <- function() {
	
	# create the origin
	origin <- list()
	class(origin) <- "pippo"
	
	checkException(securityFactory(origin))
}

test.shouldTestEquity <- function() {
	
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryForOriginTestData.R")
	
	# create the equity repository and instrument repository
	allocateTestRepositories("equities")	
	allocateTestRepositories("instruments")
	
	# create the origin
	repository <- createRepositoryForOriginTestData()
	origin <- repository$equity1

	equity <- securityFactory(origin)
	
	checkEquals(equity@name,"Roche Holding Gs")
	checkEquals(equity@id,new("IdAyrton",idAAA=824,idStrumento=1))
	
	# restore initial conditions	
	deallocateTestRepositories("equities")	
	deallocateTestRepositories("instruments")	
}

test.shouldTestBond <- function() {
	
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryForOriginTestData.R")
	
	# create the equity repository and instrument repository
	allocateTestRepositories("instruments")	
	
	# create the origin
	repository <- createRepositoryForOriginTestData()
	origin <- repository$bond1
	
	bond <- securityFactory(origin)
	
	checkEquals(bond@name,"20130603 - 3.625% Pfizer 03-06-13")
	checkEquals(bond@id,new("IdAyrton",idAAA=1218,idStrumento=2))
	checkEquals(bond@maturity,"2013-06-03")
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")	
}

