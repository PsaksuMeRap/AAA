# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldCreateUnclassified <- function() {
	# uses a default method
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$equity1
	class(origin) <- "Unclassified"
	
	unclassified <- createSecurity(origin)
	class(unclassified) <- "Unclassified"

	checkEquals(unclassified@name,"Roche Holding Gs")
	checkEquals(unclassified@id,new("IdAyrton",idAAA=824,idStrumento=1))
	
}


test.shouldCreateEquity <- function() {
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the equity repository and instrument repository
	allocateTestRepositories("equities")	
	allocateTestRepositories("instruments")
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$equity1
	class(origin) <- "Equity"
	
	equity <- createSecurity(origin)
	
	checkEquals(equity@name,"Roche Holding Gs")
	checkEquals(equity@id,new("IdAyrton",idAAA=824,idStrumento=1))
	
	# restore initial conditions	
	deallocateTestRepositories("equities")	
	deallocateTestRepositories("instruments")	
}

test.shouldCreateBond <- function() {
	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$bond1
	class(origin) <-  "Bond"

	bond <- createSecurity(origin)
	
	checkEquals(bond@name,"20130603 - 3.625% Pfizer 03-06-13")
	checkEquals(bond@id,new("IdAyrton",idAAA=1218,idStrumento=2))
	checkEquals(bond@maturity,"2013-06-03")
		
}

test.shouldNULLForAccruedInterest <- function() {
	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the equity repository and instrument repository
	allocateTestRepositories("instruments")	
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$proRata1
	class(origin) <- "Bond"

	null <- createSecurity(origin)
	
	checkEquals(is.null(null),TRUE)
	
}


