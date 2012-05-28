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
	class(origin) <- "Ayrton_Unclassified"
	
	unclassified <- createSecurity(origin)

	checkEquals(unclassified@name,"Roche Holding Gs")
	checkEquals(unclassified@currency,new("Currency","CHF"))
	checkEquals(unclassified@id,new("IdAyrton",idAAA=new("IdAAA_numeric",824),idStrumento=1))
	
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
	class(origin) <- "Ayrton_Equity"
	
	equity <- createSecurity(origin)
	
	checkEquals(equity@name,"Roche Holding Gs")
	checkEquals(equity@currency,new("Currency","CHF"))
	checkEquals(equity@id,new("IdAyrton",idAAA=new("IdAAA_numeric",824),idStrumento=1))
	
	# same test but with equity in EUR
	origin <- repository$equity2
	class(origin) <- "Ayrton_Equity"

	equity <- createSecurity(origin)

	checkEquals(equity@currency,new("Currency","EUR"))
	
	
	# restore initial conditions	
	deallocateTestRepositories("equities")	
	deallocateTestRepositories("instruments")	
}

test.shouldCreateBond <- function() {
	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$bond1
	class(origin) <- "Ayrton_Bond"

	bond <- createSecurity(origin)
	
	checkEquals(bond@name,"20130603 - 3.625% Pfizer 03-06-13")
	checkEquals(bond@id,new("IdAyrton",idAAA=new("IdAAA_numeric",1218),idStrumento=2))
	checkEquals(bond@maturity,"2013-06-03")
		
}

test.shoulCreateTimeDeposit <- function() {
	# this is valid for Anticipi fissi and depositi a termine
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Anticipi_fissi1
	class(origin) <- "Anticipi_fissi"
	
	anticipiFissi <- createSecurity(origin)
	checkEquals(anticipiFissi@name,"20130603 - 3.625% Pfizer 03-06-13")
	checkEquals(anticipiFissi@id,new("IdAyrton",idAAA=new("IdAAA_numeric",1218),idStrumento=2))
	checkEquals(anticipiFissi@maturity,"2013-06-03")
	
	
}



test.shouldNULLForAccruedInterest <- function() {
	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the equity repository and instrument repository
	allocateTestRepositories("instruments")	
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$proRata1
	class(origin) <- "Ayrton_Bond"

	null <- createSecurity(origin)
	
	checkEquals(is.null(null),TRUE)
	
}

test.shouldCreateStrutturati_FI <- function() {
	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the equity repository and instrument repository
	allocateTestRepositories("instruments")	
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$strutturati_FI
	class(origin) <- "Ayrton_Strutturati_FI"
	
	security <- createSecurity(origin)
	
	checkEquals(security@expiryDate,"2013-05-21")
	checkEquals(security@underlyingHorizon,"<3Y")
	
}
