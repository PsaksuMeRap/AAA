# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldCreateUnclassifiedPosition <- function() {	
	
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")

	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$unclassified1
	
	unclassified <- securityFactory(origin)
	
	position <- createPosition(unclassified,origin)
	
	checkEquals(position@id,10.2)	
	checkEquals(position@quantity,100.3)
	checkEquals(position@value,toMoney(new("Amount",123.55),new("Currency","CHF")))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldBeNullPositionFromAccruedInterest <- function() {	
	
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$proRata1
	
	security <- securityFactory(origin)
	
	position <- createPosition(security,origin)
	
	checkEquals(position,NULL)
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldCreatePositionEquity <- function() {	
	
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$equity1
	
	equitySecurity <- securityFactory(origin)
	
	equityPosition <- createPosition(equitySecurity,origin)
	
	checkEquals(is(equityPosition)[1],"PositionEquity")
	checkEquals(equityPosition@id,10.2)	
	checkEquals(equityPosition@quantity,15)
	checkEquals(equityPosition@value,toMoney(new("Amount",88205),new("Currency","CHF")))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldCreatePositionBond <- function() {	
	
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$bond1
	
	bondSecurity <- securityFactory(origin)
	
	bondPosition <- createPosition(bondSecurity,origin)
	
	checkEquals(is(bondPosition)[1],"PositionBond")
	checkEquals(bondPosition@id,10.2)
	checkEquals(bondPosition@accruedInterest@amount,new("Amount",NA_real_))	
	checkEquals(bondPosition@quantity,new("NominalValue",amount=new("Amount",100000),currency=new("Currency","EUR")))
	checkEquals(bondPosition@value,toMoney(124345.632268,"CHF"))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}
