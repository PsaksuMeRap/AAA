# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateEquitySecurityFromIsin <- function() {
	source("./base//lib/repository.R")
	source("./base/unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("DBEquities")
	
	# test1: no corresponding equity
	isin <- "CH0011027469"
	security <- createEquitySecurityFromIsin(isin)
	checkEquals(security@currency,new("Currency"))
	
	# test2: with Credit-Suisse
	isin <- "CH0012138530"
	security <- createEquitySecurityFromIsin(isin)
	checkEquals(security@currency,new("Currency","CHF"))
	
	
	# test3: with two equities with the same ISIN code
	isin <- "DE0007236101"
	security <- createEquitySecurityFromIsin(isin,"EUR")
	checkEquals(security@currency,new("Currency","EUR"))
	
	deallocateTestRepositories("DBEquities")
}

test.shouldFailToCreateEquitySecurityFromIsin <- function() {
	source("./base//lib/repository.R")
	source("./base/unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("DBEquities")
	
	
	# test1: with two equities with the same ISIN code
	#        but no currency input
	isin <- "DE0007236101"
	checkException(createEquitySecurityFromIsin(isin))
	
}
