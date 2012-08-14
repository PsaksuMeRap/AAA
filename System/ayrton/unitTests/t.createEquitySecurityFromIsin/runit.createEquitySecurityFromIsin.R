# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateEquitySecurityFromIsin <- function() {
	
	allocateTestRepositories("DBEquities")
	
	# test1: no corresponding equity
	isin <- "CH0011027469"
	security <- createEquitySecurityFromIsin(isin)
	checkEquals(security@currency,new("Currency"))
	
	# test2: with Credit-Suisse
	isin <- "CH0012138530"
	security <- createEquitySecurityFromIsin(isin)
	checkEquals(security@currency,new("Currency","CHF"))
	
	deallocateTestRepositories("DBEquities")
}
