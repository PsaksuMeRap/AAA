# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateRepositoryAllocareInvestementType <- function() {
	
	repositoryAllocareInvestmentType <- create_repositoryAllocareInvestmentType()
	
	# test1: should return the equity ID
	
	checkEquals(repositoryAllocareInvestmentType$getId("Ordinary Stocks"),1)
	
	# test2: should return NA 
	checkEquals(repositoryAllocareInvestmentType$getId("xyt"),NA_integer_)
	
}
