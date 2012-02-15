# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldMatchToPositionBond <- function() {
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	source("./unitTests/utilities/createRepositoryPositions.R")	
	
	# create the origins
	repository1 <- createRepositoryAyrtonPositions()
	repository2 <- createRepositoryPositions()
	
	bond <- repository2$bond4
	accruedInterestAyrton <-  repository1$proRata1
	
	checkEquals(matchToPositionBond(accruedInterestAyrton,bond),TRUE)
	
}

test.shouldFailToMatchToPositionBond <- function() {
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	source("./unitTests/utilities/createRepositoryPositions.R")	
	
	# create the origins
	repository1 <- createRepositoryAyrtonPositions()
	repository2 <- createRepositoryPositions()
	
	bond <- repository2$bond3
	accruedInterestAyrton <-  repository1$proRata1
	
	checkEquals(matchToPositionBond(accruedInterestAyrton,bond),FALSE)
	
}