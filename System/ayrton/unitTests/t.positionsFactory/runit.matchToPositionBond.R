# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldMatchPositionToAyrtonPosition <- function() {
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	source("./base/unitTests/utilities/createRepositoryPositions.R")	
	
	# create the origins
	repository1 <- createRepositoryAyrtonPositions()
	repository2 <- createRepositoryPositions()
	
	bond <- repository2$bond4
	accruedInterestAyrton <-  repository1$proRata1
	
	checkEquals(matchPositionToAyrtonPosition(accruedInterestAyrton,bond),TRUE)
	
}

test.shouldFailToMatchPositionToAyrtonPosition <- function() {
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	source("./base/unitTests/utilities/createRepositoryPositions.R")	
	
	# create the origins
	repository1 <- createRepositoryAyrtonPositions()
	repository2 <- createRepositoryPositions()
	
	bond <- repository2$bond3
	accruedInterestAyrton <-  repository1$proRata1
	
	checkEquals(matchPositionToAyrtonPosition(accruedInterestAyrton,bond),FALSE)
	
}