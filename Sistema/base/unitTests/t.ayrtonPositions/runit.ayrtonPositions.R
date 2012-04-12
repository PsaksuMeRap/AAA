# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldCreateAyrtonPositions <- function() {
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	repository <- createRepositoryAyrtonPositions()
	positions <- list(repository$equity1,repository$equity2,repository$bond1)
	
	result <- new("AyrtonPositions",positions)
	checkEquals(is(result,"AyrtonPositions"),TRUE)
}


test.shouldExtractAyrtonPositions <- function() {
	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	repository <- createRepositoryAyrtonPositions()
	
	# create the Positions
	positions <- list(repository$equity1,repository$equity2,repository$bond1)
	Positions <- new("AyrtonPositions",positions)
	
	# test 1: use numeric indices
	result <- Positions[c(1,3)]
	checkEquals(is(result,"AyrtonPositions"),TRUE)
	checkEquals(result,new("AyrtonPositions",positions[-2]))
	
	# test 2: use numeric indices
	result <- Positions[c(TRUE,FALSE,TRUE)]
	checkEquals(is(result,"AyrtonPositions"),TRUE)
	checkEquals(result,new("AyrtonPositions",positions[-2]))

}