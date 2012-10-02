# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldGroupById <- function() {
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# create the positions
	positions <- list(repository$equity1,repository$Futures_EQ1,repository$equity2,repository$bond1)
		
	# check with an empty list of Positions
	result <- groupBy(new("Positions"),"securityId")
	checkEquals(result,new("Positions"))
	
	# check with a Positions variable with 1 element
	positions <- new("Positions",list(repository$equity1))
	result <- groupBy(positions,"securityId")
	checkEquals(result,positions)
	
	# check with a Positions variable with 2 equal elements
	positions <- new("Positions",list(repository$equity1,repository$equity2,repository$equity1))
	result <- groupBy(positions,"securityId")
	should <- repository$equity1
	should@quantity <- 2*should@quantity
	should@value <- 2*should@value
	checkEquals(result[[1]],should)
	checkEquals(result[[2]],repository$equity2)
	
	# check with a Positions variable with 3 equal elements
	positions <- new("Positions",list(repository$equity1,repository$equity2,repository$equity1,repository$equity1))
	result <- groupBy(positions,"securityId")
	should <- repository$equity1
	should@quantity <- 3*should@quantity
	should@value <- 3*should@value
	checkEquals(result[[1]],should)
	checkEquals(result[[2]],repository$equity2)
	
}
