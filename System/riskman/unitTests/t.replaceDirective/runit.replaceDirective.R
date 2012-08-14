# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldReplacePositionFutures_EQ <- function() {
	
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# create the positions
	positions <- list(repository$equity1,repository$Futures_EQ1,repository$equity2,repository$bond1)
	
	result <- replaceDirective(positions[[2]])
	
	checkEquals(length(result),2)
	checkEquals(is(result[[1]],"PositionFutures_EQ"),TRUE)
	checkEquals(is(result[[2]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[1]]@value,-1 * result[[2]]@value)
	
}