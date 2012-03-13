# TODO: Add comment
# 
# Author: claudio
###############################################################################


weightPositions <- function(positions,weight) {
	return(invisible(lapply(positions$positions,weightPosition,weight)))
}

areConsistent <- function(positions) {
	isConsistent <- function(position) {
		return(position$isConsistent())
	}
	
	areConsistent <- sapply(positions$positions,isConsistent)
	return(areConsistent)
}

