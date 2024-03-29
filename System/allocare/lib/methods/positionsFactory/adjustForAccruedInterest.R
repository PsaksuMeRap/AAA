# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./ayrton/lib/methods/positionsFactory/completePositionWithAccruedInterest.R")

adjustForAccruedInterest <- function(positions,accPositions) {
	# result: a list of positions (is not a variable of class Positions!)
	# accPositions: a list of AyrtonPositions of "type" accruedInterest
	
	slotNames <- lapply(positions,slotNames)
	
	haveAccruedInterest <- sapply(slotNames,function(x,y)return(is.element(y,x)),"accruedInterest")
	
	if (any(haveAccruedInterest)) {
		# extract the positions having an accruedInterest
		positionsWithAccruedInterest <- positions[haveAccruedInterest]
		
		# complete the slot accruedInterest with the corresponding money amount
		positionsWithAccruedInterest <- lapply(positionsWithAccruedInterest,completePositionWithAccruedInterest,accPositions)
		positions[haveAccruedInterest] <- positionsWithAccruedInterest
	}
	
	return(positions)
}
