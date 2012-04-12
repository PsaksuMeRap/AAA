# TODO: Add comment
# 
# Author: claudio
###############################################################################


adjustForAccruedInterest <- function(positions,accruedInterestPositions) {
	# result: a list of positions (is not a variable of class Positions!)
	# accruedInterestPositions: a list of AyrtonPositions "of type" accruedInterest
	
	slotNames <- lapply(positions,slotNames)
	
	haveAccruedInterest <- sapply(slotNames,function(x,y)return(is.element(y,x)),"accruedInterest")
	
	if (any(haveAccruedInterest)) {
		# extract the positions having an accruedInterest
		positionsWithAccruedInterest <- positions[haveAccruedInterest]
		
		# complete the slot accruedInterest with the corresponding money amount
		positionsWithAccruedInterest <- lapply(positionsWithAccruedInterest,completePositionWithAccruedInterest,accruedInterestPositions)
		positions[haveAccruedInterest] <- positionsWithAccruedInterest
	}
	
	return(positions)
}
