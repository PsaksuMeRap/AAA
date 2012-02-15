# TODO: Add comment
# 
# Author: claudio
###############################################################################


adjustForAccruedInterest <- function(positions,accruedInterestPositions) {
	# result: a list of positions (is not a variable of class Positions!)
	# accruedInterestPositions: a list of AyrtonPositions "of type" accruedInterest
	
	areBondPositions <- sapply(positions,is,"BondPosition")
	if (any(areBondPositions)) {
		# extract the bondPositions
		bondPositions <- positions[areBondPositions]
		# complete the slot accruedInterest with the corresponding money amount
		bondPositions <- lapply(bondPositions,completeBondPosition,accruedInterestPositions)
		positions[areBondPositions] <- bondPositions
	}
	
	return(positions)
}
