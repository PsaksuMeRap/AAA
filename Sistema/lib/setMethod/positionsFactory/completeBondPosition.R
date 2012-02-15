# TODO: Add comment
# 
# Author: claudio
###############################################################################


completeBondPosition <- function(positionBond,accruedInterestPositions) {
	# positionBond: a variable of class PositionBond
	# accruedInterestPositions: a list of AyrtonPositions of type accruedInterest

	isMatchedAccruedInterest <- sapply(accruedInterestPositions,matchToPositionBond,positionBond)
	if (!any(isMatchedAccruedInterest)) {
		message <- "Error from completeBondPosition.\n The bond position:\n"
		message <- paste(message,as.character(positionBond),"\n",sep="")
		message <- paste(message,"has no matching accruedInterest.")
		stop(message)
	}
	matchedAccruedInterest <- accruedInterestPositions[isMatchedAccruedInterest]
	
	currency <- "CHF"
	amount <- matchedAccruedInterest[[1]]@ValoreMercatoMonetaCHF
	positionBond@accruedInterest <- new("AccruedInterest",toMoney(amount,currency))
	
	return(positionBond)
}


