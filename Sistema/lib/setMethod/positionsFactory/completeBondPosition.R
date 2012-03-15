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
	# compute the value of the accruedInterest
	amount <- matchedAccruedInterest[[1]]@ValoreMercatoMonetaCHF
	value <- toMoney(amount,"CHF")
	value <- repositories$exchangeRates$exchange(value,positionBond@security@currency)
	accruedInterest <- new("AccruedInterest",value)
	
	# compute the dirty value of the bond
	dirtyValue <- positionBond@value + accruedInterest
	positionBond <- new("PositionBond",
			accruedInterest=accruedInterest,
			id=positionBond@id,
			security=positionBond@security,
			quantity=positionBond@quantity,
			value=dirtyValue)


	return(positionBond)
}


