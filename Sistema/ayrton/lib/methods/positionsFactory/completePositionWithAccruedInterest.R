# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./ayrton/lib/methods/positionsFactory/matchPositionToAyrtonPosition.R")

completePositionWithAccruedInterest <- function(positionWithAcc,accruedInterestPositions) {
	# positionWithAcc: a variable having an accruedInterest slot
	# accruedInterestPositions: a list of AyrtonPositions of type accruedInterest

	isMatchedAccruedInterest <- sapply(accruedInterestPositions,matchPositionToAyrtonPosition,positionWithAcc)
	if (!any(isMatchedAccruedInterest)) {
		message <- "Error from completePositionWithAccruedInterest.\n The bond position:\n"
		message <- paste(message,as.character(positionWithAcc),"\n",sep="")
		message <- paste(message,"has no matching accruedInterest.")
		stop(message)
	}
	matchedAccruedInterest <- accruedInterestPositions[isMatchedAccruedInterest]
	# compute the value of the accruedInterest
	amount <- matchedAccruedInterest[[1]]@ValoreMercatoMonetaCHF
	value <- toMoney(amount,"CHF")
	value <- repositories$exchangeRates$exchange(value,positionWithAcc@security@currency)
	accruedInterest <- new("AccruedInterest",value)
	
	# compute the dirty value of the bond
	dirtyValue <- positionWithAcc@value + accruedInterest
	positionWithAcc@value <- dirtyValue
	positionWithAcc@accruedInterest <- accruedInterest

	return(positionWithAcc)
}


