# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./ayrton/lib/methods/positionsFactory/matchPositionToAyrtonPosition.R")

completePositionWithAccruedInterest <- function(positionWithAcc,oaccPositions) {
	# positionWithAcc: a position having an accruedInterest slot
	# oaccPositions: a list of AyrtonPositions of type accruedInterest

	isMatchedAccruedInterest <- sapply(oaccPositions,matchPositionToAyrtonPosition,positionWithAcc)
	if (!any(isMatchedAccruedInterest)) {
		message <- "Error from completePositionWithAccruedInterest.\n The position:\n"
		message <- paste(message,as.character(positionWithAcc),"\n",sep="")
		message <- paste(message,"has no matching accruedInterest.")
		stop(message)
	}
	matchedAccruedInterest <- oaccPositions[isMatchedAccruedInterest]
	# compute the value of the accruedInterest
	amount <- matchedAccruedInterest[[1]]@ValoreMercatoMonetaCHF
	value <- toMoney(amount,"CHF")
	value <- repositories$exchangeRates$exchange(value,positionWithAcc@security@currency)
	accruedInterest <- new("AccruedInterest",value)
	
	# compute the dirty value of the bond / anticipo_fisso / time deposit
	dirtyValue <- positionWithAcc@value + accruedInterest
	positionWithAcc@value <- dirtyValue
	positionWithAcc@accruedInterest <- accruedInterest

	return(positionWithAcc)
}


