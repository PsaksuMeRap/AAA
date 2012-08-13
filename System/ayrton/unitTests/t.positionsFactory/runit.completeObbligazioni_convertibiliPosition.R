# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldcompletePositionConvertibleBondWithAccruedInterest <- function() {
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	source("./base/unitTests/utilities/createRepositoryPositions.R")	
	
	# create the origins
	repository1 <- createRepositoryAyrtonPositions()
	repository2 <- createRepositoryPositions()
	
	convertibleBond <- repository2$Obbligazioni_convertibili
	
	accruedInterestAyrton <- list()
	accruedInterestAyrton[[1]] <- repository1$proRata1 # è quello giusto
	accruedInterestAyrton[[2]] <- repository1$proRataFondiObbligazionari
	accruedInterestAyrton[[3]] <- repository1$proRataObbligazioni_convertibili
	
	accruedInterestAyrton <- new("AyrtonPositions",accruedInterestAyrton)
	
	result <- completePositionWithAccruedInterest(convertibleBond,accruedInterestAyrton)
	money <- toMoney(accruedInterestAyrton[[3]]@ValoreMercatoMonetaCHF,"CHF")
	
	convertibleBond@accruedInterest <- new("AccruedInterest",money)
	convertibleBond@value <- convertibleBond@value + convertibleBond@accruedInterest
	checkEquals(identical(result,convertibleBond),TRUE)
	
}
