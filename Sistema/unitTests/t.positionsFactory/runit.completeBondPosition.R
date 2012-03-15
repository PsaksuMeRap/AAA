# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCompleteBondPosition <- function() {
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	source("./unitTests/utilities/createRepositoryPositions.R")	
	
	# create the origins
	repository1 <- createRepositoryAyrtonPositions()
	repository2 <- createRepositoryPositions()
	
	bond <- repository2$bond4
	accruedInterestAyrton <- list()
	accruedInterestAyrton[[1]] <- repository1$proRata1 # è quello giusto
	accruedInterestAyrton[[2]] <- repository1$proRataFondiObbligazionari
	accruedInterestAyrton <- new("AyrtonPositions",accruedInterestAyrton)
	
	result <- completeBondPosition(bond,accruedInterestAyrton)
	money <- toMoney(accruedInterestAyrton[[1]]@ValoreMercatoMonetaCHF,"CHF")
	money <- repositories$exchangeRates$exchange(money,bond@security@currency)
	
	bond@accruedInterest <- new("AccruedInterest",money)
	bond@value <- bond@value + bond@accruedInterest
	checkEquals(identical(result,bond),TRUE)
	
}
