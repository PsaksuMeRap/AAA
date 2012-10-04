# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldFailToApplyGroupByDirective <- function() {
	replaceDirective <- new("GroupByDirective","aiuto")
	positions <- new("Positions")

	checkException(Apply(replaceDirective,positions))
}


test.shouldApplyGroupByDirectiveEmpty <- function() {
	replaceDirective <- new("GroupByDirective","securityId")
	positions <- new("Positions")

	checkEquals(Apply(replaceDirective,positions),new("Positions"))
	
}

test.shouldApplyGroupByDirective <- function() {
	
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	## check with opzioni_su_azioni 
	## create the positions
	positions <- new("Positions",list(repository$Opzioni_su_divise2,repository$Opzioni_su_azioni1,
			repository$Opzioni_su_azioni2,repository$Opzioni_su_azioni1))
	
	directive <- new("GroupByDirective","securityId")
	
	result <- Apply(directive,positions)
	
	checkEquals(length(result),3)
	checkEquals(result[[2]],repository$Opzioni_su_divise2)
	checkEquals(result[[1]]@value,2*repository$Opzioni_su_azioni1@value)
	name <- "-200 / Call / Syngenta AG / 17-02-12 / Premio(11000 CHF) / CH0011027469 / 337.9 / 10"
	checkEquals(result[[1]]@security@name,name)
	checkEquals(result[[3]],repository$Opzioni_su_azioni2)
	
	## check with opzioni_su_valute
	## create the positions
	positions <- new("Positions",list(repository$Opzioni_su_divise2,repository$Opzioni_su_azioni1,
					repository$Opzioni_su_azioni2,repository$Opzioni_su_divise2))
	
	directive <- new("GroupByDirective","securityId")
	
	result <- Apply(directive,positions)
	
	checkEquals(length(result),3)
	checkEquals(result[[2]],repository$Opzioni_su_divise2)
	checkEquals(result[[1]]@value,2*repository$Opzioni_su_azioni1@value)
	name <- "-200 / Call / Syngenta AG / 17-02-12 / Premio(11000 CHF) / CH0011027469 / 337.9 / 10"
	checkEquals(result[[1]]@security@name,name)
	checkEquals(result[[3]],repository$Opzioni_su_azioni2)
}

