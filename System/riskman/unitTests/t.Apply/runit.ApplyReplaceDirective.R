# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldFailToApplyReplaceDirective <- function() {
	replaceDirective <- new("ReplaceDirective","aiuto")
	positions <- new("Positions")

	checkException(Apply(replaceDirective,positions))

}


test.shouldApplyReplaceDirectiveEmpty <- function() {
	replaceDirective <- new("ReplaceDirective","Opzioni_su_azioni")
	positions <- new("Positions")

	checkEquals(Apply(replaceDirective,positions),new("Positions"))
	
}

test.shouldApplyReplaceDirectiveOpzioni_su_azioni <- function() {
	
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	# create the positions
	positions <- list(repository$Opzioni_su_azioni1,repository$Opzioni_su_divise2,
			repository$Opzioni_su_azioni2,repository$Opzioni_su_divise4)
	
	directive <- new("ReplaceDirective",c("Opzioni_su_azioni"))
	
	result <- Apply(directive,positions)
	
	checkEquals(length(result),6)
	checkEquals(is(result[[1]]),c("PositionOpzioni_su_divise","Position"))
	checkEquals(is(result[[2]]),c("PositionOpzioni_su_divise","Position"))
	checkEquals(is(result[[3]]),c("PositionEquity","Position"))	
	checkEquals(is(result[[4]]),c("PositionConto_corrente","Position"))		
	checkEquals(is(result[[5]]),c("PositionEquity","Position"))	
	checkEquals(is(result[[6]]),c("PositionConto_corrente","Position"))			
}

test.shouldApplyReplaceDirectiveOpzioni_su_divise <- function() {
	
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	# create the positions
	positions <- list(repository$Opzioni_su_azioni1,repository$Opzioni_su_divise2,
			repository$Opzioni_su_azioni2,repository$Opzioni_su_divise4)
	
	directive <- new("ReplaceDirective",c("Opzioni_su_divise"))
	
	result <- Apply(directive,positions)
	
	checkEquals(length(result),6)
	checkEquals(is(result[[1]]),c("PositionOpzioni_su_azioni","Position"))
	checkEquals(is(result[[2]]),c("PositionOpzioni_su_azioni","Position"))
	checkEquals(is(result[[3]]),c("PositionConto_corrente","Position"))	
	checkEquals(is(result[[4]]),c("PositionConto_corrente","Position"))		
	checkEquals(is(result[[3]]),c("PositionConto_corrente","Position"))	
	checkEquals(is(result[[4]]),c("PositionConto_corrente","Position"))
	
}

test.shouldApplyReplaceDirectiveOpzioni_su_divise_azioni <- function() {
	
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	# create the positions
	positions <- list(repository$Opzioni_su_azioni1,repository$Opzioni_su_divise2,
			repository$Opzioni_su_azioni2,repository$Opzioni_su_divise4)
	
	directive <- new("ReplaceDirective",c("Opzioni_su_azioni","Opzioni_su_divise"))
	
	result <- Apply(directive,positions)
	
	checkEquals(length(result),8)
	checkEquals(is(result[[1]]),c("PositionEquity","Position"))
	checkEquals(is(result[[2]]),c("PositionConto_corrente","Position"))
	checkEquals(is(result[[3]]),c("PositionEquity","Position"))
	checkEquals(is(result[[4]]),c("PositionConto_corrente","Position"))
	checkEquals(is(result[[5]]),c("PositionConto_corrente","Position"))	
	checkEquals(is(result[[6]]),c("PositionConto_corrente","Position"))		
	checkEquals(is(result[[7]]),c("PositionConto_corrente","Position"))	
	checkEquals(is(result[[8]]),c("PositionConto_corrente","Position"))
	
}