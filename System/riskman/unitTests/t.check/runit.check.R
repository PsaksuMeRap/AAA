# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldCheckPositionByAmount <- function() {
	
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	indexCertificate <- repo$indexCertificate
	
	# the indexCertificate has currency "USD" and a value in CHF of 283354.88
	
	# check >
	factorString <- new("FactorString","amount:>100.8623CHF")
	factorStringParsed <- split(factorString)
	selectionCriterium <- selectionCriteriumFactory(factorStringParsed)
	
	result <- check(indexCertificate,selectionCriterium)
	checkEquals(result,TRUE)
	
	
	# check <
	factorString <- new("FactorString","amount:<100.8623CHF")
	factorStringParsed <- split(factorString)
	selectionCriterium <- selectionCriteriumFactory(factorStringParsed)
	
	result <- check(indexCertificate,selectionCriterium)
	checkEquals(result,FALSE)
	
	# check =
	factorString <- new("FactorString","amount:=294333.520307469  USD")
	factorStringParsed <- split(factorString)
	selectionCriterium <- selectionCriteriumFactory(factorStringParsed)
	result <- check(indexCertificate,selectionCriterium)
	checkEquals(result,TRUE)

	
	# check >=
	factorString <- new("FactorString","amount:>=300000.8623CHF")
	factorStringParsed <- split(factorString)
	selectionCriterium <- selectionCriteriumFactory(factorStringParsed)
	result <- check(indexCertificate,selectionCriterium)
	checkEquals(result,FALSE)
	
	# check <=
	factorString <- new("FactorString","amount:<=283354.88 CHF")
	factorStringParsed <- split(factorString)
	selectionCriterium <- selectionCriteriumFactory(factorStringParsed)
	result <- check(indexCertificate,selectionCriterium)
	checkEquals(result,TRUE)
	
	# check !=
	factorString <- new("FactorString","amount:!=283354.89 CHF")
	factorStringParsed <- split(factorString)
	selectionCriterium <- selectionCriteriumFactory(factorStringParsed)
	result <- check(indexCertificate,selectionCriterium)
	checkEquals(result,TRUE)
	
	# reset the repository in the original state
	if (!is.null(repository)) repositories$exchangeRates <- repository
	
}

test.shouldCheckPositionBySecurity <- function() {
	# initialize the position
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	equity <- repo$equity1
	
	# check 1: should recognize equity
	factorString <- new("FactorString","security:Bond,Equity")
	factorStringParsed <- split(factorString)
	selectionCriterium <- selectionCriteriumFactory(factorStringParsed)
	result <- check(equity,selectionCriterium)
	checkEquals(result,TRUE)
	
	# check 2: should not recognize equity
	factorString <- new("FactorString","security:Option on equity, Bond  ")
	factorStringParsed <- split(factorString)
	selectionCriterium <- selectionCriteriumFactory(factorStringParsed)
	result <- check(equity,selectionCriterium)	
	checkEquals(result,FALSE)	
	
}

test.shouldCheckPositionByCurrency <- function() {
	# initialize the position
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	security <- repo$indexCertificate
	
	# check 1: should recognize equity
	factorString <- new("FactorString","currency:EUR,CHF")
	factorStringParsed <- split(factorString)
	selectionCriterium <- selectionCriteriumFactory(factorStringParsed)
	result <- check(security,selectionCriterium)
	checkEquals(result,FALSE)
	
	# check 2: should not recognize equity
	factorString <- new("FactorString","currency:EUR,CHF, USD")
	factorStringParsed <- split(factorString)
	selectionCriterium <- selectionCriteriumFactory(factorStringParsed)
	result <- check(security,selectionCriterium)	
	checkEquals(result,TRUE)	
}
