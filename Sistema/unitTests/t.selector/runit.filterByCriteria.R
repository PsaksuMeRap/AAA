# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldApplyLogicalAnd <- function() {
	
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$equity1
	p2 <- repo$bond1
	p3 <- repo$strutturati_FI
	p4 <- repo$equity2
	p5 <- repo$fondiObbligazionari
	positions <- new("Positions",list(p1,p2,p3,p4,p5))
	
	
	# create the selectionCriteria	
	factorString1 <- new("FactorString","security:Bond,Equity")
	parsedFactorString1 <- split(factorString1)	
	selectionCriterium1 <- selectionCriteriumFactory(parsedFactorString1)
	
	factorString2 <- new("FactorString","currency:CHF")
	parsedFactorString2 <- split(factorString2)
	selectionCriterium2 <- selectionCriteriumFactory(parsedFactorString2)	
	selectionCriteria <- new("SelectionCriteria",list(selectionCriterium1,selectionCriterium2))
	
	result <- filterByCriteriaLogicalAnd(selectionCriteria,positions)
	should <- c(TRUE,FALSE,FALSE,FALSE,FALSE)
	checkEquals(result,should)
	
	# create the selectionCriteria	
	factorString1 <- new("FactorString","security:Bond,Equity")
	parsedFactorString1 <- split(factorString1)	
	selectionCriterium1 <- selectionCriteriumFactory(parsedFactorString1)
	
	factorString2 <- new("FactorString","currency:CHF,EUR")
	parsedFactorString2 <- split(factorString2)
	selectionCriterium2 <- selectionCriteriumFactory(parsedFactorString2)	
	selectionCriteria <- new("SelectionCriteria",list(selectionCriterium1,selectionCriterium2))
	
	result <- filterByCriteriaLogicalAnd(selectionCriteria,positions)
	should <- c(TRUE,TRUE,FALSE,TRUE,FALSE)
	checkEquals(result,should)
	
	# check that returns an empty logical when applying to empty positions
	positions <- new("Positions")
	result <- filterByCriteriaLogicalAnd(selectionCriteria,positions)
	checkEquals(result,vector(mode="logical"))
	
	# reset the repository in the original state
	if (!is.null(repository)) repositories$exchangeRates <- repository
	
}

test.shouldApplyLogicalOr <- function() {
	
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$equity1
	p2 <- repo$bond1
	p3 <- repo$strutturati_FI
	p4 <- repo$equity2
	p5 <- repo$fondiObbligazionari
	positions <- new("Positions",list(p1,p2,p3,p4,p5))
	
	
	# create the selectionCriteria
	#selectionCriteria1
	factorString1 <- new("FactorString","security:Bond,Equity")
	parsedFactorString1 <- split(factorString1)	
	selectionCriterium1 <- selectionCriteriumFactory(parsedFactorString1)
	
	factorString2 <- new("FactorString","currency:CHF")
	parsedFactorString2 <- split(factorString2)
	selectionCriterium2 <- selectionCriteriumFactory(parsedFactorString2)	
	selectionCriteria1 <- new("SelectionCriteria",list(selectionCriterium1,selectionCriterium2))
	
	#selectionCriteria2
	factorString1 <- new("FactorString","currency:EUR")
	parsedFactorString1 <- split(factorString1)	
	selectionCriterium1 <- selectionCriteriumFactory(parsedFactorString1)
	
	factorString2 <- new("FactorString","amount:<79EUR")
	parsedFactorString2 <- split(factorString2)
	selectionCriterium2 <- selectionCriteriumFactory(parsedFactorString2)	
	selectionCriteria2 <- new("SelectionCriteria",list(selectionCriterium1,selectionCriterium2))	
	
	listOfSelectionCriteria <- list(selectionCriteria1,selectionCriteria2)
	
	#test 1
	result <- filterByCriteriaLogicalOr(listOfSelectionCriteria,positions)
	should <- c(TRUE,FALSE,FALSE,FALSE,FALSE)
	checkEquals(result,should)
	
	
	#test 2: change the second selectionCriterium2 from < to >
	#selectionCriteria2
	factorString1 <- new("FactorString","currency:EUR")
	parsedFactorString1 <- split(factorString1)	
	selectionCriterium1 <- selectionCriteriumFactory(parsedFactorString1)
	
	factorString2 <- new("FactorString","amount:>79EUR")
	parsedFactorString2 <- split(factorString2)
	selectionCriterium2 <- selectionCriteriumFactory(parsedFactorString2)	
	selectionCriteria2 <- new("SelectionCriteria",list(selectionCriterium1,selectionCriterium2))	
	
	listOfSelectionCriteria <- list(selectionCriteria1,selectionCriteria2)
	result <- filterByCriteriaLogicalOr(listOfSelectionCriteria,positions)
	should <- rep(TRUE,5)
	checkEquals(result,should)
	
	# check that returns an empty logical when applying to empty positions
	positions <- new("Positions")
	result <- filterByCriteriaLogicalOr(listOfSelectionCriteria,positions)
	checkEquals(result,vector(mode="logical"))
	
	# reset the repository in the original state
	if (!is.null(repository)) repositories$exchangeRates <- repository
	
}
