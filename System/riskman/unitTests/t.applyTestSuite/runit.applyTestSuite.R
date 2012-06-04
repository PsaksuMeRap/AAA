# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldImportTestSuiteConfigLines <- function() {
	testSuiteName <- "Test"
	directories <- "./riskman/unitTests/data"
	fileName="testImportTestSuite2.txt"

	testSuiteCollection <- testSuiteFactory(testSuiteName,directories,fileName)
	
	configOptions <- names(testSuiteCollection@testSuitesParsed[[1]]@configLines)
	
	checkEquals(is.element("testSuiteName",configOptions),TRUE)
	checkEquals(is.element("creationDate",configOptions),TRUE)
	checkEquals(is.element("creator",configOptions),TRUE)
	checkEquals(is.element("testSuiteKind",configOptions),TRUE)
	checkEquals(is.element("testTarget",configOptions),TRUE)
	checkEquals(is.element("reportTo",configOptions),TRUE)
	checkEquals(is.element("outputFile",configOptions),TRUE)
	checkEquals(is.element("externalSuiteFileName",configOptions),TRUE)
	
	testString1 <- "security:bond & currency:CHF ; > 5%"  
	testString2 <- "currency:USD & security:equity ; < 10%"

	checkEquals(unclass(testSuiteCollection@testSuitesParsed[[1]]@checkStrings[[1]]),testString1)
	checkEquals(unclass(testSuiteCollection@testSuitesParsed[[1]]@checkStrings[[2]]),testString2)
	
}


test.shouldApplyTestSuite_test1_1 <- function() {

	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# create the positions 
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$equity1 # Equity / CHF / 88'205.00 / Roche Holding Gs
	p2 <- repo$bond1   # Bond / EUR / 92'896.60 / 20130603 - 3.625% Pfizer 03-06-13
	p3 <- repo$equity2 # Equity / EUR / 5'558.12 / Kontron AG

	positions <- new("Positions",list(p1,p2,p3))
	
	testSuiteName <- "test1_1"
	directories <- "./riskman/unitTests/data/testSuites/test1"
	fileName <- "check.runTestSuite1.txt"
	#security:Bond & currency:CHF ; > 5%  
	#currency:USD & security:Equity ; < 10%
	#currency:CHF + security:Equity & currency:EUR ; > 50% 

	testSuiteCollection <- testSuiteFactory(testSuiteName,directories,fileName)
	testSuiteParsed <- testSuiteCollection@testSuitesParsed[[1]]

	#test1: use positions
	result <- applyTestSuite(testSuiteParsed,positions)
	checkEquals(result,c(FALSE,TRUE,FALSE))

	#test2: use portfolio
	portfolio <- new("Portfolio",owner="pippo53",
			referenceCurrency=new("Currency","CHF"),
			positions)
	result <- applyTestSuite(testSuiteParsed,portfolio)
	checkEquals(result,c(FALSE,TRUE,FALSE))
	
	#test3: use portfolios
	p4 <- repo$strutturati_FI
	p5 <- repo$fondiObbligazionari
	positions1 <- new("Positions",list(p4,p5)) 
	portfolio1 <- new("Portfolio",owner="12345kd",
			referenceCurrency=new("Currency","USD"),
			positions1)
	portfolios <- new("Portfolios",list(portfolio1,portfolio))
	result <- applyTestSuite(testSuiteParsed,portfolios)
	checkEquals(result,c(FALSE,TRUE,FALSE))
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
	
}

test.shouldTestSingleRiskmanCheckFileAggregated <- function() {
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# create the positions 
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	# create portfolio 1
	p1 <- repo$equity1 # Equity / CHF / 88'205.00 / Roche Holding Gs
	p2 <- repo$bond1   # Bond / EUR / 92'896.60 / 20130603 - 3.625% Pfizer 03-06-13
	p3 <- repo$equity2 # Equity / EUR / 5'558.12 / Kontron AG
	
	positions <- new("Positions",list(p1,p2,p3))
	portfolio1 <- new("Portfolio",owner="pippo118",
			referenceCurrency=new("Currency","CHF"),
			positions)
	# create portfolio 2
	p4 <- repo$strutturati_FI      # Strutturati_FI / EUR / 133'951.68
	p5 <- repo$fondiObbligazionari # Fondi_obbligazionari / EUR / 7'694.96
	positions <- new("Positions",list(p4,p5)) 
	portfolio2 <- new("Portfolio",owner="pippo120",
			referenceCurrency=new("Currency","USD"),
			positions)
	# create portfolio 3
	positions <- new("Positions",list(p4,p1,p3)) 
	portfolio3 <- new("Portfolio",owner="pippo1",
			referenceCurrency=new("Currency","EUR"),
			positions)
	portfolios <- new("Portfolios",list(portfolio1,portfolio2,portfolio3))
	
	# define the testSuite parameters
	testSuiteName <- "test3"
	directories <- "./riskman/unitTests/data/testSuites/test3"
	fileName <- "check.runTestSuite3.txt"
	testSuiteCollection <- testSuiteFactory(testSuiteName,directories,fileName)
	testSuiteParsed <- testSuiteCollection@testSuitesParsed[[1]]
	# security:Bond & currency:CHF ; > 5%  
	# amount:>63880USD ; > 10%
	# currency:CHF + security:Equity & currency:EUR ; > 50%  
	
	#test
	result <- applyTestSuite(testSuiteParsed,portfolios)
	checkEquals(result,c(FALSE,TRUE,FALSE))
	
	# Equity / CHF / 88'205.00 / Roche Holding Gs
	# Bond / EUR / 92'896.60 / 20130603 - 3.625% Pfizer 03-06-13
	# Equity / EUR / 5'558.12 / Kontron AG
	# Strutturati_FI / EUR / 133'951.68
	# Fondi_obbligazionari / EUR / 7'694.96
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
	
}

test.shouldFailSingleRiskmanCheckFileAggregated <- function() {
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# create the positions 
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	# create portfolio 1
	p1 <- repo$equity1 # Equity / CHF / 88'205.00 / Roche Holding Gs
	p2 <- repo$bond1   # Bond / EUR / 92'896.60 / 20130603 - 3.625% Pfizer 03-06-13
	p3 <- repo$equity2 # Equity / EUR / 5'558.12 / Kontron AG
	
	positions <- new("Positions",list(p1,p2,p3))
	portfolio1 <- new("Portfolio",owner="pippo118",
			referenceCurrency=new("Currency","CHF"),
			positions)
	# create portfolio 2
	p4 <- repo$strutturati_FI      # Strutturati_FI / EUR / 133'951.68
	p5 <- repo$fondiObbligazionari # Fondi_obbligazionari / EUR / 7'694.96
	positions <- new("Positions",list(p4,p5)) 
	portfolio2 <- new("Portfolio",owner="pippo119",
			referenceCurrency=new("Currency","USD"),
			positions)
	# create portfolio 3
	positions <- new("Positions",list(p4,p1,p3)) 
	portfolio3 <- new("Portfolio",owner="pippo1",
			referenceCurrency=new("Currency","EUR"),
			positions)
	portfolios <- new("Portfolios",list(portfolio1,portfolio2,portfolio3))
	
	# define the testSuite parameters
	testSuiteName <- "test3"
	directories <- "./riskman/unitTests/data/testSuites/test3"
	fileName <- "check.runTestSuite3.txt"
	testSuiteCollection <- testSuiteFactory(testSuiteName,directories,fileName)
	testSuiteParsed <- testSuiteCollection@testSuitesParsed[[1]]
	# security:Bond & currency:CHF ; > 5%  
	# amount:>63880USD ; > 10%
	# currency:CHF + security:Equity & currency:EUR ; > 50%  
	
	#test
	checkException(applyTestSuite(testSuiteParsed,portfolios))
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
	
}