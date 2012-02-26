# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldcreateRiskmanTestSuite <- function() {
	name = "Test di una testSuite"
	dirs = c("./unitTests/data")
	bacepTestSuite <- create_riskmanTestSuite(name=name,dirs=dirs)
	
	checkEquals(class(bacepTestSuite),"riskmanTestSuite")
	checkEquals(bacepTestSuite$name,"Test di una testSuite")
	checkEquals(bacepTestSuite$dirs,c("./unitTests/data"))
}

test.importTestSuite <- function() {
	
	fileName="./unitTests/data/testImportTestSuite2.txt"
	parser <- create_parserTestSuite()
	parser$importFile(fileName)
	parserResult <- parser$parseStrings()
	
	configOptions <- names(parserResult@configLines)
	
	checkEquals(class(parserResult),"Testsuite")
	checkEquals(is.element("testSuiteName",configOptions),TRUE)
	checkEquals(is.element("creationDate",configOptions),TRUE)
	checkEquals(is.element("creator",configOptions),TRUE)
	checkEquals(is.element("testSuiteKind",configOptions),TRUE)
	checkEquals(is.element("testTarget",configOptions),TRUE)
	checkEquals(is.element("reportTo",configOptions),TRUE)
	checkEquals(is.element("outputFile",configOptions),TRUE)
	checkEquals(is.element("externalSuiteFileName",configOptions),TRUE)
	
	testString1 <- "instrument:bond & currency:CHF ; > 5%"  
	testString2 <- "currency:USD & instrument:equity ; < 10%"

	checkEquals(parserResult@checkStrings,c(testString1,testString2))
}


test.shouldTestSingleRiskmanCheckFile11 <- function() {
	# l'input della procedura "testSingleRiskmanCheckFile" è
	# in questo primo test una lista di portfoli
	source("./unitTests/utilities/allocateTestRepositories.R")
	source("./unitTests/utilities/createOriginData.R")
	
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	allocateTestRepositories("politicaInvestimento")
	
	fileName <- "check.RunTestSuite1.txt"
	inputDir <- "./unitTests/data/testSuites/test1"
	
	# crea il portafoglio da parsare
	origin <- new("AyrtonPositions",createOriginData())
	
	# create the instrument repository 
	source("./unitTests/utilities/allocateTestRepositories.R")  
	allocateTestRepositories("instruments")
	allocateTestRepositories("politicaInvestimento")
	politicaInvestimento.df <- repositories$politicaInvestimento$politicaInvestimento.df
	
	# extract the portfolios
	portfolios <- portfoliosFactory(origin,politicaInvestimento.df)

	checkResults <- testSingleRiskmanCheckFile(fileName,inputDir,po=portfolios)
	
	checkEquals(checkResults,c(FALSE,TRUE,TRUE))

	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("exchangeRates")	
	deallocateTestRepositories("politicaInvestimento")

}

test.shouldTestSingleRiskmanCheckFileAggregated <- function() {
	# l'input della procedura "testSingleRiskmanCheckFile" è
	# in questo primo test una lista di portfoli di cui vengono
	# considerati ed aggregati solo 2
	source("./unitTests/utilities/allocateTestRepositories.R")
	source("./unitTests/utilities/createOriginData.R")
	
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	allocateTestRepositories("politicaInvestimento")
	
	fileName <- "check.RunTestSuite1.txt"
	inputDir <- "./unitTests/data/testSuites/test3"
	
	# crea il portafoglio da parsare
	origin <- createOriginData()
	owners <- unique(extractFromList(origin,"Cliente"))
	portfParser <- create_parserPortfolio()
	portfolios <- lapply(owners,portfParser$parse,
			origin,repositories$politicaInvestimento$politicaInvestimento.df)
		
	checkResults <- testSingleRiskmanCheckFile(fileName,inputDir,po=portfolios)
	checkEquals(checkResults,c(FALSE,TRUE,TRUE))
	
	fileName <- "check.RunTestSuite2.txt"
	checkResults <- testSingleRiskmanCheckFile(fileName,inputDir,po=portfolios)
	checkEquals(checkResults,c(FALSE,FALSE,FALSE))	
	
	fileName <- "check.RunTestSuite3.txt"
	checkResults <- testSingleRiskmanCheckFile(fileName,inputDir,po=portfolios)
	
	checkEquals(checkResults,c(FALSE,TRUE,TRUE))
	
	
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("exchangeRates")	
	deallocateTestRepositories("politicaInvestimento")
	
}



test.shouldTestSingleRiskmanCheckFile12 <- function() {
	# l'input della procedura "testSingleRiskmanCheckFile" è
	# in questo secondo test un oggetto di classe positions
	source("./unitTests/utilities/allocateTestRepositories.R")
	source("./unitTests/utilities/createOriginData.R")
	
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	allocateTestRepositories("politicaInvestimento")
	
	fileName <- "check.RunTestSuite1.txt"
	inputDir <- "./unitTests/data/testSuites/test1"
	outputDir <- "./unitTests/data/testSuites/test1"
	
	# crea il portafoglio da parsare
	origin <- createOriginData()
	owners <- unique(extractFromList(origin,"Cliente"))
	portfParser <- create_parserPortfolio()
	portfolios <- lapply(owners,portfParser$parse,
			origin,repositories$politicaInvestimento$politicaInvestimento.df)
	owner <- "pippo53"
	portfolio <- filterLists(portfolios,"owner",owner)[[1]]; rm(portfolios)
	positions <- portfolio$positions
	checkResults <- testSingleRiskmanCheckFile(fileName,inputDir,po=positions)
	
	checkEquals(checkResults,c(FALSE,TRUE,TRUE))
	
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("exchangeRates")	
	deallocateTestRepositories("politicaInvestimento")
	
}



test.shouldRunRiskmanTestSuite <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	source("./unitTests/utilities/createOriginData.R")
	
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	allocateTestRepositories("politicaInvestimento")
	
	# inizializza la testSuite
	name = "Test di una testSuite"
	dirs = c("./unitTests/data/testSuites/test1",
			 "./unitTests/data/testSuites/test2"
	)
	bacepTestSuite <- create_riskmanTestSuite(name=name,dirs=dirs)
	
	# crea il portafoglio da parsare
	origin <- createOriginData()
	owners <- unique(extractFromList(origin,"Cliente"))
	portfParser <- create_parserPortfolio()
	portfolios <- lapply(owners,portfParser$parse,
			origin,repositories$politicaInvestimento$politicaInvestimento.df)
	
	# run the testSuite
	results <- importAndRunRiskmanTestSuite(bacepTestSuite,portfolios)
	names(results) <- bacepTestSuite$dirs
	
	# compare the results
	should <- c(FALSE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,TRUE)

 	checkEquals(unlist(results,use.names = FALSE),should)
	
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("exchangeRates")	
	deallocateTestRepositories("politicaInvestimento")
}
