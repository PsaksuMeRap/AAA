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
	
	configOptions <- names(parserResult$configLines)
	
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

	checkEquals(parserResult$checkStrings,c(testString1,testString2))
}

test.shouldImportAndRunRiskmanTestSuite <- function() {
	checkEquals(TRUE,TRUE)
}

test.shouldTestSingleRiskmanCheckFile <- function() {
	source("./unitTests/utilities/createunitTestsData.R")
	
	fileName <- "check.RunTestSuite1.txt"
	inputDir <- "./unitTests/data/testSuites/test1"
	outputDir <- "./unitTests/data/testSuites/tmp"

	# parsa il file
	parser <- create_parserTestSuite()
	parser$importFile(paste(inputDir,"/",fileName,sep=""))
	dati.df <- createPositionsData()
	clienti <- unique(dati.df[,"Cliente"])
	portfParser <- create_parserPortfolio()
    portafogli <- lapply(clienti,portfParser$parse,dati.df)
}

test.shouldRunRiskmanTestSuite <- function() {
	name = "Test di una testSuite"
	dirs = c("./unitTests/data/testSuites/test1",
			"./unitTests/data/testSuites/test2"
	)
	bacepTestSuite <- create_riskmanTestSuite(name=name,dirs=dirs)
	
	for (dir in bacepTestSuite$dirs) {
		fileList <- list.files(path=dir, 
			pattern=bacepTestSuite$testFileRegexp)

		results <- unlist(lapply(fileList,importAndRunRiskmanTestSuite))
	}
	checkEquals(results,TRUE)
}
