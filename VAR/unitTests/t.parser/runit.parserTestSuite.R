# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateParser <- function() {
	fileName <- "./unitTests/data/testImportTestSuite1.txt"
	
	parser <- create_parserTestSuite()
	
	# check parser creation
	checkEquals(class(parser),"parserTestSuite")
	
	# check identifyComment
	checkEquals(parser$identifyComment("#aa"),TRUE)
	
	# check identifyEmptyLine
	checkEquals(parser$identifyEmptyLine("    "),TRUE)
	checkEquals(parser$identifyEmptyLine(""),TRUE)
	checkEquals(parser$identifyEmptyLine(" a"),FALSE)
	
	# check parser importFile
	parser$importFile(fileName)
	checkEquals(parser$strings[1],"testSuiteName: test importazione di una testSuite")
	
	# check lineCheckListStart and lineCheckListEnd
	strings <- c(
			"uno:1",
			"due:chicchirichi",
			"checkListStart:",
			"instrument:equity & currency:USD + instrument:bond ; > 500EUR",
			"checkListEnd:",
			"tre:123"
	)
	
	checkEquals(parser$lineCheckListStart(strings),3)
	checkEquals(parser$lineCheckListEnd(strings),5)
	
	# check parseConfigLine
	configLines <- c("uno:1","due:chicchirichi")

	result <- parser$parseConfigLine("uno:1")
	should1 <- "1"
	names(should1) <- "uno"
	checkEquals(result,should1)		
	
	# check parseStrings
	strings <- c(
			"testSuiteName: test importazione di una testSuite",
			"",
			"# testSuiteKind: general - specific",
			"outputFile:",
			"checkListStart:",
			"instrument:bond & currency:CHF ; > 5%",
			"checkListEnd:",
			"externalSuiteFileName:"			
	)
	result <- parser$parseStrings()
	configLines <- c(
			"testSuiteName: test importazione di una testSuite",
			"externalSuiteFileName:"
	)
	checkStrings <- c("instrument:bond & currency:CHF ; > 5%")
	
	should1 <- "test importazione di una testSuite"
	names(should1) <- "testSuiteName"

	should2 <- NA_character_
	names(should2) <- "externalSuiteFileName"

	configLines <- list(should1,should2)
	
	should <- list(configLines=unlist(configLines),checkStrings=checkStrings)

	checkEquals(result,should)
	
}
