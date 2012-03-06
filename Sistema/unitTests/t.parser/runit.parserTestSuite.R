# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCheckSetParserTestsuiteUtils <- function() {
	
	f <- setParserTestsuiteUtils()
	
	# check identifyComment
	checkEquals(f$identifyComment("#aa"),TRUE)
	
	# check identifyEmptyLine
	checkEquals(f$identifyEmptyLine("    "),TRUE)
	checkEquals(f$identifyEmptyLine(""),TRUE)
	checkEquals(f$identifyEmptyLine(" a"),FALSE)
	
	# check lineCheckListStart and lineCheckListEnd
	strings <- c(
			"uno:1",
			"due:chicchirichi",
			"checkListStart:",
			"instrument:equity & currency:USD + instrument:bond ; > 500EUR",
			"checkListEnd:",
			"tre:123"
	)
	
	checkEquals(f$lineCheckListStart(strings),3)
	checkEquals(f$lineCheckListEnd(strings),5)
	
	# check parseConfigLine
	configLines <- c("uno:1","due:chicchirichi")
	
	result <- f$parseConfigLine("uno:1")
	should1 <- "1"
	names(should1) <- "uno"
	checkEquals(result,should1)
	
}


test.shouldParserTestSuite <- function() {
	fileName <- "testImportTestSuite1.txt"
	path <- "./unitTests/data"
	
	testSuite <- new("TestSuite",path=path,fileName=fileName)
	
	# check parser 
	parsedTestSuite <- parser(testSuite)

	should1 <- "test importazione di una testSuite"
	names(should1) <- "testSuiteName"
	
	should2 <- NA_character_
	names(should2) <- "externalSuiteFileName"
	
	configLines <- c(should1,should2)	
	checkEquals(parsedTestSuite@configLines,configLines)
	
	checkStrings <- c("instrument:bond & currency:CHF ; > 5%")
	checkEquals(parsedTestSuite@checkStrings,checkStrings)
	
}
