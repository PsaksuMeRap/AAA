# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldApplyGroupDefinitionToCheckString <- function() {
	groupDefinition <- c("pippo123", "Claudio,Richy,Gianna")
	checkString <- new("CheckString","currency:EUR,pippo123 & instrument:conto_corrente ; > 11%")	
	
	result <- applyGroupDefinition(checkString,groupDefinition)
	should <- new("CheckString","currency:EUR,Claudio,Richy,Gianna & instrument:conto_corrente ; > 11%")
	checkEquals(result,should)
}

test.shouldApplyGroupDefinitionToTestSuiteParsed <- function() {
	fileName <- "testImportTestSuiteWithGroupDefinition.txt"
	directory <- "./riskman/unitTests/data"
	
	testSuite <- new("TestSuite",directory=directory,fileName=fileName)
	
	# check parser 
	parsedTestSuite <- parser(testSuite)

	
	result <- applyGroupDefinition(parsedTestSuite)
	should1 <- new("CheckString","security:bond & currency:CHF & pippo:conto_corrente,bond,mamma   ; > 5%")
	should2 <- new("CheckString","currency:USD & security:equity,USD,EUR,CHF & pippo:conto_corrente,bond,mamma  ; < 10%")
	
	checkEquals(result@checkStrings,list(should1,should2))
	
	
}