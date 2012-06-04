# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateTestSuiteCollection <- function() {
	
	# Test1: test al files in the given directories 
	testSuiteName <- "Test_pippo"
	directories <- c("./riskman/unitTests/data/testSuites/test1",
			"./riskman/unitTests/data/testSuites/test2",
			"./riskman/unitTests/data/testSuites/test3"
	)
	testSuiteCollection <- testSuiteFactory(testSuiteName,directories)
	
	checkEquals(testSuiteCollection@name,"Test_pippo")
	checkEquals(testSuiteCollection@directories,directories)
	checkEquals(testSuiteCollection@testFileRegexp,"^check.+\\.txt$")
	checkEquals(testSuiteCollection@testSuitesParsed[[1]]@fileName,"check.runTestSuite1.txt")
	
	checkEquals(testSuiteCollection@testSuitesParsed[[2]]@fileName,"check.runTestSuite1.txt")
	checkEquals(testSuiteCollection@testSuitesParsed[[2]]@directory,"./riskman/unitTests/data/testSuites/test2")
	checkEquals(testSuiteCollection@testSuitesParsed[[3]]@directory,"./riskman/unitTests/data/testSuites/test2")
	checkEquals(testSuiteCollection@testSuitesParsed[[5]]@directory,"./riskman/unitTests/data/testSuites/test3")
	checkEquals(testSuiteCollection@testSuitesParsed[[5]]@fileName,"check.runTestSuite1.txt")
	checkEquals(unclass(testSuiteCollection@testSuitesParsed[[5]]@checkStrings[[1]]),"security:bond & currency:CHF ; > 5%")
	checkEquals(unclass(testSuiteCollection@testSuitesParsed[[5]]@checkStrings[[2]]),"currency:USD & security:equity ; < 10%")
	
	# Test2: test a specific file in the given directory 
	testSuiteName <- "Test_pippo"
	directories <- "./riskman/unitTests/data/"
	fileName <- "testImportTestSuite1.txt"
	testSuiteCollection <- testSuiteFactory(testSuiteName,directories,fileName)
	
	checkEquals(testSuiteCollection@name,"Test_pippo")
	checkEquals(testSuiteCollection@directories,directories)
	checkEquals(testSuiteCollection@testFileRegexp,"^check.+\\.txt$")
	checkEquals(testSuiteCollection@testSuitesParsed[[1]]@fileName,fileName)
	checkEquals(unclass(testSuiteCollection@testSuitesParsed[[1]]@checkStrings[[1]]),"security:bond & currency:CHF ; > 5%")
	
}

test.shouldFailToCreateTestSuiteCollection <- function() {
	
	# Test 
	testSuiteName <- "Test_pippo"
	
	checkException(testSuiteFactory(testSuiteName))
	
}