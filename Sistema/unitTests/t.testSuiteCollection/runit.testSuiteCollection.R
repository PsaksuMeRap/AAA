# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldInitializeTestSuiteCollection <- function() {
	name="Test_pippo"
	directories=c("./unitTests/data/testSuites/test1",
			"./unitTests/data/testSuites/test2",
			"./unitTests/data/testSuites/test3"
	)
	testSuiteCollection <- new("TestSuiteCollection",name,directories)

	checkEquals(testSuiteCollection@name,"Test_pippo")
	checkEquals(testSuiteCollection@directories,directories)
	checkEquals(testSuiteCollection@testFileRegexp,"^check.+\\.txt$")
	checkEquals(testSuiteCollection@testSuites[[1]]@fileName,"check.RunTestSuite1.txt")
	
	checkEquals(testSuiteCollection@testSuites[[2]]@fileName,"check.RunTestSuite1.txt")
	checkEquals(testSuiteCollection@testSuites[[2]]@path,"./unitTests/data/testSuites/test2")
	checkEquals(testSuiteCollection@testSuites[[3]]@path,"./unitTests/data/testSuites/test2")
	checkEquals(testSuiteCollection@testSuites[[5]]@path,"./unitTests/data/testSuites/test3")

}
