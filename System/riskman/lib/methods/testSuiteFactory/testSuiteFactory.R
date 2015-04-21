# TODO: Add comment
# 
# Author: claudio
###############################################################################


testSuiteFactory <- function(testSuiteName,directories,fileName,testFileRegexp="^check\\..+\\.txt$") {

	if (missing(directories)) {
		stop("testSuiteFactory: argument 'directories' is missing without a default.")
	}
	
	if (missing(testSuiteName)) {
		warning("argument 'name' is missing. using basename(dirs)[1] instead.")
		testSuiteName <- basename(directories)[1]
	}
	
	if (missing(fileName)) {
		# initialize the variable containing the results
		fileNames <- character(0)
		testSuites <- list()
		
		# create a wrapper for the new("TestSuite") method
		newTestSuiteWrapper <- function(fileName,directory) {
			testSuite <- new("TestSuite",directory=directory,fileName=fileName)
			return(testSuite)
		}
		
		for (directory in directories) {
			tmpFileNames <- list.files(path=directory,pattern=testFileRegexp)				
			fileNames <- c(fileNames,tmpFileNames)
			testSuites <- c(testSuites,lapply(fileNames,newTestSuiteWrapper,directory=directory))
		}
		
		testSuitesParsed <- lapply(testSuites,parser)
	} else {
		testSuitesParsed <- list(parser(new("TestSuite",directory=directories[[1]],fileName=fileName)))
	}
	
	testSuiteCollection <- new("TestSuiteCollection",name=testSuiteName,
			directories=directories,testFileRegexp=testFileRegexp,
			testSuitesParsed=testSuitesParsed)
	
	return(testSuiteCollection)
}

