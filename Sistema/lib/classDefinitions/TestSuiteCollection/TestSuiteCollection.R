# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("TestSuiteCollection",
		representation(
				name="character",
				directories="character",
				testFileRegexp="character",
				testSuites="list"
		)
)

setMethod("initialize", "TestSuiteCollection",
		function(.Object, name, directories, testFileRegexp="^check.+\\.txt$",fileList) {
		
			if (missing(directories)) {
				stop("argument 'dirs' is missing without a default.")
			}
			.Object@directories <- directories
			
			if (missing(name)) {
				warning("argument 'name' is missing. using basename(dirs)[1] instead.")
				.Object@name <- basename(directories)[1]
			} else {
				.Object@name <- name
			}
			
			.Object@testFileRegexp <- testFileRegexp
			
			tmp <- function(fileName,path) {
				testSuite <- new("TestSuite",path=path,fileName=fileName)
				return(testSuite)
			}
			if (missing(fileList)) {
				fileList <- character(0)
				testSuites <- list()
				for (path in directories) {
					tmpFileList <- list.files(path=path, 
							pattern=testFileRegexp)				
					fileList <- c(fileList,tmpFileList)
					testSuites <- c(testSuites,lapply(fileList,tmp,path=path))
				}
			} else {
				testSuites <- lapply(fileList,tmp,path=directories[[1]])
			}
			
			.Object@testSuites <- testSuites
			return(.Object)
		}
)