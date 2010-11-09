# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_riskmanTestSuite <- function(name,dirs,testFileRegexp = "^check.+\\.txt$") {
	riskmanTestSuite <- new.env()
	class(riskmanTestSuite) <- "riskmanTestSuite"

	if (missing(dirs)) {
		stop("argument 'dirs' is missing without a default.")
	}
	if (missing(name)) {
		warning("argument 'name' is missing. using basename(dirs)[1] instead.")
		name <- basename(dirs)[1]
	}
	
	riskmanTestSuite$name <- name
	riskmanTestSuite$dirs <- dirs
	riskmanTestSuite$testFileRegexp <- testFileRegexp

	return(invisible(riskmanTestSuite))
}

create_riskmanTest <- function(fileName,dirs) {
	
}

testSingleRiskmanCheckFile <- function(positions,fileName,inputDir,outputDir) {
	
}

importAndRunRiskmanTestSuite <- function(x) return (TRUE)