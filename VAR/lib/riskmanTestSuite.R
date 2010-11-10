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

testSingleRiskmanCheckFile <- function(fileName,inputDir,outputDir,po) {
	
	# parsa il file della testSuite1 con i dati sui check
	parserTestSuite <- create_parserTestSuite()
	parserTestSuite$importFile(paste(inputDir,"/",fileName,sep=""))
	testSuiteData <- parserTestSuite$parseStrings()
	
	# crea output infos
	if (testSuiteData$configLines[["testSuiteKind"]]=="specific") owner <- testSuiteData$configLines[["testTarget"]] else stop("Error: manca owner o aggiorna codice")
	outputFileName <- paste(owner,"_",Sys.Date(),".log",sep="")
	logFile <- paste(outputDir,outputFileName,sep="/")
	
	if (is.element("positions",class(po))) {
		positions <- po
	} else {
		portfolio <- filterLists(po,"owner",owner)
		if (length(portfolio)==0) {
			con <- file(description=logFile,open="w")
			cat(paste("Error: porfolio for owner",owner,"is missing."),file=logFile,sep="\n",append=TRUE)
			close(con)
			return(FALSE)
		}  
		positions <- portfolio[[1]]$positions
	}		

	con <- file(description=logFile,open="w")
	checkResults <- sapply(testSuiteData$checkStrings,checkCheckStringOnPositions,positions,logFile)
	summary <- paste(checkResults,": ", names(checkResults),sep="",collapse="\n")
	cat(paste("Summary:"),file=logFile,sep="\n",append=TRUE)
	cat(summary,file=logFile,sep="\n",append=TRUE)
	close(con)
	names(checkResults) <- NULL
	return(checkResults)
}

importAndRunRiskmanTestSuite <- function(x) return (TRUE)