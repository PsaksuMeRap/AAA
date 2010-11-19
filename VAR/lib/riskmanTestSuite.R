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

testSingleRiskmanCheckFile <- function(fileName,inputDir,po) {
	
	# parsa il file della testSuite con i dati sui check
	parserTestSuite <- create_parserTestSuite()
	parserTestSuite$importFile(paste(inputDir,"/",fileName,sep=""))
	testSuiteData <- parserTestSuite$parseStrings()
	
	# crea output infos
	if (testSuiteData$configLines[["testSuiteKind"]]=="specific") { 
		owner <- testSuiteData$configLines[["testTarget"]]
		outputFileName <- paste(owner,"_",Sys.Date(),".log",sep="")
	} else {
		stop("Error: manca owner o aggiorna codice")
	}
	
	outputDir <- testSuiteData$configLines[["outputDir"]]
	logFile <- paste(outputDir,outputFileName,sep="/")
	
	# se po è di classe positions parsa po altrimenti estrai
	# l'owner e segnala un errore
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
	cat(paste("Portfolio:",owner),file=logFile,sep="\n",append=TRUE)
	cat(paste("Input file:",fileName),file=logFile,sep="\n",append=TRUE)
	cat(paste("Inp. directory:",inputDir),file=logFile,sep="\n",append=TRUE)
	cat(paste("Out. directory:",outputDir),file=logFile,sep="\n",append=TRUE)
	cat("\n",file=logFile,sep="\n",append=TRUE)
	
	checkResults <- sapply(testSuiteData$checkStrings,checkCheckStringOnPositions,positions,logFile)
	summary <- paste(checkResults,": ", names(checkResults),sep="",collapse="\n")
	cat("Summary:",file=logFile,sep="\n",append=TRUE)
	cat(summary,file=logFile,sep="\n",append=TRUE)
	close(con)
	
	# stampa il portafoglio in un file
	owner <- testSuiteData$configLines[["testTarget"]]
	outputFileName <- paste("Portafoglio_",owner,"_",Sys.Date(),".log",sep="")
	outputDir <- testSuiteData$configLines[["outputDir"]]
	logFile <- paste(outputDir,outputFileName,sep="/")

	
	con <- file(description=logFile,open="w")
	cat("\n",file=logFile,sep="\n",append=TRUE)
	cat("Portfolio structure:",file=logFile,sep="\n",append=TRUE)
	portfolioStrings <- paste(positions$toString(),collapse="\n")
	cat(portfolioStrings,file=logFile,sep="\n",append=TRUE)
	close(con)

	names(checkResults) <- NULL
	return(checkResults)
}


importAndRunRiskmanTestSuite <- function(testSuite,portfolios) {
	runDirectory <- function(dir,portfolios) {
		fileList <- list.files(path=dir, 
				pattern=testSuite$testFileRegexp)
		
		result <- lapply(fileList,testSingleRiskmanCheckFile,
				inputDir=dir,po=portfolios)
		names(result) <- fileList
		return(result)
	}
	
	results <- lapply(testSuite$dirs,runDirectory,portfolios)

}