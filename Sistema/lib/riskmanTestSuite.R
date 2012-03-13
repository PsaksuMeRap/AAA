# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("runTest",def=function(x,...) standardGeneric("test"))

setMethod("runTest",signature(x="TestSuite",positions="Positions"),
		function(x,positions,valuationDate) { 
# nome della funzione originale, poi cancella ...			
#testSingleRiskmanCheckFile <- function(fileName,inputDir,po,valuationDate) {
#}			
			# parsa il file della testSuite con i dati sui check
			parsedTestSuite <- parser(x)
			
			if (missing(valuationDate)) valuationDate <- Sys.Date()
			
			# crea output infos
			if (x@configLines[["testSuiteKind"]]=="specific") { 
				owner <- x@configLines[["testTarget"]]
				owner <- removeStartEndSpaces(unlist(strsplit(owner,",")))
				ownerPrintName <- paste(owner,collapse="_")
				outputFileName <- paste(ownerPrintName,"_",valuationDate,".log",sep="")
			} else {
				stop("Error: missing owner or update your code")
			}
			
			outputDir <- x@configLines[["outputDir"]]
			logFile <- paste(outputDir,outputFileName,sep=.Platform$file.sep)
	
			con <- file(description=logFile,open="w")
			cat(paste("Portfolio:",ownerPrintName),file=logFile,sep="\n",append=TRUE)
			cat(paste("Input file:",fileName),file=logFile,sep="\n",append=TRUE)
			cat(paste("Inp. directory:",inputDir),file=logFile,sep="\n",append=TRUE)
			cat(paste("Out. directory:",outputDir),file=logFile,sep="\n",append=TRUE)
			cat("\n",file=logFile,sep="\n",append=TRUE)
			
			# simultaneously apply all the checkStrings on the positions
			results <- lapply(x@checkStrings,apply,positions,logFile)
			checkResults <- extractFromList(results,"checkResult")
			names(checkResults) <- extractFromList(results,"checkString")
			
			
			results.actualPercentage <- extractFromList(results,"actualPercentage")
			summary <- paste(checkResults,": ", names(checkResults)," (actual ",results.actualPercentage,")",sep="",collapse="\n")
			cat("Summary:",file=logFile,sep="\n",append=TRUE)
			cat(summary,file=logFile,sep="\n",append=TRUE)
			close(con)
			
			# print the portfolio to a file
			outputFileName <- paste("Portafoglio_",ownerPrintName,"_",valuationDate,".log",sep="")
			outputDir <- testSuiteData@configLines[["outputDir"]]
			logFile <- paste(outputDir,outputFileName,sep="/")
			
			
			con <- file(description=logFile,open="w")
			cat("\n",file=logFile,sep="\n",append=TRUE)
			cat("Portfolio structure:",file=logFile,sep="\n",append=TRUE)
			portfolioStrings <- paste(toString(positions),collapse="\n")
			cat(portfolioStrings,file=logFile,sep="\n",append=TRUE)
			close(con)
			
			# create a warning if a FALSE is detected
			if (!all(checkResults)) {
				outputFileName <- paste("warning_",valuationDate,".log",sep="")
				outputDir <- testSuiteData@configLines[["outputDir"]]
				logFile <- paste(outputDir,outputFileName,sep="/")
				con <- file(description=logFile,open="at")
				cat(ownerPrintName,file=logFile,sep="\n",append=TRUE)
				close(con)
			}
			names(checkResults) <- NULL
			return(checkResults)
		}
)



runTestSuiteCollection <- function(testSuiteCollection,portfolios,valuationDate) {

	runDirectory <- function(dir,portfolios,valuationDate) {
		fileList <- list.files(path=dir,pattern=testSuite$testFileRegexp)
		if(missing(valuationDate)) {
			result <- lapply(fileList,testSingleRiskmanCheckFile,
					inputDir=dir,po=portfolios)
		} else {
			result <- lapply(fileList,testSingleRiskmanCheckFile,
					inputDir=dir,po=portfolios,valuationDate=valuationDate)		
		}
		names(result) <- fileList
		return(result)
	}
	
	if (missing(valuationDate)) {
		results <- lapply(testSuite$dirs,runDirectory,portfolios)
	} else {
		results <- lapply(testSuite$dirs,runDirectory,portfolios,valuationDate)	
	}
}
