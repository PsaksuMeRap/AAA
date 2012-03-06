# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("runTest",def=function(x,...) standardGeneric("test"))

setMethod("runTest",signature(x="TestSuite",positions="Positions"),
		function(x,positions,valuationDate) { 
			
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
				stop("Error: manca owner o aggiorna codice")
			}
			
			outputDir <- x@configLines[["outputDir"]]
			logFile <- paste(outputDir,outputFileName,sep=.Platform$file.sep)
	
			con <- file(description=logFile,open="w")
			cat(paste("Portfolio:",ownerPrintName),file=logFile,sep="\n",append=TRUE)
			cat(paste("Input file:",fileName),file=logFile,sep="\n",append=TRUE)
			cat(paste("Inp. directory:",inputDir),file=logFile,sep="\n",append=TRUE)
			cat(paste("Out. directory:",outputDir),file=logFile,sep="\n",append=TRUE)
			cat("\n",file=logFile,sep="\n",append=TRUE)
			

			results <- lapply(x@checkStrings,checkCheckStringOnPositions,positions,logFile)
			checkResults <- extractFromList(results,"checkResult")
			names(checkResults) <- extractFromList(results,"checkString")
			
			
			results.actualPercentage <- extractFromList(results,"actualPercentage")
			summary <- paste(checkResults,": ", names(checkResults)," (actual ",results.actualPercentage,")",sep="",collapse="\n")
			cat("Summary:",file=logFile,sep="\n",append=TRUE)
			cat(summary,file=logFile,sep="\n",append=TRUE)
			close(con)
			
			# stampa il portafoglio in un file
			outputFileName <- paste("Portafoglio_",ownerPrintName,"_",valuationDate,".log",sep="")
			outputDir <- testSuiteData@configLines[["outputDir"]]
			logFile <- paste(outputDir,outputFileName,sep="/")
			
			
			con <- file(description=logFile,open="w")
			cat("\n",file=logFile,sep="\n",append=TRUE)
			cat("Portfolio structure:",file=logFile,sep="\n",append=TRUE)
			portfolioStrings <- paste(positions$toString(),collapse="\n")
			cat(portfolioStrings,file=logFile,sep="\n",append=TRUE)
			close(con)
			
			# se c'è anche un solo FALSE crea un file di warning
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


testSingleRiskmanCheckFile <- function(fileName,inputDir,po,valuationDate) {
	
	# parsa il file della testSuite con i dati sui check
	parserTestSuite <- create_parserTestSuite()
	parserTestSuite$importFile(paste(inputDir,"/",fileName,sep=""))
	testSuiteData <- parserTestSuite$parseStrings()
	
	if (missing(valuationDate)) valuationDate <- Sys.Date()
	# crea output infos
	if (testSuiteData@configLines[["testSuiteKind"]]=="specific") { 
		owner <- testSuiteData@configLines[["testTarget"]]
		owner <- removeStartEndSpaces(unlist(strsplit(owner,",")))
		ownerPrintName <- paste(owner,collapse="_")
		outputFileName <- paste(ownerPrintName,"_",valuationDate,".log",sep="")
	} else {
		stop("Error: manca owner o aggiorna codice")
	}
	
	outputDir <- testSuiteData@configLines[["outputDir"]]
	logFile <- paste(outputDir,outputFileName,sep="/")
#???	
	# se po è di classe positions parsa po altrimenti estrai
	# l'owner e segnala un errore
	if (is(po,"Positions")) {
		positions <- po
	} else {
		portfolio <- filterClassLists(po,"owner",owner)
		if (length(portfolio)==0) {
			con <- file(description=logFile,open="w")
			cat(paste("Error: porfolio for owner",ownerPrintName,"is missing."),file=logFile,sep="\n",append=TRUE)
			close(con)
			return(FALSE)
		}
		if (length(portfolio)>1) {
			portfolio <- join(portfolio[[1]],portfolio[-1])
		}
		
		positions <- as(portfolio[[1]]@.Data,"Positions")
		refCurrency <- portfolio[[1]]@referenceCurrency
	}		
	
	con <- file(description=logFile,open="w")
	cat(paste("Portfolio:",ownerPrintName),file=logFile,sep="\n",append=TRUE)
	cat(paste("Input file:",fileName),file=logFile,sep="\n",append=TRUE)
	cat(paste("Inp. directory:",inputDir),file=logFile,sep="\n",append=TRUE)
	cat(paste("Out. directory:",outputDir),file=logFile,sep="\n",append=TRUE)
	cat("\n",file=logFile,sep="\n",append=TRUE)
# arrivato qui: devo verificare che la funzione checkCheckStringOnPositions funzioni
	if (is.element("positions",class(po))) {
		results <- lapply(testSuiteData@checkStrings,checkCheckStringOnPositions,positions,logFile)
		checkResults <- extractFromList(results,"checkResult")
		names(checkResults) <- extractFromList(results,"checkString")
	} else {
		results <- lapply(testSuiteData@checkStrings,checkCheckStringOnPositions,positions,logFile,refCurrency)			
		checkResults <- extractFromList(results,"checkResult")
		names(checkResults) <- extractFromList(results,"checkString")
	}
	
	results.actualPercentage <- extractFromList(results,"actualPercentage")
	summary <- paste(checkResults,": ", names(checkResults)," (actual ",results.actualPercentage,")",sep="",collapse="\n")
	cat("Summary:",file=logFile,sep="\n",append=TRUE)
	cat(summary,file=logFile,sep="\n",append=TRUE)
	close(con)
	
	# stampa il portafoglio in un file
	outputFileName <- paste("Portafoglio_",ownerPrintName,"_",valuationDate,".log",sep="")
	outputDir <- testSuiteData@configLines[["outputDir"]]
	logFile <- paste(outputDir,outputFileName,sep="/")
	
	
	con <- file(description=logFile,open="w")
	cat("\n",file=logFile,sep="\n",append=TRUE)
	cat("Portfolio structure:",file=logFile,sep="\n",append=TRUE)
	portfolioStrings <- paste(positions$toString(),collapse="\n")
	cat(portfolioStrings,file=logFile,sep="\n",append=TRUE)
	close(con)
	
	# se c'è anche un solo FALSE crea un file di warning
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
runTestSuiteCollection <- function(testSuiteCollection,portfolios,valuationDate) {

	runDirectory <- function(dir,portfolios,valuationDate) {
		fileList <- list.files(path=dir, 
				pattern=testSuite$testFileRegexp)
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
