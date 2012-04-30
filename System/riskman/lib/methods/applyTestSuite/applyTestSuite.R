# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("applyTestSuite",def=function(x,po,...) standardGeneric("applyTestSuite"))

setMethod("applyTestSuite",signature(x="TestSuiteParsed",po="Portfolios"),
		function(x,po,valuationDate) { 		
			
			if (missing(valuationDate)) valuationDate <- Sys.Date()
			
			owners <- x@configLines[["testTarget"]]
			owners <- removeStartEndSpaces(unlist(strsplit(owners,",")))
			owner <- paste(owners,collapse="_")
			
			availableOwners <- extractFromList(po,"owner")
			available <- is.element(owners,availableOwners)
			
			if (any(!available)) stop(paste("No portfolio/s for owner/s",paste(owners[!available],collapse=" & ")))
			
			# filter the portfolios
			portfolios <- filterLists(po,"owner",owners)
			
			if (length(portfolios)==1) {
				portfolio <- portfolios[[1]]
			} else {
				portfolio <- new("Portfolio",owner=owner,
						referenceCurrency=portfolios[[1]]@referenceCurrency,
						unlist(lapply(portfolios,function(x)return(x@.Data)),recursive=FALSE))
			}
			
			applyTestSuite(x,as(portfolio,"Positions"),valuationDate,portfolio@referenceCurrency)
		}
)

setMethod("applyTestSuite",signature(x="TestSuiteParsed",po="Portfolio"),
		function(x,po,valuationDate) { 		
		
			if (missing(valuationDate)) valuationDate <- Sys.Date()
			
			owners <- x@configLines[["testTarget"]]
			owners <- removeStartEndSpaces(unlist(strsplit(owners,",")))
			owner <- paste(owners,collapse="_")
			
			if (po@owner!=owner) stop(paste("Wrong portfolio for owner",owner))
			applyTestSuite(x,as(po,"Positions"),valuationDate,po@referenceCurrency)
		}
)

setMethod("applyTestSuite",signature(x="TestSuiteParsed",po="Positions"),
		function(x,po,valuationDate,referenceCurrency=new("Currency","CHF")) { 		
	
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
			inputDir  <- x@directory
			logFile <- paste(outputDir,outputFileName,sep=.Platform$file.sep)
	
			logFile <- file(description=logFile,open="w")
			cat(paste("Portfolio:",ownerPrintName),file=logFile,sep="\n",append=TRUE)
			cat(paste("Input file:",x@fileName),file=logFile,sep="\n",append=TRUE)
			cat(paste("Inp. directory:",inputDir),file=logFile,sep="\n",append=TRUE)
			cat(paste("Out. directory:",outputDir),file=logFile,sep="\n",append=TRUE)
			cat("\n",file=logFile,sep="\n",append=TRUE)
			
			# simultaneously apply all the checkStrings on the po
			results <- lapply(x@checkStrings,Apply,po,logFile,referenceCurrency)

			checkResults <- extractFromList(results,"checkResult")
			names(checkResults) <- extractFromList(results,"checkString")
			
			
			results.actualPercentage <- extractFromList(results,"actualPercentage")
			summary <- paste(checkResults,": ", names(checkResults)," (actual ",results.actualPercentage,")",sep="",collapse="\n")
			cat("Summary:",file=logFile,sep="\n",append=TRUE)
			cat(summary,file=logFile,sep="\n",append=TRUE)
			close(logFile)
			
			# print the portfolio to a file
			outputFileName <- paste("Portafoglio_",ownerPrintName,"_",valuationDate,".log",sep="")
			outputDir <- x@configLines[["outputDir"]]
			logFile <- paste(outputDir,outputFileName,sep="/")
			
		
			logFile <- file(description=logFile,open="w")
			cat("\n",file=logFile,sep="\n",append=TRUE)
			cat("Portfolio structure:",file=logFile,sep="\n",append=TRUE)
			portfolioStrings <- paste(as.character(po),collapse="\n")
			cat(portfolioStrings,file=logFile,sep="\n",append=TRUE)
			close(logFile)
			
			# create a warning if a FALSE is detected
			if (!all(checkResults)) {
				outputFileName <- paste("warning_",valuationDate,".log",sep="")
				outputDir <- x@configLines[["outputDir"]]
				logFile <- paste(outputDir,outputFileName,sep="/")
				logFile <- file(description=logFile,open="at")
				cat(ownerPrintName,file=logFile,sep="\n",append=TRUE)
				close(logFile)
			}
			names(checkResults) <- NULL
			return(checkResults)
		}
)
