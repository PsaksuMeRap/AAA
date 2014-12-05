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
			
			
			if (length(portfolios)==1) { # se si tratta di un unico portafoglio
				portfolio <- portfolios[[1]]
			} else { # altrimenti crea un unico portafoglio
				portfolio <- new("Portfolio",owner=owner,
						referenceCurrency=portfolios[[1]]@referenceCurrency,
						unlist(lapply(portfolios,function(x)return(x@.Data)),recursive=FALSE))
			}
			
			return(applyTestSuite(x,portfolio,valuationDate))
		}
)

setMethod("applyTestSuite",signature(x="TestSuiteParsed",po="Portfolio"),
		function(x,po,valuationDate) { 		
	
			if (missing(valuationDate)) valuationDate <- Sys.Date()
			
			owners <- x@configLines[["testTarget"]]
			owners <- removeStartEndSpaces(unlist(strsplit(owners,",")))
			owner <- paste(owners,collapse="_")
			
			if (po@owner!=owner) stop(paste("Wrong portfolio for owner",owner))
			return(applyTestSuite(x,as(po,"Positions"),valuationDate,po@referenceCurrency))
		}
)

setMethod("applyTestSuite",signature(x="TestSuiteParsed",po="Positions"),
		function(x,po,valuationDate,referenceCurrency=new("Currency","CHF")) { 		

			if (missing(valuationDate)) valuationDate <- Sys.Date()
			
			# explode the groupDefinitions
			x <- applyGroupDefinition(x)
			
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
			
			# open the logFile
			logFileName <- paste(outputDir,outputFileName,sep=.Platform$file.sep)
			logFile <- file(description=logFileName,open="w")
			cat(paste("Portfolio:",ownerPrintName),file=logFile,sep="\n",append=TRUE)
			cat(paste("Input file:",x@fileName),file=logFile,sep="\n",append=TRUE)
			cat(paste("Inp. directory:",inputDir),file=logFile,sep="\n",append=TRUE)
			cat(paste("Out. directory:",outputDir),file=logFile,sep="\n",append=TRUE)
			cat("\n",file=logFile,sep="\n",append=TRUE)
			
			# open the tmpLogfile where 
			tmpLogFileName <- paste(outputDir,"tmpLog.txt",sep=.Platform$file.sep)
			tmpLogFile <- file(description=tmpLogFileName,open="w")
			# simultaneously apply all the checkStrings on the po
			results <- lapply(x@checkStrings,Apply,po,tmpLogFile,referenceCurrency)
			# close the tmpLogFile
			close(tmpLogFile)
			
			checkResults <- extractFromList(results,"checkResult")
			names(checkResults) <- extractFromList(results,"checkString")
			
			
			results.actualPercentage <- extractFromList(results,"actualPercentage")
			summary <- paste(checkResults,": ", names(checkResults)," (actual ",results.actualPercentage,")",sep="",collapse="\n")
			cat("Summary:",file=logFile,sep="\n",append=TRUE)
			cat(summary,file=logFile,sep="\n",append=TRUE)
			cat("",file=logFile,sep="\n\n",append=TRUE)
			
			inputConnection <- file(description=tmpLogFileName,open="r")
			while (length(input<-readLines(inputConnection, n=1000)) > 0) {
				writeLines(text=input, con=logFile)
			}
			close(logFile)
			close(tmpLogFile)
			unlink(tmpLogFileName)
			
			# print the portfolio to a file
			outputFileName <- paste("Portafoglio_",ownerPrintName,"_",valuationDate,".log",sep="")
			outputDir <- x@configLines[["outputDir"]]
			logFileName <- paste(outputDir,outputFileName,sep="/")
			
		
			logFile <- file(description=logFileName,open="w")
			cat("\n",file=logFile,sep="\n",append=TRUE)
			cat("Portfolio structure:",file=logFile,sep="\n",append=TRUE)
			portfolioStrings <- paste(as.character(po),collapse="\n")
			cat(portfolioStrings,file=logFile,sep="\n",append=TRUE)
			close(logFile)
			
			# create a warning if a FALSE is detected
			if (!all(checkResults)) {
				outputFileName <- paste("warning_",valuationDate,".log",sep="")
				outputDir <- x@configLines[["outputDir"]]
				warningFileName <- file.path(outputDir,outputFileName)
				if (file.exists(warningFileName)) {
					warningFile <- file(description=warningFileName,open="at",encoding="UTF-8") 
				} else {
					warningFile <- file(description=warningFileName,open="wt",encoding="UTF-8")
				} 
				cat(ownerPrintName,file=warningFile,sep="\n",append=TRUE)
				close(warningFile)
				print(ownerPrintName)
			}
			names(checkResults) <- NULL
			return(checkResults)
		}
)
