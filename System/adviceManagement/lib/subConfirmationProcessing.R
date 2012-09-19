# TODO: Add comment
# 
# Author: Claudio
############################################################################

## First read in the arguments listed at the command line
args=commandArgs(trailingOnly = TRUE)

# if it is a test (lengtht(args)==0) then set test values
if (length(args)==0) {
	sourceCodeDir <- getwd()
	csvTradesFileName <- "2012-05-21_12-33-21_Riskmanager_globalEconomy_confirmation.csv"
	if (.Platform$OS.type=="windows") homeDir <- "C:/riskman" else homeDir <- "/home/claudio/riskman"
} else {	
	## args is now a list of character vectors
	## Then cycle through each element of the list and evaluate the expressions.
	eval(parse(text=args[[1]])) # this is the fileName of the message
	eval(parse(text=args[[2]])) # this is the sourceCodeDir variable
	eval(parse(text=args[[3]])) # this is the homeDir variable
	
	if (.Platform$OS.type=="windows") {
		library("tcltk",quietly=TRUE,verbose=FALSE)
		library("stringr",quietly=TRUE)
		library("rJava",quietly=TRUE)
		library("Rbbg",quietly=TRUE)
	} else {
		library("tcltk",quietly=TRUE,verbose=FALSE)
		library("stringr",quietly=TRUE)
		library("rJava",quietly=TRUE)
	}
}

source(file.path(sourceCodeDir,"adviceManagement","lib","subInitialSetup.R"))

# import the desired trades and the corresponding cash movements as new positions
logger(paste("Create position from trades for portfolio",message[["portfolioName"]],"..."))
positionsFromTrades <- tradesToPositionsFactory(csvTradesFileName,checkDirectory)
portfolio <- portfolio + positionsFromTrades
textMessage <- paste("Imported positions from desired trades:")
positionsStrings <- paste(as.character(positionsFromTrades),collapse="\n")
logger(paste(textMessage,positionsStrings,sep="\n"))

# save the portfolio
logger(paste("Saving the portfolio in the data/portfolios directory ..."))
directory <- file.path(sys[["homeDir"]],"data","portfolios",message[["portfolioName"]])
saveLastObject(portfolio,"portfolio.RData",directory)
loggerDone()


# copy the checkFile into the pre-compliance input/output folder
logger(paste("Copying check file for",message[["portfolioName"]],"in",checkDirectory,"..."))
checkFileName <- paste("check_",message[["portfolioName"]],".txt",sep="")
fileFrom <- file.path(sys[["homeDir"]],"data","checkFiles",message[["portfolioName"]],checkFileName) 
fileTo <- file.path(checkDirectory,checkFileName)
ok <- file.copy(from=fileFrom,to=fileTo)
if (!ok) {
	logger(paste("Error when copying",checkFileName,"in",checkDirectory,"!"))
	loggerDone()
	return(0)
}
loggerDone()

# set the working directory to the checkDirectory
logger(paste("Set the working directory to",checkDirectory))
setwd(checkDirectory)
loggerDone()
## -- fine procedura controllo - parte generale


## -- inizio procedura di controllo parte specifica
testSuite <- testSuiteFactory(testSuiteName=message[["portfolioName"]],directories="./",fileName=checkFileName)

testResults <- applyTestSuite(testSuite@testSuitesParsed[[1]],portfolio)
testResult <- all(testResults)
if (testResult) {
	logger(paste("Check terminated. All constraints are ok."))
} else {
	logger(paste("Check detected. Some constraints are violated:\n",paste(testResults,collapse=" - ")))
}

# zip all results in a single file
logger("compressing result files ...")
zipToDir <- file.path(sys[["homeDir"]],"postOffice",message[["portfolioName"]])
zipFromDir <- file.path(sys[["homeDir"]],"postOffice",message[["portfolioName"]],"pending")
zipFullFileName <- zipResults(message,all(testResult),zipFromDir,zipToDir)
loggerDone()

# remove all results from the pending directory
logger("Removing result files ... ")
cat(file.remove(list.files(zipFromDir,full.names=TRUE)))

# copy the result in the archive
logger("Archiving compressed result files ... ")
if (testResult) {
	to <- file.path(sys[["homeDir"]],"archive","processed","accepted")
} else {
	to <- file.path(sys[["homeDir"]],"archive","processed","rejected")
}
cat(file.copy(zipFullFileName,to))

# copy the zip file into the postOffice/inbox folder
logger("Copying result in the postOffice/inbox folder ... ")
to <- file.path(sys[["homeDir"]],"postOffice","inbox")
cat(file.copy(zipFullFileName,to))

# remove compressed file
logger(paste("Removing compressed archive from the postOffice/",message[["portfolioName"]],
				" directory"," ... ",sep=""))
cat(file.remove(zipFullFileName))

## -- fine procedura di controllo parte specifica
sink()
