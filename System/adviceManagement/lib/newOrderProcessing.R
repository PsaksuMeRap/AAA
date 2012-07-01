# TODO: Add comment
# 
# Author: Claudio
############################################################################

## First read in the arguments listed at the command line
args=commandArgs(trailingOnly = TRUE)

# if it is a test (lengtht(args)==0) then set test values
if (length(args)==0) {
	sourceCodeDir <- getwd()
	fileName <- "2012-06-19_14-27-47_Ortelli_globalEconomy_newAdvice.csv"
	if (.Platform$OS.type=="windows") homeDir <- "C:/riskman" else homeDir <- "/home/claudio/riskman"
} else {	
	## args is now a list of character vectors
	## Then cycle through each element of the list and evaluate the expressions.
	eval(parse(text=args[[1]])) # this is the fileName of the message
	eval(parse(text=args[[2]])) # this is the sourceCodeDir variable
	eval(parse(text=args[[3]])) # this is the homeDir variable
}

# create the log file
source(file.path(sourceCodeDir,"adviceManagement","lib","logger.R"))
logFileName <- paste(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"),"newOrderProcessing",fileName,sep="_")
invisible(create_logger(fileName=logFileName,directory=file.path(homeDir,"log")))
logger("Logger successfully created.\n")
logger(fileName)

# set the working directory to sourceCodeDir
logger("Change working directory to sourceCodeDir ...")
setwd(sourceCodeDir)
loggerDone()

# source the code
logger("Starting initialSetup ...")
source(file.path(sourceCodeDir,"adviceManagement","lib","initialSetup.R"))
# loggerDone not required

csvTradesFileName <- fileName

# extract the portfolioName and construct the directory path where the
# message is waiting for processing
logger("Creating directory containing message ...")
directory <- strsplit(fileName,"_")[[1]][4]
directory <- file.path(systemOptions[["homeDir"]],"postOffice",directory,"pending")
loggerDone()

# create the message
logger(paste("Creating message for",file.path(directory,fileName),"..."))
message <- messageFactory(fileName,directory)
loggerDone()

# source the repositoryPoliticaInvestimento
logger(paste("Loading repositoryPoliticaInvestimento ..."))
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","methods","repositories","repositoryPoliticaInvestimento.R"))
loggerDone()

# source the instrument repository
logger(paste("Loading repositoryInstruments ..."))
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","methods","repositories","repositoryInstruments.R"))
loggerDone()

# source the exchangerates repository
logger(paste("Loading repositoryEchangeRates ..."))
load_repositoryExchangeRate()
loggerDone()

# define the checkDirectory, i.e. the directory containing the checkFile, the csv file with
# the desired trades and the output result
checkDirectory <- file.path(systemOptions[["homeDir"]],"postOffice",message[["portfolioName"]],"pending")

# import the portfolio
# create the path to the portfolio directory
logger(paste("Loading portfolio",message[["portfolioName"]],"..."))
portfolio <- loadPortfolio(portfolioId=message[["portfolioName"]])
loggerDone()

# import the bloomberg repository
logger("Loading the bloomberg data file ...")
bloombergDataFile <- file.path(systemOptions[["homeDir"]],"data","bloomberg",message[["portfolioName"]],"bloombergData.RData")
load(bloombergDataFile,envir=repositories)
repositories$bloombergData <- repositories$object
rm("object",pos=repositories)

# import the desired trades and the corresponding cash movements as new positions
logger(paste("Create position from trades for portfolio",message[["portfolioName"]],"..."))
positionsFromTrades <- tradesToPositionsFactory(csvTradesFileName,checkDirectory)
portfolio <- portfolio + positionsFromTrades
textMessage <- paste("Imported positions from desired trades:")
positionsStrings <- paste(as.character(positionsFromTrades),collapse="\n")
logger(paste(textMessage,positionsStrings,sep="\n"))

# copy the checkFile into the pre-compliance input/output folder
logger(paste("Copying check file for",message[["portfolioName"]],"..."))
fileFrom <- file.path(systemOptions[["homeDir"]],"data","checkFiles",message[["portfolioName"]],"check.txt") 
fileTo <- file.path(checkDirectory,"check.txt")
ok <- file.copy(from=fileFrom,to=fileTo)

# set the working directory to the checkDirectory
setwd(checkDirectory)
## -- fine procedura controllo - parte generale


## -- inizio procedura di controllo parte specifica
testSuite <- testSuiteFactory(testSuiteName=message[["portfolioName"]],directories="./",fileName="check.txt")

testResults <- applyTestSuite(testSuite@testSuitesParsed[[1]],portfolio)
testResult <- all(testResults)
if (testResult) {
	logger(paste("Check terminated. All constraints are ok."))
} else {
	logger(paste("Check detected. Some constraints are violated:\n",paste(testResults,collapse=" - ")))
}

# zip all results in a single file
logger("compressing result files ...")
zipToDir <- file.path(systemOptions[["homeDir"]],"postOffice",message[["portfolioName"]])
zipFromDir <- file.path(systemOptions[["homeDir"]],"postOffice",message[["portfolioName"]],"pending")
zipFullFileName <- zipResults(message,all(testResult),zipFromDir,zipToDir)

# remove all results from the pending directory
logger("Removing result files ...")
isOk <- file.remove(list.files(zipFromDir,full.names=TRUE))

# copy the result in the archive
logger("Archiving compressed result files ...")
if (testResult) {
	to <- file.path(systemOptions[["homeDir"]],"archive","processed","accepted")
} else {
	to <- file.path(systemOptions[["homeDir"]],"archive","processed","rejected")
}
file.copy(zipFullFileName,to)

# copy the zip file into the postOffice/inbox folder
logger("Copying result in the postOffice/inbox folder ...")
to <- file.path(systemOptions[["homeDir"]],"postOffice","inbox")
file.copy(zipFullFileName,to)

# remove compressed file
logger(paste("Removing compressed archive from the postOffice/",message[["portfolioName"]],
				" directory",sep=""))
file.remove(zipFullFileName)

## -- fine procedura di controllo parte specifica
sink()
setwd(systemOptions[["sourceCodeDir"]])
# messageText <- paste("Order from",message[["from"]],"for portfolio",message[["portfolioName"]],"terminated.")
# tkmessageBox(message=messageText,type="ok",icon="info")
