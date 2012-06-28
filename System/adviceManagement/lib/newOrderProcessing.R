# TODO: Add comment
# 
# Author: Claudio
############################################################################

## First read in the arguments listed at the command line
args=commandArgs(trailingOnly = TRUE)

# if it is a test (lengtht(args)==0) then set test values
if (length(args)==0) {
	sourceCodeDir <- getwd()
	# fileName <- "2012-06-19_14-27-47_Ortelli_globalEconomy_newAdvice.csv"
} else {	
	## args is now a list of character vectors
	## Then cycle through each element of the list and evaluate the expressions.
	eval(parse(text=args[[1]])) # this is the fileName of the message
	eval(parse(text=args[[2]])) # this is the sourceCodeDir variable
}

csvTradesFileName <- fileName

setwd(sourceCodeDir)

source(file.path(sourceCodeDir,"adviceManagement","lib","initialSetup.R"))

# load the adviceManagement code
if (length(args)>0) source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","library.R"))

logFileName <- create_logger(fileType="newAdvice")
# load the advisors
logger("Loading Advisors list ...")
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","advisors.R"))

# extract the portfolioName and construct the directory path where the
# message is waiting for processing
directory <- strsplit(fileName,"_")[[1]][4]
directory <- file.path(systemOptions[["homeDir"]],"postOffice",directory,"pending")

# create the message
logger(paste("Creating message for",file.path(directory,fileName),"..."))
message <- messageFactory(fileName,directory)

# source the repositoryPoliticaInvestimento
logger(paste("Loading repositoryPoliticaInvestimento ..."))
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","methods","repositories","repositoryPoliticaInvestimento.R"))

# source the instrument repository
logger(paste("Loading repositoryInstruments ..."))
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","methods","repositories","repositoryInstruments.R"))

# source the instrument repository
load_repositoryExchangeRate()
logger(paste("Loading repositoryEchangeRates ..."))

# define the checkDirectory, i.e. the directory containing the checkFile, the csv file with
# the desired trades and the output result
checkDirectory <- file.path(systemOptions[["homeDir"]],"postOffice",message[["portfolioName"]],"pending")

# import the portfolio
# create the path to the portfolio directory
logger(paste("Loading portfolio",message[["portfolioName"]],"..."))
portfolio <- loadPortfolio(portfolioId=message[["portfolioName"]])

# import the bloomberg repository
bloombergDataFile <- file.path(systemOptions[["homeDir"]],"data","bloomberg",message[["portfolioName"]],"bloombergData.RData")
load(bloombergDataFile,envir=repositories)
repositories$bloombergData <- repositories$object
rm("object",pos=repositories)

# import the desired trades and the corresponding cash movements as new positions
logger(paste("Loading desired trades",message[["portfolioName"]],"..."))
positionsFromTrades <- tradesToPositionsFactory(csvTradesFileName,checkDirectory)
portfolio <- portfolio + positionsFromTrades
textMessage <- paste("Imported positions from desired trades:\n")
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

result <- applyTestSuite(testSuite@testSuitesParsed[[1]],portfolio)

if (all(result)) {
	logger(paste("Check terminated. All constraints are ok."))
} else {
	logger(paste("Check detected. Some constraints are violated"))
}

# zip all results in a single file
Sys.setenv(R_ZIPCMD=systemOptions[["R_ZIPCMD"]])
zipfile <- file.path(systemOptions[["homeDir"]],"postOffice",message[["portfolioName"]],
		paste("output_check",message[["portfolioName"]],sep="_"))
filesToZip <- list.files(path=file.path(systemOptions[["homeDir"]],"postOffice",
				message[["portfolioName"]],"pending"),full.names=TRUE)
zip(zipfile,files=filesToZip)

## -- fine procedura di controllo parte specifica
sink()
# messageText <- paste("Order from",message[["from"]],"for portfolio",message[["portfolioName"]],"terminated.")
# tkmessageBox(message=messageText,type="ok",icon="info")
