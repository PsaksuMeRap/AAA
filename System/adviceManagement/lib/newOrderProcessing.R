# TODO: Add comment
# 
# Author: Claudio
############################################################################

## First read in the arguments listed at the command line
args=commandArgs(trailingOnly = TRUE)

# if it is a test (lengtht(args)==0) then set test values
if (length(args)==0) {
	sourceCodeDir <- getwd()
	fileName <- "2012-06-19_14-27-47_GhidossiGlobalEconomy_newAdvice.csv"
} else {	
	## args is now a list of character vectors
	## Then cycle through each element of the list and evaluate the expressions.
	eval(parse(text=args[[1]])) # this is the fileName of the message
	eval(parse(text=args[[2]])) # this is the sourceCodeDir variable
}

setwd(sourceCodeDir)

source(file.path(sourceCodeDir,"adviceManagement","lib","initialSetup.R"))

# load the adviceManagement code
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","library.R"))

logFileName <- create_logger(fileType="newAdvice")
# load the advisors
logger("Loading Advisors list ...")
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","advisors.R"))


# extract the folderName and construct the directory path where the
# message is waiting for processing
directory <- strsplit(fileName,"_")[[1]][3]
directory <- file.path(systemOptions[["homeDir"]],"postOffice",directory,"pending")

# create the message
logger(paste("Creating message for",file.path(directory,fileName),"..."))
message <- messageFactory(fileName,directory,advisors)


## -- inizio procedura controllo - parte generale
# import the portfolio
# create the path to the portfolio directory
directoryPortfolio <- file.path(systemOptions[["homeDir"]],"data","portfolios",message@advisor@folderName)
logger(paste("Loading portfolio from",directoryPortfolio,"..."))
portfolio <- loadPortfolio(directoryPortfolio)

# source the repositoryPoliticaInvestimento
logger(paste("Loading repositoryPoliticaInvetimento ..."))
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","methods","repositories","repositoryPoliticaInvestimento.R"))

# source the instrument repository
logger(paste("Loading repositoryInstruments ..."))
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","methods","repositories","repositoryInstruments.R"))

# source the instrument repository
logger(paste("Loading repositoryEchangeRates ..."))
load_repositoryExchangeRate()

sink()
# copy the checkFile into the pre-compliance input/output folder
checkDirectory <- file.path(systemOptions[["homeDir"]],"postOffice",directory,"pending")
ok <- dir.create(checkDirectory)
fileFrom <- file.path(systemOptions[["homeDir"]],) 
fileTo <- file.path()
ok <- file.copy(from=fileFrom,to=fileTo)

# set the working directory to the checkDirectory
setwd(checkDirectory)
## -- fine procedura controllo - parte generale

## -- inizio procedura di controllo parte specifica
repositories$exchangeRates <- create_repositoryExchangeRates(exchangeRatesDate=date)
dati <- importDBPortfolioGeneraleByDate(date)		
fundPortfolios <- filterLists(dati,"Cliente",value=fundsOwners)

fundPortfolios <- portfoliosFactory(fundPortfolios)

AyrtonTestSuite <- testSuiteFactory(testSuiteName="Fondi OpenCapital",directories="./FondiNew")

results <- lapply(AyrtonTestSuite@testSuitesParsed,applyTestSuite,fundPortfolios,date)

## -- fine procedura di controllo parte specifica

topWindow <- tktoplevel()
tktitle(topWindow) <- messageFrom

Sys.sleep(10)

