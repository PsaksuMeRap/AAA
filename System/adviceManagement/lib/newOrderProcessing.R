# TODO: Add comment
# 
# Author: Claudio
############################################################################


library("tcltk")

## First read in the arguments listed at the command line
args=commandArgs(trailingOnly = TRUE)

## args is now a list of character vectors
## Then cycle through each element of the list and evaluate the expressions.
eval(parse(text=args[[1]])) # this is the fileName of the message
eval(parse(text=args[[2]])) # this is the sourceCodeDir variable

setwd(sourceCodeDir)
source(file.path(sourceCodeDir,"adviceManagement","lib","initialSetup.R"))

saveLastObject(args,fileName,directory="C:/Users/Claudio/workspace/AAA/System")

# load the advisors
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","advisors.R"))
 
# extract the folderName and construct the directory path
directory <- strsplit(fileName,"_")[[1]][3]
directory <- file.path(systemOptions[["homeDir"]],"postOffice",directory,"pending")

message <- paste(systemOptions[["homeDir"]],systemOptions[["sourceCodeDir"]],sep="\n")
tkmessageBox(message=message,type="ok")
quit(save="no")

# create the message
message <- messageFactory(fileName,directory,advisors)

## -- inizio procedura controllo - parte generale
# import the portfolio
	# create the path to the portfolio directory
directoryPortfolio <- file.path(systemOptions[["homeDir"]],"data","portfolios",message@advisor@portfolioName)
portfolio <- loadPortfolio(directoryPortfolio)

# source the repositoryPoliticaInvestimento
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","repositoryPoliticaInvestimento.R"))

# source the instrument repository
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","repositoryInstruments.R"))

repositories$exchangeRates <- create_repositoryExchangeRates()

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

