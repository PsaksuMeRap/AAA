# TODO: Add comment
# 
# Author: Claudio
###############################################################################

# create the log file
source(file.path(sourceCodeDir,"adviceManagement","lib","logger.R"))
logFileName <- paste(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"),"processing",fileName,sep="_")
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

pendingDirectory <- strsplit(fileName,"_")[[1]][4]
pendingDdirectory <- file.path(systemOptions[["homeDir"]],"postOffice",pendingDdirectory,"pending")

# create the message
logger(paste("Creating message for",fileName, "in",pendingDdirectory,"..."))
message <- messageFactory(fileName,pendingDdirectory)
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
