# TODO: Add comment
# 
# Author: claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RUnit")
library("tcltk")
library("stringr")

if(.Platform$OS.type=="windows") {
	library("rJava")
	library("Rbbg")
}

stringsAsFactors = FALSE

# this variable is used to indicate that we are running a testsuite
isTest <- TRUE
repositories <- new.env()
if (.Platform$OS.type=="windows") {
	homeDir <- "C:/riskman" 
} else {
	homeDir <- "/home/claudio/riskman"
}

# set the directory where the source code is installed (i.e. folders adviceManagement, ayrton, base, riskman)
sourceCodeDir <- getwd()

source("./base/lib/library.R")
source("./ayrton/lib/library.R")
source("./riskman/lib/library.R")

systemOptions[["sourceCodeDir"]] <- sourceCodeDir
source("./adviceManagement/lib/library.R")

source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories <- new.env()
repositories$exchangeRates <- testRepository

# source the test advisors
source("./adviceManagement/unitTests/t.advisors/advisors.R")

## test globale
dirs = c(
		"./adviceManagement/unitTests/t.logger",
		"./adviceManagement/unitTests/t.fileMove",
		"./adviceManagement/unitTests/t.archive",
		"./adviceManagement/unitTests/t.postOffice",
		"./adviceManagement/unitTests/t.mailBox",
		"./adviceManagement/unitTests/t.sendStopToRProcess",
		"./adviceManagement/unitTests/t.detectRprocesses",
		"./adviceManagement/unitTests/t.messageFactory",
		"./adviceManagement/unitTests/t.newAdviceNoLock",
		"./adviceManagement/unitTests/t.newAdviceWithLock",
		"./adviceManagement/unitTests/t.lock_unlock",
		"./adviceManagement/unitTests/t.mainMessageProcessing",
		"./adviceManagement/unitTests/t.mail",
		"./adviceManagement/unitTests/t.tradeFactory",
	 	"./adviceManagement/unitTests/t.tradeToSecurityFactory",
		"./adviceManagement/unitTests/t.tradeToPositionFactory",
		"./adviceManagement/unitTests/t.tradeToPositionsFactory",
		"./adviceManagement/unitTests/t.tradesToPositionsFactory",
		"./adviceManagement/unitTests/t.loadLastPortfolio",
		"./adviceManagement/unitTests/t.saveLastObject",
		"./adviceManagement/unitTests/t.bloombergData",
		"./adviceManagement/unitTests/t.bloombergRequestHandler",
		"./adviceManagement/unitTests/t.zipResults"
		#"./adviceManagement/unitTests/t.tradesToPositionsFactory"
		#"./adviceManagement/unitTests/t.subNewOrderProcessing"
	)
testsuite.lists <- defineTestSuite("Test adviceManagement",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()


# restore System working directory
# set the working directory to adviceManagement
mySetwd()


## test messageFactory
dirs = c(
		"./adviceManagement/unitTests/t.messageFactory"
)
testsuite.lists <- defineTestSuite("Test adviceManagement",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()

