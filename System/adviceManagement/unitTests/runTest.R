# TODO: Add comment
# 
# Author: claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
library("tcltk")
library("stringr")
library("Rbbg")

stringsAsFactors = FALSE

# this variable is used to indicate that we are running a testsuite
isTest <- TRUE

source("./base/lib/library.R")

# set the directory where the source code is installed (i.e. folders adviceManagement, ayrton, base, riskman)
sourceCodeDir <- getwd()
systemOptions[["sourceCodeDir"]] <- sourceCodeDir

source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories <- new.env()
repositories$exchangeRates <- testRepository

# source the test advisors
source("./adviceManagement/unitTests/advisors.R")

## test globale
dirs = c(
		"./adviceManagement/unitTests/t.fileMove",
		"./adviceManagement/unitTests/t.archive",
		"./adviceManagement/unitTests/t.PostOffice",
		"./adviceManagement/unitTests/t.sendStopToRProcess",
		"./adviceManagement/unitTests/t.detectRprocesses",
		"./adviceManagement/unitTests/t.messageFactory",
		# "./adviceManagement/unitTests/t.noLockOnNewAdvice",
		"./adviceManagement/unitTests/t.isLockOnNewAdvice",
		"./adviceManagement/unitTests/t.lock_unlock",
		"./adviceManagement/unitTests/t.messageProcessing",
		"./adviceManagement/unitTests/t.mail",
		"./adviceManagement/unitTests/t.tradeFactory",
	 	"./adviceManagement/unitTests/t.tradeToSecurityFactoryStep1",
		"./adviceManagement/unitTests/t.loadLastPortfolio",
		"./adviceManagement/unitTests/t.saveLastObject",
		"./adviceManagement/unitTests/t.bloombergData",
		"./adviceManagement/unitTests/t.bloombergRequestHandler"
	)
testsuite.lists <- defineTestSuite("Test adviceManagement",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()


# restore System working directory
# set the working directory to adviceManagement
mySetwd()


## test messageFactory
dirs = c(
		"./adviceManagement/unitTests/t.tradeToSecurityFactoryStep1"
)
testsuite.lists <- defineTestSuite("Test adviceManagement",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()

