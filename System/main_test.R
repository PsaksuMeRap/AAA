# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
library("tcltk")
library("stringr")

stringsAsFactors = FALSE
repositories <- new.env()

if (.Platform$OS.type=="windows") {
	homeDir <- "C:/riskman"
} else {
	homeDir <- "/home/claudio/riskman"
}

sourceCodeDir <- getwd()

source("./base/lib/library.R")
source("./ayrton/lib/library.R")
source("./riskman/lib/library.R")
source("./adviceManagement/lib/library.R")

sys[["sourceCodeDir"]] <- getwd()

rm(homeDir,sourceCodeDir)

source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository
## -- fine setup 





# base test suite
source("./base/unitTests/tests.R")

testsuite.lists <- defineTestSuite("Test globale",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()


# ayrton test suite
source("./ayrton/unitTests/tests.R")

testsuite.lists <- defineTestSuite("Test ayrton",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()




# riskman test suite
source("./riskman/unitTests/tests.R")

dirs <- "./riskman/unitTests/t.Apply"
testsuite.lists <- defineTestSuite("Tests riskman",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()


# adviceManagement test suite
if(.Platform$OS.type=="windows") {
	library("rJava")
	library("Rbbg")
}

# source the test advisors
source("./adviceManagement/unitTests/t.advisors/advisors.R")
source(file.path(sys[["sourceCodeDir"]],"adviceManagement","lib","secrets.R"))

## test globale
dirs = c(
		"./adviceManagement/unitTests/t.archive",
		#"./adviceManagement/unitTests/t.bloombergData",
		#"./adviceManagement/unitTests/t.bloombergRequestHandler",
		"./adviceManagement/unitTests/t.confirmationNoLock",
		"./adviceManagement/unitTests/t.confirmationWithLock",
		"./adviceManagement/unitTests/t.detectRprocesses",
		"./adviceManagement/unitTests/t.fileMove",
		"./adviceManagement/unitTests/t.logger",
		"./adviceManagement/unitTests/t.loadPortfolio",
		"./adviceManagement/unitTests/t.lock_unlock",
		"./adviceManagement/unitTests/t.logger",
		"./adviceManagement/unitTests/t.mail",
		"./adviceManagement/unitTests/t.mailBox",
		"./adviceManagement/unitTests/t.mainMessageProcessing",
		"./adviceManagement/unitTests/t.messageFactory",
		"./adviceManagement/unitTests/t.messageFileNameFactory",
		"./adviceManagement/unitTests/t.newAdviceNoLock",
		"./adviceManagement/unitTests/t.newAdviceWithLock",
		"./adviceManagement/unitTests/t.postOffice",
		"./adviceManagement/unitTests/t.saveLastObject",
		"./adviceManagement/unitTests/t.sendStopToRProcess",
		"./adviceManagement/unitTests/t.startBatchProcess",
		"./adviceManagement/unitTests/t.subConfirmationProcessing",
		"./adviceManagement/unitTests/t.subNewAdviceProcessing",
		"./adviceManagement/unitTests/t.tradeFactory",
		"./adviceManagement/unitTests/t.tradesToPositionsFactory",	
		"./adviceManagement/unitTests/t.tradeToPositionFactory",
		"./adviceManagement/unitTests/t.tradeToPositionsFactory",		
		"./adviceManagement/unitTests/t.tradeToSecurityFactory",
		"./adviceManagement/unitTests/t.zipResults"
)

testsuite.lists <- defineTestSuite("Test adviceManagement",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()
