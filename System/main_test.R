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

sys[["sourceCodeDir"]] <- getwd()

rm(homeDir,sourceCodeDir)


## -- fine setup 


# base test suite
source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository
## test globale
dirs = c(
		"./base/unitTests/t.lists",
		"./base/unitTests/t.utilities",
		"./base/unitTests/t.repositories/runit.repositoryExchangeRates",
		"./base/unitTests/t.currency",
		"./base/unitTests/t.money",
		"./base/unitTests/t.position",
		"./base/unitTests/t.positions",
		"./base/unitTests/t.portfolio" ,
		"./base/unitTests/t.toXXX",
		"./base/unitTests/t.explode",
		"./base/unitTests/t.identifyPositionsToExplode",
		"./base/unitTests/t.explodePortfolioBy"
)
testsuite.lists <- defineTestSuite("Test globale",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()


# ayrton test suite

## test globale
dirs = c(
		"./ayrton/unitTests/t.ayrtonPositions",
		"./ayrton/unitTests/t.explode",				
		"./ayrton/unitTests/t.idAyrton",		
		"./ayrton/unitTests/t.idFactory",
		"./ayrton/unitTests/t.portfolioFactory",
		"./ayrton/unitTests/t.portfoliosFactory",
		"./ayrton/unitTests/t.positionFactory",
		"./ayrton/unitTests/t.positionsFactory",
		"./ayrton/unitTests/t.securityFactory"
)

testsuite.lists <- defineTestSuite("Test ayrton",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()


# riskman test suite
## test directories
dirs = c(
		"./riskman/unitTests/t.Apply",
		"./riskman/unitTests/t.strings",
		"./riskman/unitTests/t.constraintFactory",
		"./riskman/unitTests/t.parser",
		"./riskman/unitTests/t.check",
		"./riskman/unitTests/t.testSuiteFactory",
		"./riskman/unitTests/t.applyTestSuite",
		"./riskman/unitTests/t.selector",
		"./riskman/unitTests/t.selectionCriteriumFactory"

)

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
