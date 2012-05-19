# TODO: Add comment
# 
# Author: claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
library("tcltk")
library("stringr")

stringsAsFactors = FALSE

source("./base/lib/library.R")

# set the directory where the source code is installed (i.e. folders adviceManagement, ayrton, base, riskman)
sourceCodeDir <- getwd()
systemOptions[["sourceCodeDir"]] <- sourceCodeDir

# set the homeDir, i.e. the directory where the postBox must be installed
homeDir <- file.path(sourceCodeDir,"adviceManagement","unitTests","directories")
systemOptions[["homeDir"]] <- "c:/riskman"

# import the adviceManagement library
#source(file.path(sourceCodeDir,"adviceManagement","lib","library.R"))

# set the working directory to base and import the exchangeRates
mySetwd("base")

source("./unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories <- new.env()
repositories$exchangeRates <- testRepository

# set the working directory to sourceCodeDir
mySetwd()


## test globale
dirs = c(
		"./adviceManagement/unitTests/t.fileMove",
		"./adviceManagement/unitTests/t.archive",
		"./adviceManagement/unitTests/t.PostOffice",
		"./adviceManagement/unitTests/t.sendStopToRProcess",
		"./adviceManagement/unitTests/t.detectRprocesses",
		"./adviceManagement/unitTests/t.messageFactory",
		"./adviceManagement/unitTests/t.noLockOnNewAdvice",
		"./adviceManagement/unitTests/t.isLockOnNewAdvice",
		"./adviceManagement/unitTests/t.lock_unlock",
		"./adviceManagement/unitTests/t.messageProcessing",
		"./adviceManagement/unitTests/t.mail",
		"./adviceManagement/unitTests/t.tradeFactory",
		"./adviceManagement/unitTests/t.tradeToPositionFactory"
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

