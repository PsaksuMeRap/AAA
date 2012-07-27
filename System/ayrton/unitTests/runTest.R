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
repositories <- new.env()

if (.Platform$OS.type=="windows") {	
	homeDir <- "C:/riskman"
} else {
	homeDir <- "/home/claudio/riskman"
}

sourceCodeDir <- getwd()

source("./base/lib/library.R")
source("./ayrton/lib/library.R")

source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository


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


