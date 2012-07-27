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
source("./riskman/lib/library.R")


source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository


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


