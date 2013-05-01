# TODO: Add comment
# 
# Author: claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
library("tcltk")

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
source("./allocare/lib/library.R")


source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository


# load the list of test in the variable dirs
source("./allocare/unitTests/tests.R")

testsuite.lists <- defineTestSuite("Tests riskman",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()


