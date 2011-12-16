# TODO: Add comment
# 
# Author: claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RUnit")

stringsAsFactors = FALSE
setwd("/home/claudio/workspace/AAA/ITII-calcolo/")
source("./lib/library.R")
source("./unitTests/testUtilities.R")

## test globale
dirs = c("./unitTests/t.lag",
		"./unitTests/t.monomial",
		"./unitTests/t.monomialMethods",
		"./unitTests/t.parser",
		"./unitTests/t.randomVariable",
		"./unitTests/t.symbol",
		"./unitTests/t.expectations"		
		)
testsuite.lists <- defineTestSuite("Test globale",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

