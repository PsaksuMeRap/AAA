# TODO: Add comment
# 
# Author: claudio
###############################################################################


library("RUnit")
source("./lib/library.R")

## test globale
dirs = c("./unitTests/t.monomial",
		"./unitTests/t.randomVariable",
		"./unitTests/t.symbol",
		"./unitTests/t.monomialMethods",
		"./unitTests/t.parser"
		)
testsuite.lists <- defineTestSuite("Test globale",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

