# TODO: Add comment
# 
# Author: claudio
###############################################################################


library("RUnit")
source("./lib/library.R")

## test globale
dirs = c(
		"./unitTests/t.lists",
		"./unitTests/t.utilities",
		"./unitTests/t.repositories/runit.repositoryExchangeRates",
		"./unitTests/t.currency",
		"./unitTests/t.money",
		"./unitTests/t.securityFactory",
		"./unitTests/t.positionFactory",
		"./unitTests/t.ayrtonPositions",
		"./unitTests/t.positionsFactory",
		"./unitTests/t.position",		
		"./unitTests/t.positions",
		"./unitTests/t.portfolio" ,
		"./unitTests/t.portfolioFactory",
		"./unitTests/t.riskmanTestSuite",
		"./unitTests/t.strings",
		"./unitTests/t.constraintFactory",
		"./unitTests/t.parser",
		"./unitTests/t.check",
		"./unitTests/t.testSuiteFactory"
		)
testsuite.lists <- defineTestSuite("Test globale",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

library("RUnit")
dirs = c("./unitTests/t.testSuiteFactory")
testsuite.lists <- defineTestSuite("testSuiteFactory",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

