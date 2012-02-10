# TODO: Add comment
# 
# Author: claudio
###############################################################################


library("RUnit")
source("./lib/library.R")

## test globale
dirs = c(
		"./unitTests/t.utilities",
		"./unitTests/t.repositories",
		"./unitTests/t.currency",
		"./unitTests/t.money",
		"./unitTests/t.securityFactory",
		"./unitTests/t.positionFactory",
		"./unitTests/t.ayrtonPositions",
		"./unitTests/t.positionsFactory",
		"./unitTests/t.positions"
		)
testsuite.lists <- defineTestSuite("Test globale",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)


dirs = c("./unitTests/t.positionsFactory")
testsuite.lists <- defineTestSuite("positionsFactory",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

