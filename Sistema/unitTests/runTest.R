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
		"./unitTests/t.strings",
		"./unitTests/t.constraintFactory",
		"./unitTests/t.parser",
		"./unitTests/t.check",
		"./unitTests/t.testSuiteFactory",
		"./unitTests/t.applyTestSuite",
		"./unitTests/t.toXXX",
		"./unitTests/t.selector",
		"./unitTests/t.explode",
		"./unitTests/t.Apply",
		"./unitTests/t.identifyPositionsToExplode",
		"./unitTests/t.explodePortfolioBy"
		)
testsuite.lists <- defineTestSuite("Test globale",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()


dirs = c("./unitTests/t.lists")
testsuite.lists <- defineTestSuite("x1",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.utilities")
testsuite.lists <- defineTestSuite("x2",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.repositories/runit.repositoryExchangeRates")
testsuite.lists <- defineTestSuite("x3",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.currency")
testsuite.lists <- defineTestSuite("x4",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.money")
testsuite.lists <- defineTestSuite("x5",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.securityFactory")
testsuite.lists <- defineTestSuite("x6",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.positionFactory")
testsuite.lists <- defineTestSuite("x7",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.ayrtonPositions")
testsuite.lists <- defineTestSuite("x8",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.positionsFactory")
testsuite.lists <- defineTestSuite("x9",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.position")
testsuite.lists <- defineTestSuite("x10",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.positions")
testsuite.lists <- defineTestSuite("x11",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.portfolio")
testsuite.lists <- defineTestSuite("x12",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.portfolioFactory")
testsuite.lists <- defineTestSuite("x13",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.strings")
testsuite.lists <- defineTestSuite("x14",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.constraintFactory")
testsuite.lists <- defineTestSuite("x15",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.parser")
testsuite.lists <- defineTestSuite("x16",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.check")
testsuite.lists <- defineTestSuite("x17",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.testSuiteFactory")
testsuite.lists <- defineTestSuite("x18",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.applyTestSuite")
testsuite.lists <- defineTestSuite("x19",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.toXXX")
testsuite.lists <- defineTestSuite("x20",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.selector")
testsuite.lists <- defineTestSuite("x21",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.explode")
testsuite.lists <- defineTestSuite("x22",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.Apply")
testsuite.lists <- defineTestSuite("x23",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.identifyPositionsToExplode")
testsuite.lists <- defineTestSuite("x24",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()


dirs = c("./unitTests/t.explodePortfolioBy")
testsuite.lists <- defineTestSuite("x25",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()