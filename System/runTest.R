# TODO: Add comment
# 
# Author: claudio
###############################################################################

if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/base/"
} else {
	home <- "/home/claudio/workspace/AAA/System/base/"
}

setwd(home)

source("./unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository

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
		"./unitTests/t.toXXX",
		"./unitTests/t.explode",
		"./unitTests/t.identifyPositionsToExplode",
		"./unitTests/t.explodePortfolioBy",
		"./unitTests/t.idAyrton",	
		"./unitTests/t.idFactory"
		)
testsuite.lists <- defineTestSuite("Test globale",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()



# the riskman test suites 
if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/riskman/"
} else {
	home <- "/home/claudio/workspace/AAA/System/riskman/"
}

setwd(home)

## test globale
dirs = c(
		"./unitTests/t.Apply",
		"./unitTests/t.strings",
		"./unitTests/t.constraintFactory",
		"./unitTests/t.parser",
		"./unitTests/t.check",
		"./unitTests/t.testSuiteFactory",
		"./unitTests/t.applyTestSuite",
		"./unitTests/t.selector"

)
testsuite.lists <- defineTestSuite("Tests riskman",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()


























dirs = c("./unitTests/t.idAyrton")
testsuite.lists <- defineTestSuite("idAyrton",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)

warnings()

dirs = c("./unitTests/t.idFactory")
testsuite.lists <- defineTestSuite("idFactory",dirs = dirs)
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