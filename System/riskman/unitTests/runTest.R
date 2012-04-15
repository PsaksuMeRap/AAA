# TODO: Add comment
# 
# Author: claudio
###############################################################################

if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/riskman/"
} else {
	home <- "/home/claudio/workspace/AAA/System/riskman/"
}

setwd(home)

source("./unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository

## test globale
dirs = c(
		"./unitTests/t.strings",
		"./unitTests/t.constraintFactory",
		"./unitTests/t.parser",
		"./unitTests/t.check",
		"./unitTests/t.testSuiteFactory",
		"./unitTests/t.applyTestSuite",
		"./unitTests/t.selector",
		"./unitTests/t.selectionCriteriumFactory"
		)
testsuite.lists <- defineTestSuite("Test globale",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()
