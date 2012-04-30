# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./riskman/lib/library.R")

if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/riskman/"
} else {
	home <- "/home/claudio/workspace/AAA/System/riskman/"
}

setwd(home)


source("./unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository

## test directories
dirs = c(
		"./unitTests/t.Apply",
		"./unitTests/t.strings",
		"./unitTests/t.constraintFactory",
		"./unitTests/t.parser",
		"./unitTests/t.check",
		"./unitTests/t.testSuiteFactory",
		"./unitTests/t.applyTestSuite",
		"./unitTests/t.selector",
		"./unitTests/t.selectionCriteriumFactory"

)
testsuite.lists <- defineTestSuite("Tests riskman",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()


if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/"
} else {
	home <- "/home/claudio/workspace/AAA/System/"
}

setwd(home)
