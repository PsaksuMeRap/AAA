# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./riskman/lib/library.R")


# set the working directory to base
mySetwd("base")

source("./unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository

# set the working directory to riskman
mySetwd("riskman")

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


# set the working directory to System
mySetwd()
