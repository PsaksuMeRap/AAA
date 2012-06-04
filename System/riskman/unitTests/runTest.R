# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./riskman/lib/library.R")


# set the working directory to base
# mySetwd("base")

source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository

# set the working directory to riskman
# mySetwd("riskman")

## test directories
dirs = c(
		"./riskman/unitTests/t.Apply",
		"./riskman/unitTests/t.strings",
		"./riskman/unitTests/t.constraintFactory",
		"./riskman/unitTests/t.parser",
		"./riskman/unitTests/t.check",
		"./riskman/unitTests/t.testSuiteFactory",
		"./riskman/unitTests/t.applyTestSuite",
		"./riskman/unitTests/t.selector",
		"./riskman/unitTests/t.selectionCriteriumFactory"

)
testsuite.lists <- defineTestSuite("Tests riskman",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()


# set the working directory to System
# mySetwd()
