# TODO: Add comment
# 
# Author: claudio
###############################################################################

# mySetwd("base")

source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository

## test globale
dirs = c(
		"./base/unitTests/t.lists",
		"./base/unitTests/t.utilities",
		"./base/unitTests/t.repositories/runit.repositoryExchangeRates",
		"./base/unitTests/t.currency",
		"./base/unitTests/t.money",
		"./base/unitTests/t.securityFactory",
		"./base/unitTests/t.position",
		"./base/unitTests/t.positions",
		"./base/unitTests/t.portfolio" ,
		"./base/unitTests/t.toXXX",
		"./base/unitTests/t.explode",
		"./base/unitTests/t.identifyPositionsToExplode",
		"./base/unitTests/t.explodePortfolioBy"
		)
testsuite.lists <- defineTestSuite("Test globale",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()

# restore System working directory
# mySetwd()

