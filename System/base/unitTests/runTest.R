# TODO: Add comment
# 
# Author: claudio
###############################################################################

mySetwd("base")

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
		"./unitTests/t.positionsFactory",
		"./unitTests/t.position",		
		"./unitTests/t.positions",
		"./unitTests/t.portfolio" ,
#		"./unitTests/t.portfolioFactory",
		"./unitTests/t.toXXX",
		"./unitTests/t.explode",
#		"./unitTests/t.Apply",
		"./unitTests/t.identifyPositionsToExplode",
		"./unitTests/t.explodePortfolioBy"
#		"./unitTests/t.idAyrton",	
#		"./unitTests/t.idFactory"
		)
testsuite.lists <- defineTestSuite("Test globale",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()

# restore System working directory
mySetwd()
