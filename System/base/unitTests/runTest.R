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
#		"./unitTests/t.ayrtonPositions",
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
if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/"
} else {
	home <- "/home/claudio/workspace/AAA/System/"
}

setwd(home)
