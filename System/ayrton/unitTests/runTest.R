# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./ayrton/lib/library.R")

#source("./unitTests/utilities/createExchangeRatesTestRepository.R")
#testRepository <- createExchangeRatesTestRepository() 
#repositories$exchangeRates <- testRepository



if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/ayrton/"
} else {
	home <- "/home/claudio/workspace/AAA/System/ayrton/"
}

setwd(home)

## test globale
dirs = c(
		"./unitTests/t.ayrtonPositions",
		"./unitTests/t.explode",				
		"./unitTests/t.idAyrton",		
		"./unitTests/t.idFactory",
		"./unitTests/t.portfolioFactory",
		"./unitTests/t.portfoliosFactory",
		"./unitTests/t.positionFactory",
		"./unitTests/t.positionsFactory",
		"./unitTests/t.securityFactory"
)

testsuite.lists <- defineTestSuite("Test ayrton",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()


if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/"
} else {
	home <- "/home/claudio/workspace/AAA/System/"
}

setwd(home)




		
