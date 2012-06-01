# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./ayrton/lib/library.R")

mySetwd("ayrton")

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


mySetwd()




		
