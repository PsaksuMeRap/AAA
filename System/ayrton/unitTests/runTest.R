# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./ayrton/lib/library.R")

# mySetwd("ayrton")

## test globale
dirs = c(
		"./ayrton/unitTests/t.ayrtonPositions",
		"./ayrton/unitTests/t.explode",				
		"./ayrton/unitTests/t.idAyrton",		
		"./ayrton/unitTests/t.idFactory",
		"./ayrton/unitTests/t.portfolioFactory",
		"./ayrton/unitTests/t.portfoliosFactory",
		"./ayrton/unitTests/t.positionFactory",
		"./ayrton/unitTests/t.positionsFactory",
		"./ayrton/unitTests/t.securityFactory"
)

testsuite.lists <- defineTestSuite("Test ayrton",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()
# mySetwd()




		
