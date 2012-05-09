# TODO: Add comment
# 
# Author: claudio
###############################################################################

# set the rootDir of System
rootDir <- getwd()

# set the working directory to base and import the exchangeRates
mySetwd("base")

source("./unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository

# set the working directory to adviceManagement
mySetwd("adviceManagement")


## test globale
dirs = c(
		"./unitTests/t.PostOffice",
		"./unitTests/t.sendStopToRProcess"#,
		#"./unitTests/t.mail"
		)
testsuite.lists <- defineTestSuite("Test adviceManagement",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()

# restore System working directory
# set the working directory to adviceManagement
mySetwd()




