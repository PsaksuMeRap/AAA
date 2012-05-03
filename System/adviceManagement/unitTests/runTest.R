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

if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/adviceManagement/"
} else {
	home <- "/home/claudio/workspace/AAA/System/adviceManagement/"
}

setwd(home)

## test globale
dirs = c(
		"./unitTests/t.PostOffice"
		)
testsuite.lists <- defineTestSuite("Test adviceManagement",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()

# restore System working directory
if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/"
} else {
	home <- "/home/claudio/workspace/AAA/System/"
}

setwd(home)
