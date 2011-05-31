# TODO: Add comment
# 
# Author: claudio
###############################################################################


library("RUnit")
source("./lib/library.R")

## test globale
dirs = c("./unitTests/t.lists",
		"./unitTests/t.utilities",
		"./unitTests/t.repositories",
		"./unitTests/t.parser",
		"./unitTests/t.money",
		"./unitTests/t.accruedInterest",
		"./unitTests/t.position",
		"./unitTests/t.portfolio",
		"./unitTests/t.criterium",
		"./unitTests/t.positionsMethods",
		"./unitTests/t.riskmanTestSuite",
		"./unitTests/t.riskFactor"
		)
testsuite.lists <- defineTestSuite("Test globale",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)





## test per le procedure lists.R
testsuite.lists <- defineTestSuite("Test procedure lists.R",
		dirs = "./unitTests/t.lists")

testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)



## test per la procedura allocateTestRepositories
testsuite.allocateTestRepositories <- defineTestSuite("Test creazione dei repositories di test",
		dirs = "./unitTests/t.utilities")

testResult <- runTestSuite(testsuite.allocateTestRepositories); printTextProtocol(testResult)


## test per le classi repositoryXYZ (exchangeRates, interestRates, ...)
testsuite.repositories <- defineTestSuite("Test creazione repositories",
		dirs = "./unitTests/t.repositories")

testResult <- runTestSuite(testsuite.repositories); printTextProtocol(testResult)


## test per la classe parser
testsuite.parser <- defineTestSuite("Test parser",
		dirs = "./unitTests/t.parser")

testResult <- runTestSuite(testsuite.parser); printTextProtocol(testResult)


## test per la classe money
testsuite.money <- defineTestSuite("Test classe money",
		dirs = "./unitTests/t.money")

testResult <- runTestSuite(testsuite.money); printTextProtocol(testResult)


## test per la classe accruedInterest
testsuite.accruedInterest <- defineTestSuite("Test classe accruedInterest",
		dirs = "./unitTests/t.accruedInterest")

testResult <- runTestSuite(testsuite.accruedInterest); printTextProtocol(testResult)


## test per la classe position
testsuite.position <- defineTestSuite("Test classe position e positions",
		dirs = "./unitTests/t.position")

testResult <- runTestSuite(testsuite.position); printTextProtocol(testResult)


## test per la classe portfolio
testsuite.portfolio <- defineTestSuite("Test classe portfolio",
		dirs = "./unitTests/t.portfolio")

testResult <- runTestSuite(testsuite.portfolio); printTextProtocol(testResult)


## test per la directory t.criterium
testsuite.criterium <- defineTestSuite("Test directory criterium",
		dirs = "./unitTests/t.criterium")

testResult <- runTestSuite(testsuite.criterium); printTextProtocol(testResult)



## test per la classe positionsMethods
testsuite.positionsMethods <- defineTestSuite("Test dei metodi positionsMethods",
		dirs = "./unitTests/t.positionsMethods")

testResult <- runTestSuite(testsuite.positionsMethods); printTextProtocol(testResult)



## test per la classe riskmanTestSuite
testsuite.riskmanTestSuite <- defineTestSuite("Test della classe riskmanTestSuite",
		dirs = "./unitTests/t.riskmanTestSuite")

testResult <- runTestSuite(testsuite.riskmanTestSuite); printTextProtocol(testResult)


## test per la classe riskFactor
testsuite.riskFactorTestSuite <- defineTestSuite("Test della classe riskFactor",
		dirs = "./unitTests/t.riskFactor")

testResult <- runTestSuite(testsuite.riskFactorTestSuite); printTextProtocol(testResult)
