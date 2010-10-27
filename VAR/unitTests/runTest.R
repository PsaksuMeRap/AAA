# TODO: Add comment
# 
# Author: claudio
###############################################################################


library("RUnit")
source("./lib/library.R")


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


## test per la classe selectionCriterium
testsuite.selectionCriterium <- defineTestSuite("Test classe selectionCriterium",
		dirs = "./unitTests/t.selectionCriterium")

testResult <- runTestSuite(testsuite.selectionCriterium); printTextProtocol(testResult)


## test per la classe positionsMethods
testsuite.positionsMethods <- defineTestSuite("Test dei metodi positionsMethods",
		dirs = "./unitTests/t.positionsMethods")

testResult <- runTestSuite(testsuite.positionsMethods); printTextProtocol(testResult)
