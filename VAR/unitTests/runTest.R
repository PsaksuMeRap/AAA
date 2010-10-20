# TODO: Add comment
# 
# Author: claudio
###############################################################################



library("RUnit")
source("./lib/library.R")


## test per le classi repositoryXYZ (exchangeRates, interestRates, ...)
testsuite.repositories <- defineTestSuite("Test creazione repositories",
		dirs = paste(home,"/unitTests/t.repositories",sep=""))

testResult <- runTestSuite(testsuite.repositories); printTextProtocol(testResult)


## test per la classe parser
testsuite.parser <- defineTestSuite("Test parser",
		dirs = paste(home,"/unitTests/t.parser",sep=""))

testResult <- runTestSuite(testsuite.parser); printTextProtocol(testResult)


## test per la classe money
testsuite.money <- defineTestSuite("Test classe money",
		dirs = "./unitTests/t.money")

testResult <- runTestSuite(testsuite.money); printTextProtocol(testResult)


## test per la classe position
testsuite.position <- defineTestSuite("Test classe position e positions",
		dirs = paste(home,"/unitTests/t.position",sep=""))

testResult <- runTestSuite(testsuite.position); printTextProtocol(testResult)


## test per la classe portfolio
testsuite.portfolio <- defineTestSuite("Test classe portfolio",
		dirs = "./unitTests/t.portfolio")

testResult <- runTestSuite(testsuite.portfolio); printTextProtocol(testResult)


