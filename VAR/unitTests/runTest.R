# TODO: Add comment
# 
# Author: claudio
###############################################################################



library("RUnit")
source("./lib/library.R")


## test per l'importazione dei dati
testsuite.repositories <- defineTestSuite("Test creazione repositories",
		dirs = paste(home,"/unitTests/t.repositories",sep=""))

testResult <- runTestSuite(testsuite.repositories); printTextProtocol(testResult)


## test per la classe parser
testsuite.parser <- defineTestSuite("Test parser",
		dirs = paste(home,"/unitTests/t.parser",sep=""))

testResult <- runTestSuite(testsuite.parser); printTextProtocol(testResult)



## test per la classe position
testsuite.position <- defineTestSuite("Test classe position",
		dirs = paste(home,"/unitTests/t.position",sep=""))

testResult <- runTestSuite(testsuite.position); printTextProtocol(testResult)


## test per la classe portfolio
testsuite.portfolio <- defineTestSuite("Test classe portfolio",
		dirs = "./unitTests/t.portfolio")

testResult <- runTestSuite(testsuite.portfolio); printTextProtocol(testResult)


## test per la classe currencyConverter
testsuite.currencyConverter <- defineTestSuite("Test classe currencyConverter",
		dirs = "./unitTests/t.currencyConverter")

testResult <- runTestSuite(testsuite.currencyConverter); printTextProtocol(testResult)


