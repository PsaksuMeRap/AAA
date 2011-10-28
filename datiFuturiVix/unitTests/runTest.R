# TODO: Add comment
# 
# Author: claudio
###############################################################################


library("RUnit")
source("./lib/library.R")

## test globale
dirs = c("./unitTests/t.importerVixFutures",
		 "./unitTests/t.dsCodeParser",
		 "./unitTests/t.contract",
		 "./unitTests/t.futureContractMethods",
		 "./unitTests/t.dataProcedures",
		 "./unitTests/t.utilities")

testsuite.lists <- defineTestSuite("Test globale", dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)




## test per le procedure contractMethods
testsuite.contractMethods <- defineTestSuite("Test contractMethods",
		dirs = "./unitTests/t.contractMethods")

testResult <- runTestSuite(testsuite.contractMethods); printTextProtocol(testResult)





## test per le procedure lists.R
testsuite.importer <- defineTestSuite("Test dell'importatore",
		dirs = "./unitTests/t.importer")

testResult <- runTestSuite(testsuite.importer); printTextProtocol(testResult)

