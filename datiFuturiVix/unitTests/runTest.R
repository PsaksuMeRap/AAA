# TODO: Add comment
# 
# Author: claudio
###############################################################################


library("RUnit")
source("./lib/library.R")

## test globale
dirs = c("./unitTests/t.importer",
		 "./unitTests/t.dsCodeParser",
		 "./unitTests/t.contract")

testsuite.lists <- defineTestSuite("Test globale", dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)



## test per le procedure lists.R
testsuite.importer <- defineTestSuite("Test dell'importatore",
		dirs = "./unitTests/t.importer")

testResult <- runTestSuite(testsuite.importer); printTextProtocol(testResult)

