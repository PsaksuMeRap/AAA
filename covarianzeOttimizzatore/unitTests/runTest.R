# TODO: Add comment
# 
# Author: claudio
###############################################################################


library("RUnit")
source("./lib/library.R")

## test globale
dirs = c("./unitTests/t.utilities")

testsuite.lists <- defineTestSuite("Test globale", dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)


## test per le procedure utilities
testsuite.utilities <- defineTestSuite("Test utilities",
		dirs = "./unitTests/t.utilities")

testResult <- runTestSuite(testsuite.utilities); printTextProtocol(testResult)
