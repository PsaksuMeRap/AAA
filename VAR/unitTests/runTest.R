# TODO: Add comment
# 
# Author: claudio
###############################################################################



library("RUnit")
home = "/home/claudio/eclipse/AAA/VAR"
setwd(home)
source(paste(home,"/lib/library.R",sep=""))



## test per l'importazione dei dati
testsuite.repositories <- defineTestSuite("Test creazione repositories",
		dirs = paste(home,"/unitTests/repositories",sep=""))

testResult <- runTestSuite(testsuite.repositories); printTextProtocol(testResult)



## test per la classe position
testsuite.position <- defineTestSuite("Test classe position",
		dirs = paste(home,"/unitTests/position",sep=""))

testResult <- runTestSuite(testsuite.position); printTextProtocol(testResult)