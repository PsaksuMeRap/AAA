# TODO: Add comment
# 
# Author: Claudio
###############################################################################

test.shouldProcessBatchfile <- function() {
	tmp <- getwd()
	mySetwd(file.path("adviceManagement","unitTests","t.startBatchProcess"))
	
	command <- "c:\\Progra~1\\R\\R-2.14.2\\bin\\R CMD BATCH --slave --no-restore-history --no-timing --no-save \"--args x='ciao' y=4\" orderProcessing.R"
	
	system(command)
	
	checkEquals(file.exists("exitOk"),TRUE)
	
	ok <- file.remove("exitOk")
	ok <- file.remove("orderProcessing.Rout")
	setwd(tmp)
	
}