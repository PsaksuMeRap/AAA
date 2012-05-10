# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldKillRunningRbatchProcess <- function() {
	# the test start an R batch process which waits
	# 100 seconds
	# the PID of the process is then identified and
	# the kill command executed
	
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.sendStopToRProcess")
	
	# create the file "stop"
	tmp <- getwd()
	setwd(directory)
	
	# start the batch R file
	#if(.Platform$OS.type=="windows") {
	#	system("R --no-save < batch.R", wait=FALSE)
	#} else {
		system("R CMD BATCH --slave --no-restore-history --no-timing --no-save batch.R", wait=FALSE)
	#}
	# identify the PID
	pidOfR <- get_PID()
	
	Sys.sleep(5)
	
	# create the file "stop"
	file.create("stop")
	
	Sys.sleep(4)
	
	# verify the success of the kill
	newPidOfR <- get_PID()

	checkEquals(newPidOfR,numeric(0))
	#remove the stop file
	file.remove("stop")
	file.remove("logFile")
	file.remove("batch.Rout")
	
	# restore the origina working directory
	setwd(tmp)
}



