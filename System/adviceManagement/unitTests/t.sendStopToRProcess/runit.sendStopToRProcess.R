# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldKillRunningRbatchProcess <- function() {
	# the test start an R batch process which waits
	# 100 seconds
	# the PID of the process is then identified and
	# the kill command executed
	
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.sendStopToRProcess")
	
	# create the file "stop"
	tmp <- getwd()
	setwd(directory)
	
	# start the batch R file
	#if(.Platform$OS.type=="windows") {
	#	system("R --no-save < batch.R", wait=FALSE)
	#} else {
		system("R CMD BATCH --slave --no-restore-history --no-timing --no-save batch.R", wait=FALSE)
	#}
	
	Sys.sleep(2.5)
	# identify the PID
	if (.Platform$OS.type=="windows") pidOfR <- get_PID("Rterm.exe") else pidOfR <- get_PID("R")
	
	checkEquals(length(pidOfR)>0,TRUE)
	
	# create the file "stop"
	file.create("stop")
	
	Sys.sleep(7)
	
	# verify the success of the stop
	if (.Platform$OS.type=="windows") newPidOfR <- get_PID("Rterm.exe") else newPidOfR <- get_PID("R")
	checkEquals(newPidOfR,numeric(0))
	
	#remove the stop file
	file.remove("stop")
	file.remove("logFile")
	file.remove("batch.Rout")
	
	# restore the origina working directory
	setwd(tmp)
}



