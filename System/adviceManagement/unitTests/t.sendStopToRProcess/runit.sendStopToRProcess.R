# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldKillRunningRbatchProcess <- function() {
	# the test start an R batch process which waits
	# 100 seconds
	# the PID of the process is then identified and
	# the kill command executed
	
	directory <- file.path(getwd(),"unitTests","t.sendStopToRProcess")
	
	# create the file "stop"
	tmp <- getwd()
	setwd(directory)
	
	# start the batch R file
	system("R --no-save < batch.R", wait=FALSE)
	
	# identify the PID
	pidOfR <- system("pidof R",intern=TRUE)
	Sys.sleep(6)
	
	# create the file "stop"
	file.create("stop")
	setwd(tmp)
	
	Sys.sleep(6)
	
	# verify the success of the kill
	newPidOfR <- system("pidof R",intern=FALSE,wait=TRUE)
	
	checkEquals(newPidOfR,1)
	#remove the stop file
	system("rm stop")
}



