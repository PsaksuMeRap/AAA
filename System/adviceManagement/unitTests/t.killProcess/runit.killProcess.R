# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldKillRunningRbatchProcess <- function() {
	# the test start an R batch process which waits
	# 100 seconds
	# the PID of the process is then identified and
	# the kill command executed
	
	directory <- file.path(getwd(),"unitTests","t.killProcess")
	fileName <- file.path(directory,"stop")
	
	# start the batch R file
	
	# identify the PID
	
	# create the file "stop"
	tmp <- getwd()
	setwd(directory)
	
	file.create()
	setwd(tmp)
	
	# kill the process
	
	# verify the success of the kill
	
	#remove the stop file
}



