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
	
	# start the batch R file which looks into t.sendStopMessage directory
	# for a stop file
	
	
	# identify the PID
	
	# create the file "stop"
	tmp <- getwd()
	setwd(directory)
	
	file.create()
	setwd(tmp)
	
	# wait 10 sec. and verify the success of the stop
	
	#remove the stop file
}
