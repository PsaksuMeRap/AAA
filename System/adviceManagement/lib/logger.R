# TODO: Add comment
# 
# Author: Claudio
###############################################################################

create_logger <- function(fileType="newAdvice",directory=file.path(systemOptions[["homeDir"]],"log")) {
	
	# create the log file
	if (!file.exists(directory)) dir.create(directory,recursive=TRUE)
	dateTimeLogName <- paste("log_",fileType,"_",format(Sys.time(),format="%Y-%m-%d_%H-%M-%S"),".txt",sep="")
	logFileName <- file.path(directory,dateTimeLogName)
	sink(file=logFileName)
	return(logFileName)
}


logger <- function(message,processName="main") {
	cat(paste(Sys.time()," from ",processName,": ",message,"\n",sep=""))
}

