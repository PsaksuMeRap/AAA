# TODO: Add comment
# 
# Author: Claudio
###############################################################################

create_logger <- function(csvFileName,directory=file.path(systemOptions[["homeDir"]],"log")) {

	# create the log file
	if (!file.exists(directory)) dir.create(directory,recursive=TRUE)

	# dateTimeLogName <- paste("log_",fileType,"_",format(Sys.time(),format="%Y-%m-%d_%H-%M-%S"),".txt",sep="")
	nbChar <- nchar(csvFileName)

	logFileName <- paste(substr(csvFileName,1,nbChar-4),".log",sep="")
	logFileName <- file.path(directory,logFileName)
	
	sink(file=logFileName)
	return(logFileName)
}


logger <- function(message,processName="main") {
	cat(paste("\n",Sys.time()," from ",processName,": ",message,"\n",sep=""))
}

