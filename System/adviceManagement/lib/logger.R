# TODO: Add comment
# 
# Author: Claudio
###############################################################################

create_logger <- function(fileName,directory=file.path(sys[["homeDir"]],"log")) {

	# create the log file
	if (!file.exists(directory)) dir.create(directory,recursive=TRUE)

	# dateTimeLogName <- paste("log_",fileType,"_",format(Sys.time(),format="%Y-%m-%d_%H-%M-%S"),".txt",sep="")

	nbChar <- nchar(fileName)
	if (nbChar>3) fileExtention <- substr(fileName,nbChar-3,nbChar) else fileExtention <- ""
	
	if (fileExtention==".csv") {
		logFileName <- paste(substr(fileName,1,nbChar-4),"_log.txt",sep="")
		logFileName <- file.path(directory,logFileName)
	} else {
		logFileName <- file.path(directory,fileName)
	}
	
	sink(file=logFileName)
	return(logFileName)
}


logger <- function(message,noOk="") {
	cat(paste("\n",Sys.time(),": ",message,noOk,sep=""))
}

loggerDone <- function(x=" [ok]\n") {
	cat(x)
}
