# TODO: Add comment
# 
# Author: ortellic
###############################################################################


get_PID <- function(imageName) {
	if(.Platform$OS.type=="windows") {
		if (missing(imageName)) imageName <- "R.exe"
		output <- system(paste("tasklist.exe /fi \"imagename eq",imageName,"\""),intern=TRUE)
		if (output[[1]]=="INFO: No tasks are running which match the specified criteria.") {
			return(numeric(0))
		} else {
			output <- output[-(1:3)]
			extract_PID <- function(string) {
				PID <- as.integer(strsplit(string,split="\\s+")[[1]][2])
				return(PID)
			}
			PIDs <- sapply(output,extract_PID,USE.NAMES=FALSE)
			return(PIDs)
		}
	} else {
		if (missing(imageName)) imageName <- "R"

		PIDs <- system(paste("pidof",imageName),intern=TRUE,wait=TRUE)
		
		if (length(PIDs)==0) return(numeric(0))
		PIDs <- as.integer(unlist(strsplit(PIDs,split=" ")))
		
		return(PIDs) 
		
	}
	
}

getNew_PIDs <- function(PIDs,existing_PIDs) {

	if (length(PIDs)==0) return(numeric(0))
	existing <- is.element(PIDs,existing_PIDs) 
	return(PIDs[!existing])
}
