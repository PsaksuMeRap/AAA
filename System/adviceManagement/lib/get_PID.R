# TODO: Add comment
# 
# Author: ortellic
###############################################################################


get_PID <- function(imageName) {
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
	
}

getNew_PID <- function(PIDs,existing_PIDs) {
	browser()
	if (length(PIDs)==0) return(numeric(0))
	existing <- is.element(PIDs,existing_PIDs) 
	return(PIDs[!existing])
}