# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldWatchDirectory <- function() {
	
	isDirectoryEmpty <- function(
			directoryToWatch=file.path(".","adviceManagement","unitTests","directories"),
			lockId="openCapital") {
		isEmpty <- TRUE
		existingFiles <- list.files(path=directoryToWatch)
		if (!identical(existingFiles,character(0))) isEmpty <- FALSE else print("Empty dir")
		return(isEmpty)
	}


	while(isDirectoryEmpty()) {
		Sys.sleep(5)
	}
	
	
}

