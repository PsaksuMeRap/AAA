# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldCreateLogger <- function() {
	
	directory <- file.path(systemOptions[["homeDir"]],"log")
	fileName <- create_logger(fileType="newAdvice")
	
	fileExist <- file.exists(fileName)
	
	checkEquals(fileExists,TRUE)
	
	if (fileExist) sink()
	unlink(directory)
}
