# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldCreateLogger <- function() {
	
	directory <- file.path(systemOptions[["homeDir"]],"log")
	fileName <- create_logger(fileType="newAdvice")
	
	fileExists <- file.exists(fileName)
	
	checkEquals(fileExists,TRUE)
	
	if (fileExists) sink()
	unlink(directory,recursive=TRUE)
}
