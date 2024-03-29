# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldCreateLogger <- function() {
	directory <- file.path(sys[["homeDir"]],"log")
	fileName <- create_logger(fileName="newAdvice.csv")
	
	fileExists <- file.exists(fileName)
	
	checkEquals(fileExists,TRUE)
	
	if (fileExists) sink()
	unlink(directory,recursive=TRUE)
}

test.shouldCreateLoggerWithLongName <- function() {
	directory <- file.path(sys[["homeDir"]],"log")
	
	inputFileName <- "2012-07-01_11-01-07_processing_2012-06-19_14-27-47_Ortelli_globalEconomy_newAdvice.csv"
	fileName <- create_logger(fileName=inputFileName)
	
	fileExists <- file.exists(fileName)
	
	checkEquals(fileExists,TRUE)
	
	if (fileExists) sink()
	unlink(directory,recursive=TRUE)
}