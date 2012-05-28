# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldSaveAnObject <- function() {
	
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement",
			"unitTests","t.saveLastObject")
	fileName <- "portfolio.RData"

	object <- "Test"
	
	save("object",file=file.path(directory,fileName))
	
	saveLastObject(object,fileName,directory)
	
	# check the existence of object in directory directory
	checkEquals(file.exists(file.path(directory,fileName)),TRUE)
	checkEquals(length(list.files(path=directory,pattern="portfolio.RData$")),2)
	
	isOk <- file.remove(file.path(directory,fileName))
	isOk <- file.remove(list.files(path=directory,pattern=".RData$",full.names=TRUE))
}
