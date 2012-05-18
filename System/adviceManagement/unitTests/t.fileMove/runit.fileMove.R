# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.fileMove <- function() {
	
	workingDirectory <- getwd()
	setwd(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.fileMove"))
	
	dir.create("./testDirectory")
	ok <- file.create("./testDirectory/test.txt")
		
	fromDirectory <- file.path(getwd(),"testDirectory")
	toDirectory <- getwd()
	
	ok <- file.move("test.txt",from=fromDirectory,to=toDirectory)
	exists <- file.exists("./testDirectory/test.txt")
	
	checkEquals(exists,FALSE)
	
	unlink("./testDirectory",recursive=TRUE)
	
	setwd(workingDirectory)
}
