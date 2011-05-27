# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.isDirectory <- function() {
	
	initial_dir <- getwd()
	setwd("/home/claudio/eclipse/AAA/libraries/unitTests/data")
	filePath="a1.R"
	checkEquals(isDirectory(filePath),FALSE)

	filePath="testEmptyDirectory"

	checkEquals(isDirectory(filePath),TRUE)
	setwd(initial_dir)		
}


test.shouldGetListOfFiles <- function() {
	
	initial_dir <- getwd()
	setwd("/home/claudio/eclipse/AAA/libraries/unitTests/data")

	#	if (any(is.file)) mapply(file.rename,from[is.file],to=c("a11.R","a22.R"))	

	checkEquals(listOfFiles(),c("a1.R","a2.R"))
	setwd(initial_dir)
	
}
