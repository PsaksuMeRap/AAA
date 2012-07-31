# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldCreateArchive <- function() {
	

	rootDir <- sys[["homeDir"]]
	create_archive(rootDir)
	
	oldWorkingDirectory <- getwd()
	
	setwd(rootDir)
	
	directories <- list.dirs()
	checkEquals(is.element("./archive",directories),TRUE)
	checkEquals(is.element("./archive/deleted",directories),TRUE)
	checkEquals(is.element("./archive/processed",directories),TRUE)
	checkEquals(is.element("./archive/processed/accepted",directories),TRUE)
	checkEquals(is.element("./archive/processed/rejected",directories),TRUE)
	
	unlink("./archive",recursive=TRUE)
	setwd(oldWorkingDirectory)
}
