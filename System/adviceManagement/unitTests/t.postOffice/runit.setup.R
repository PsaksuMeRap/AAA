# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldSetupPostOffice <- function() {
	homeDir <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=homeDir)
	setup(postOffice)
	
	checkEquals(is.element("postOffice",dir(path=homeDir)),TRUE)
	directories <- dir(file.path(postOffice@absolutePath,"postOffice"))
	checkEquals(is.element("inbox",directories),TRUE)	

	# remove the directory postOffice
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)

}
