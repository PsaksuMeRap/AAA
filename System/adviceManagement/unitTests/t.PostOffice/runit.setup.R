# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldSetupPostOffice <- function() {

	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	checkEquals(is.element("postOffice",dir(path=absolutePath)),TRUE)
	checkEquals(dir(file.path(absolutePath,"postOffice")),c("inbox"))
	
	# remove the directory postOffice
	tmp <- getwd()
	setwd(absolutePath)
	unlink("postOffice",recursive=TRUE)
	setwd(tmp)
}

test.shouldSetupMailBox <- function() {
	
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	mailBox <- new("MailBox",name="Test",folderName="blabla")
	
	setup(x=mailBox,y=postOffice)
	
	directories <- dir(path=file.path(systemOptions[["homeDir"]],"postOffice"))
	checkEquals(is.element(mailBox@folderName,directories),TRUE)
	
	directories <- dir(path=file.path(systemOptions[["homeDir"]],"postOffice",mailBox@folderName))
	checkEquals(is.element("pending",directories),TRUE)
	
	unlink("postOffice",recursive=TRUE)

}