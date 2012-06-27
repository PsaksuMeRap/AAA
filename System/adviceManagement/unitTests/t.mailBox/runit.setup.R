# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldSetupMailBox <- function() {
	
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	mailBox <- new("MailBox",name="Test",folderName="blabla")
	
	setup(x=mailBox,y=postOffice)
	
	directories <- dir(path=file.path(systemOptions[["homeDir"]],"postOffice"))
	checkEquals(is.element(mailBox@folderName,directories),TRUE)
	
	directories <- dir(path=file.path(systemOptions[["homeDir"]],"postOffice",mailBox@folderName))
	checkEquals(is.element("pending",directories),TRUE)
	
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)

}