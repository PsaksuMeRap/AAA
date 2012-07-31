# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldSetupMailBox <- function() {
	
	postOffice <- new("PostOffice",absolutePath=sys[["homeDir"]])
	setup(postOffice)
	
	mailBox <- new("MailBox",name="Test",folderName="blabla")
	
	setup(x=mailBox,y=postOffice)
	
	directories <- dir(path=file.path(sys[["homeDir"]],"postOffice"))
	checkEquals(is.element(mailBox@folderName,directories),TRUE)
	
	directories <- dir(path=file.path(sys[["homeDir"]],"postOffice",mailBox@folderName))
	checkEquals(is.element("pending",directories),TRUE)
	
	unlink(file.path(sys[["homeDir"]],"postOffice"),recursive=TRUE)

}