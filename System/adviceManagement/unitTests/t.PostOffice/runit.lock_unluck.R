# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldFailOnLockNoPostOffice <- function() {
	absolutePath <- file.path(getwd(),"adviceManagement","unitTests","directories")
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	
	checkException(lockMailBox(mailBox,postOffice))
}


test.shouldFailOnLockNoPostOffice <- function() {
	absolutePath <- file.path(getwd(),"adviceManagement","unitTests","directories")
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	
	checkException(lockMailBox(mailBox,postOffice))
	
	unlink(file.path(absolutePath,"postOffice"),recursive=TRUE)
}

test.shouldLockAdvisor <- function() {
	
	
	
	
	
}
