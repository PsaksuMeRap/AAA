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

test.shouldFailOnNoPermissions <- function() {
	
	absolutePath <- file.path(getwd(),"adviceManagement","unitTests","directories")
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	setup(x=mailBox,y=postOffice)
	
	# change the permissions 
	Sys.chmod(paths=file.path(absolutePath,"postOffice","testAdvisor1"), mode = "0222", use_umask=TRUE)
	checkException(lockMailBox(mailBox,postOffice))
		
	# restore previous situation
	Sys.chmod(paths=file.path(absolutePath,"postOffice","testAdvisor1"), mode = "0775", use_umask=TRUE)
	unlink(file.path(absolutePath,"postOffice"),recursive=TRUE)
	
}


test.shouldFailOnNoPermissions <- function() {
	
	absolutePath <- file.path(getwd(),"adviceManagement","unitTests","directories")
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	setup(x=mailBox,y=postOffice)
	
	# change the permissions
	lockMailBox(mailBox,postOffice)
	fileExists <- file.exists(file.path(absolutePath,"postOffice","testAdvisor1","lock"))
	checkEquals(fileExists,TRUE)
	
	# restore previous situation
	unlink(file.path(absolutePath,"postOffice"),recursive=TRUE)
	
}