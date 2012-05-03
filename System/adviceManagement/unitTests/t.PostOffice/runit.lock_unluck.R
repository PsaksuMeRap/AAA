# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldFailOnLockNoPostOffice <- function() {
	absolutePath <- file.path(getwd(),,"unitTests","directories")
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	
	checkException(lockMailBox(mailBox,postOffice))
}


test.shouldFailOnLockNoPostOffice <- function() {
	absolutePath <- file.path(getwd(),"unitTests","directories")
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	
	checkException(lockMailBox(mailBox,postOffice))
	
	# remove the directory postOffice
	tmp <- getwd()
	setwd(absolutePath)
	unlink("postOffice",recursive=TRUE)
	setwd(tmp)
}

test.shouldFailOnNoPermissions <- function() {
	
	absolutePath <- file.path(getwd(),"unitTests","directories")
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	setup(x=mailBox,y=postOffice)
	
	# change the permissions 
	Sys.chmod(paths=file.path(absolutePath,"postOffice","testAdvisor1"), mode = "0200", use_umask=FALSE)
	checkException(lockMailBox(mailBox,postOffice))
		
	# restore previous situation
	Sys.chmod(paths=file.path(absolutePath,"postOffice","testAdvisor1"), mode = "0775", use_umask=TRUE)
	tmp <- getwd()
	setwd(absolutePath)
	unlink("postOffice",recursive=TRUE)
	setwd(tmp)
	
	
}


test.shouldFailOnNoPermissions <- function() {
	
	absolutePath <- file.path(getwd(),"unitTests","directories")
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
	tmp <- getwd()
	setwd(absolutePath)
	unlink("postOffice",recursive=TRUE)
	setwd(tmp)
	
	
}