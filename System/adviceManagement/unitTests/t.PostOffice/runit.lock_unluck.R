# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldLockFailOnNoPostOffice <- function() {
	absolutePath <- file.path(getwd(),"unitTests","directories")
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	
	checkException(lockMailBox(mailBox,postOffice))
}


test.shouldLockFailOnNoMailBox <- function() {
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
		
	# there are problems with windows to set the correct file permissions
	# so that we skip the test in windows
	if (.Platform$OS.type=="windows") {
		checkEquals(TRUE,TRUE)
	} else {
		# change the permissions 
		Sys.chmod(paths=file.path(absolutePath,"postOffice","testAdvisor1"), mode = "0555", use_umask=FALSE)
		
		checkException(lockMailBox(mailBox,postOffice))
		# restore previous file permissions
		Sys.chmod(paths=file.path(absolutePath,"postOffice","testAdvisor1"), use_umask=TRUE)
	}
	
	# restore previous situation
	tmp <- getwd()
	setwd(absolutePath)
	unlink("postOffice",recursive=TRUE)
	setwd(tmp)
}


test.shouldFailOnExistingLock <- function() {
	
	absolutePath <- file.path(getwd(),"unitTests","directories")
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	setup(x=mailBox,y=postOffice)
	
	# create the lock and try to relock the directory
	isCreated <- file.create(file.path(absolutePath,"postOffice","testAdvisor1","lock"),showWarnings=FALSE)
	checkEquals(lockMailBox(mailBox,postOffice),FALSE)
	
	# restore previous situation
	tmp <- getwd()
	setwd(absolutePath)
	unlink("postOffice",recursive=TRUE)
	setwd(tmp)
	
}