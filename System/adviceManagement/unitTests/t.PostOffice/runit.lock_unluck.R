# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldLockFailOnNoPostOffice <- function() {

	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	
	checkException(lockMailBox(mailBox,postOffice))
}


test.shouldLockFailOnNoMailBox <- function() {
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	
	checkException(lockMailBox(mailBox,postOffice))
	
	# remove the directory postOffice
	tmp <- getwd()
	setwd(systemOptions[["homeDir"]])
	unlink("postOffice",recursive=TRUE)
	setwd(tmp)
}

test.shouldFailOnNoPermissions <- function() {
	
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
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
		Sys.chmod(paths=file.path(systemOptions[["homeDir"]],"postOffice","testAdvisor1"), mode = "0555", use_umask=FALSE)
		
		checkException(lockMailBox(mailBox,postOffice))
		# restore previous file permissions
		Sys.chmod(paths=file.path(systemOptions[["homeDir"]],"postOffice","testAdvisor1"), use_umask=TRUE)
	}
	
	# restore previous situation
	tmp <- getwd()
	setwd(systemOptions[["homeDir"]])
	unlink("postOffice",recursive=TRUE)
	setwd(tmp)
}


test.shouldFailOnExistingLock <- function() {
	
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	setup(x=mailBox,y=postOffice)
	
	# create the lock and try to relock the directory
	isCreated <- file.create(file.path(systemOptions[["homeDir"]],"postOffice","testAdvisor1","lock"),showWarnings=FALSE)
	checkException(lockMailBox(mailBox,postOffice))
	
	# restore previous situation
	tmp <- getwd()
	setwd(systemOptions[["homeDir"]])
	unlink("postOffice",recursive=TRUE)
	setwd(tmp)
	
}