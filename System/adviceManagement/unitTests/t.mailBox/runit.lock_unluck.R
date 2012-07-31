# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldLockFailOnNoPostOffice <- function() {

	postOffice <- new("PostOffice",absolutePath=sys[["homeDir"]])
	mailBox <- new("MailBox",name="Test",folderName="blabla")
	
	checkException(lockMailBox(mailBox,postOffice))
}


test.shouldLockFailOnNoMailBox <- function() {
	postOffice <- new("PostOffice",absolutePath=sys[["homeDir"]])
	setup(postOffice)
	
	mailBox <- new("MailBox",name="Test",folderName="blabla")
	checkException(lockMailBox(mailBox,postOffice))
	
	# remove the directory postOffice
	unlink(file.path(sys[["homeDir"]],"postOffice"),recursive=TRUE)
}

test.shouldFailOnNoPermissions <- function() {
	
	postOffice <- new("PostOffice",absolutePath=sys[["homeDir"]])
	setup(postOffice)
	
	mailBox <- new("MailBox",name="Test",folderName="blabla")
	setup(x=mailBox,y=postOffice)
		
	# there are problems with windows to set the correct file permissions
	# so that we skip the test in windows
	if (.Platform$OS.type=="windows") {
		checkEquals(TRUE,TRUE)
	} else {
		# change the permissions 
		Sys.chmod(paths=file.path(sys[["homeDir"]],"postOffice","blabla"), mode = "0555", use_umask=FALSE)
		
		checkException(lockMailBox(mailBox,postOffice))
		# restore previous file permissions
		Sys.chmod(paths=file.path(sys[["homeDir"]],"postOffice","blabla"), use_umask=TRUE)
	}
	
	unlink(file.path(sys[["homeDir"]],"postOffice"),recursive=TRUE)
}


test.shouldFailOnExistingLock <- function() {
	
	postOffice <- new("PostOffice",absolutePath=sys[["homeDir"]])
	setup(postOffice)
	
	mailBox <- new("MailBox",name="Test",folderName="blabla")
	setup(x=mailBox,y=postOffice)
	
	# create the lock and try to relock the directory
	isCreated <- file.create(file.path(sys[["homeDir"]],"postOffice","blabla","lock"),showWarnings=FALSE)
	checkEquals(lockMailBox(mailBox,postOffice),!isCreated)
	
	unlink(file.path(sys[["homeDir"]],"postOffice"),recursive=TRUE)
}