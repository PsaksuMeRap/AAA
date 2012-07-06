# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shoudLock <- function() {
	workingDirectory <- getwd()
	
	# identify a new order
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.lock_unlock")
			
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)

	# identify the messageType
	message <- messageFactory(fileName,directory)
	
	# create the mailBox
	mailBox <- new("MailBox",folderName=message[["portfolioName"]])
	setup(x=mailBox,y=postOffice)
	
	# lock
	ok <- lock(message)
		
	# check
	lockExists <- file.exists(file.path(systemOptions[["homeDir"]],"postOffice",message[["portfolioName"]],"lock"))
	checkEquals(lockExists,TRUE)
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	setwd(workingDirectory)
}


test.shoudUnLock <- function() {
	workingDirectory <- getwd()
	
	# identify a new order
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.lock_unlock")
	
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	# identify the messageType
	message <- messageFactory(fileName,directory)
	
	# create the mailBox
	mailBox <- new("MailBox",folderName=message[["portfolioName"]])
	setup(x=mailBox,y=postOffice)
	
	# create the lock (previously tested)
	ok <- lock(message)
	lockExists <- file.exists(file.path(systemOptions[["homeDir"]],"postOffice",message[["portfolioName"]],"lock"))
	checkEquals(lockExists,TRUE)
	
	# unlock
	ok <- unlock(message)
	lockExists <- file.exists(file.path(systemOptions[["homeDir"]],"postOffice",message[["portfolioName"]],"lock"))
	
	# check
	checkEquals(lockExists,FALSE)
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	setwd(workingDirectory)
}
