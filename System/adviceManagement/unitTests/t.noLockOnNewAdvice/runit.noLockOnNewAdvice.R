# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldNoLockOnNewAdvice <- function() {
	
	#create postOffice
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	# copy the simulated incoming message 
	from <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.noLockOnNewAdvice","2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv")
	to <- file.path(systemOptions[["homeDir"]],"postOffice","inbox")
	file.copy(from,to)

	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.noLockOnNewAdvice")
	
	message <- messageFactory(fileName,directory,advisors)
	
	messageFrom <- advisors[[1]]@name
	
	postOffice@mailBoxes[length(postOffice@mailBoxes)+1] <- messageFrom
	mailbox <- new("MailBox",folderName=message[["portfolioName"]])
	setup(x=mailbox,y=postOffice)
	
	
	# run the test				
	allPID <- noLockOnNewAdvice(message)
	checkEquals(length(allPID)>0,TRUE)
	
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	
	
}
