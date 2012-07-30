# TODO: Add comment
# 
# Author: Claudio
###############################################################################

test.shouldProcessConfirmationNoLock <- function() {
	create_archive(systemOptions[["homeDir"]])
	
	#create postOffice
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	# define the file
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_confirmation.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.confirmationNoLock")
	
	# copy incoming message	
	from <- file.path(directory,fileName)
	to <- file.path(systemOptions[["homeDir"]],"postOffice","inbox")
	isOk <- file.copy(from,to)
	
	message <- messageFactory(fileName,directory)	
	
	mailbox <- new("MailBox",name=message[["portfolioName"]],
			folderName=message[["portfolioName"]])
	setup(x=mailbox,y=postOffice)
		
	# run the test				
	result <- confirmationNoLock(message)
	checkEquals(result,0)
	checkEquals(file.exists(file.path(systemOptions[["homeDir"]],"archive","deleted",fileName)),TRUE)
	
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"archive"),recursive=TRUE)
	
}

