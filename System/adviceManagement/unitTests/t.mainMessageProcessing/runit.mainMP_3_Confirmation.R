# TODO: Add comment
# 
# Author: Claudio
###############################################################################

test.shouldProcessConfirmationMessageWithoutLock <- function() {

	# copy the data directory
	from <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data")
	to <- file.path(systemOptions[["homeDir"]])
	isOk <- file.copy(from,to,recursive=TRUE)
	
	create_archive(systemOptions[["homeDir"]])
	
	#create postOffice
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	# define the file
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_confirmation.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.mainMessageProcessing")
	
	# copy incoming message	
	from <- file.path(directory,fileName)
	to <- file.path(systemOptions[["homeDir"]],"postOffice","inbox")
	isOk <- file.copy(from,to)
	
	message <- messageFactory(fileName,directory)	
	
	mailbox <- new("MailBox",name=message[["portfolioName"]],
			folderName=message[["portfolioName"]])
	setup(x=mailbox,y=postOffice)
		
	# run the test				
	mainMessageProcessing(message)
		
	checkEquals(file.exists(file.path(systemOptions[["homeDir"]],"archive","deleted","2012-05-09_14-22-24_Ortelli_globalEquity_confirmation.csv")),TRUE)
	
	unlink(file.path(systemOptions[["homeDir"]],"data"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"archive"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"log"),recursive=TRUE)
	
}
