# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldRunIsLockOnNewAdvice <- function(message) {
	
	#originalWorkingDirectory <- getwd()
	
	create_archive(systemOptions[["homeDir"]])
	
	#create postOffice
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	# define the file
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.newAdviceWithLock")
	
	# copy incoming message	
	from <- file.path(directory,fileName)
	to <- file.path(systemOptions[["homeDir"]],"postOffice","inbox")
	isOk <- file.copy(from,to)
	
	message <- messageFactory(fileName,directory)	
	
	messageFrom <- advisors[[1]]@name
	
	mailbox <- new("MailBox",name=message[["portfolioName"]],
			folderName=message[["portfolioName"]])
	setup(x=mailbox,y=postOffice)
		
	# lock the globalEquity mailBox
	isOk <- file.create(file.path(systemOptions[["homeDir"]],"postOffice","globalEquity","lock"))
	
	# run the test				
	result <- isLockOnNewAdvice(message)
	checkEquals(result,0)
	checkEquals(file.exists(file.path(systemOptions[["homeDir"]],"archive","deleted",fileName)),TRUE)
	
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"archive"),recursive=TRUE)
	#setwd(originalWorkingDirectory)
	
}
