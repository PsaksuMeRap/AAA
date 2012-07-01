# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldProcessPreComplianceResultMessageNo <- function() {
	
	# create the archive
	create_archive(systemOptions[["homeDir"]])
	
	# identify a new order
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_preComplianceResult_no.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.messageProcessing")
	
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)

	# identify the messageType
	message <- messageFactory(fileName,directory,advisors)	

	# create the mailBox
	mailBox <- new("MailBox",folderName=message[["portfolioName"]])
	setup(x=mailBox,y=postOffice)
	

	# create the newAdviceFileName who generated fileName
	newAdviceFileName <- paste(paste(getMessageDate_time_from(message),"newAdvice",sep="_"),message[["fileExtension"]],sep=".")
	
	# lock
	ok <- lock(message)
	
	# create file
	ok <- file.create(file.path(systemOptions[["homeDir"]],"postOffice",message[["portfolioName"]],"pending",message[["fileName"]])) 
	ok <- file.create(file.path(systemOptions[["homeDir"]],"postOffice",message[["portfolioName"]],"pending",newAdviceFileName)) 
	
	messageProcessing(message)
	exists <- file.exists(file.path(systemOptions[["homeDir"]],"archive","processed","rejected",message[["fileName"]]))
	checkEquals(exists,TRUE)
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"archive"),recursive=TRUE)	
	
}


test.shouldProcessPreComplianceResultMessageOk <- function() {
	
	# create the archive
	create_archive(systemOptions[["homeDir"]])
	
	# identify a new order
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_preComplianceResult_ok.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.messageProcessing")
	
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	# identify the messageType
	message <- messageFactory(fileName,directory,advisors)
	
	# create the mailBox
	mailBox <- new("MailBox",folderName=message[["portfolioName"]])
	setup(x=mailBox,y=postOffice)
	
	# lock
	ok <- lock(message)
	
	ok <- file.create(file.path(systemOptions[["homeDir"]],"postOffice",message[["portfolioName"]],"pending",message[["fileName"]])) 
	
	messageProcessing(message)
	exists <- file.exists(file.path(systemOptions[["homeDir"]],"archive","processed","rejected",message[["fileName"]]))
	checkEquals(exists,FALSE)
	exists <- file.exists(file.path(systemOptions[["homeDir"]],"postOffice",message[["portfolioName"]],"pending",message[["fileName"]]))
	checkEquals(exists,TRUE)
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"archive"),recursive=TRUE)	
	
}
