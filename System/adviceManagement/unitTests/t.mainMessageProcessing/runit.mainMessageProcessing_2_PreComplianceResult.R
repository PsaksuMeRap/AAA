# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldProcessPreComplianceResult_1 <- function() {
	
	# create the archive
	create_archive(systemOptions[["homeDir"]])
	
	# identify a new order
	fileName <- "2012-06-19_14-27-47_Ortelli_globalEconomy_preComplianceResult_1.zip"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.mainMessageProcessing")
	fullFileNameFrom <- file.path(directory,fileName)
	
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)

	mailBox <- new("MailBox",folderName="globalEconomy")
	setup(x=mailBox,y=postOffice)
	
	# identify the messageType
	message <- messageFactory(fileName,directory)

	# lock
	ok <- lock(message)
	
	# create file
	fullFileNameTo <- file.path(systemOptions[["homeDir"]],"postOffice","inbox",message[["fileName"]]) 
	isOk <- file.copy(fullFileNameFrom,fullFileNameTo)
	fullFileNameTo <- file.path(systemOptions[["homeDir"]],"archive","processed","accepted",message[["fileName"]]) 
	isOk <- file.copy(fullFileNameFrom,fullFileNameTo)
	
	mainMessageProcessing(message)
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
	message <- messageFactory(fileName,directory)
	
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
