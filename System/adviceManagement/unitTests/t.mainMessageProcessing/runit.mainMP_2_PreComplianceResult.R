# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldProcessPreComplianceResult_1 <- function() {
	# this method assumes that the preComplianceResult zip file has been 
	# 1) moved from postOffice/portfolio/pending to postOffice/incoming folder
	# 2) copied to the archive/processed/accepted or rejected folder
	
	# create the archive
	create_archive(systemOptions[["homeDir"]])
	
	# identify the preCompliance result file to copy
	fileName <- "2012-06-19_14-27-47_Ortelli_globalEconomy_preComplianceResult_1.zip"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.mainMessageProcessing")
	fullFileNameFrom <- file.path(directory,fileName)
	
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)

	mailBox <- new("MailBox",folderName="globalEconomy")
	setup(x=mailBox,y=postOffice)
	
	# create file
	fullFileNameTo <- file.path(systemOptions[["homeDir"]],"postOffice","inbox",fileName) 
	isOk <- file.copy(fullFileNameFrom,fullFileNameTo)
	fullFileNameTo <- file.path(systemOptions[["homeDir"]],"archive","processed","accepted",fileName) 
	isOk <- file.copy(fullFileNameFrom,fullFileNameTo)
	
	# identify the messageType
	directory <- file.path(systemOptions[["homeDir"]],"postOffice","inbox")
	message <- messageFactory(fileName,directory)

	# lock
	ok <- lock(message)
	
	result <- mainMessageProcessing(message)
	
	checkEquals(result,1)
	
	directory <- file.path(systemOptions[["homeDir"]],"postOffice","inbox")
	exists <- file.exists(file.path(directory,message[["fileName"]]))
	checkEquals(exists,FALSE)
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"archive"),recursive=TRUE)	
	
}


test.shouldProcessPreComplianceResultMessage_0 <- function() {
	# this method assumes that the preComplianceResult zip file has been 
	# 1) moved from postOffice/portfolio/pending to postOffice/incoming folder
	# 2) copied to the archive/processed/accepted or rejected folder
	
	# create the archive
	create_archive(systemOptions[["homeDir"]])
	
	# identify the preCompliance result file to copy
	fileName <- "2012-06-19_14-27-47_Ortelli_globalEconomy_preComplianceResult_0.zip"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.mainMessageProcessing")
	fullFileNameFrom <- file.path(directory,fileName)
	
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	mailBox <- new("MailBox",folderName="globalEconomy")
	setup(x=mailBox,y=postOffice)
	
	# create file
	fullFileNameTo <- file.path(systemOptions[["homeDir"]],"postOffice","inbox",fileName) 
	isOk <- file.copy(fullFileNameFrom,fullFileNameTo)
	fullFileNameTo <- file.path(systemOptions[["homeDir"]],"archive","processed","rejected",fileName) 
	isOk <- file.copy(fullFileNameFrom,fullFileNameTo)
	
	# identify the messageType
	directory <- file.path(systemOptions[["homeDir"]],"postOffice","inbox")
	message <- messageFactory(fileName,directory)
	
	# lock
	ok <- lock(message)
	
	result <- mainMessageProcessing(message)
	checkEquals(result,0)
	
	directory <- file.path(systemOptions[["homeDir"]],"postOffice","inbox")
	exists <- file.exists(file.path(directory,message[["fileName"]]))
	checkEquals(exists,FALSE)
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"archive"),recursive=TRUE)	
	
}
