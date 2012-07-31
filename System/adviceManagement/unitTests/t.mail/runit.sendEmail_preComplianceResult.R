# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldSendPreComplianceResultNo <- function() {
	
	create_archive(sys[["homeDir"]])
	
	# identify a new order
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_preComplianceResult_0.csv"
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.mail")
		
	# create the postOffice
	absolutePath <- sys[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	# identify the messageType
	message <- messageFactory(fileName,directory)	
	
	# create the mailBox
	mailBox <- new("MailBox",folderName=message[["portfolioName"]])
	setup(x=mailBox,y=postOffice)
	
	# create the message on the disk
	ok <- file.create(file.path(sys[["homeDir"]],"archive","processed","rejected",message[["fileName"]])) 
	
	newAdviceFileName <- paste(paste(getMessageDate_time_from(message),"newAdvice","_"),message[["fileExtension"]],sep=".")
	stringMessage <- paste("Your advice '",newAdviceFileName,"' has been rejected because of a negative pre-compliance.\n",sep="")
	subject <- "Advice refused"
	
	result <- sendEmail_preComplianceResult(message)
	checkEquals(result,"Email was sent successfully!")
	
	# clean
	unlink(file.path(sys[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(sys[["homeDir"]],"archive"),recursive=TRUE)
	setwd(sys[["sourceCodeDir"]])
}


test.shouldSendPreComplianceResultOk <- function() {
	
	create_archive(sys[["homeDir"]])
	# identify a new order
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_preComplianceResult_1.csv"
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.mail")
		
	# create the postOffice
	absolutePath <- sys[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	# identify the messageType
	message <- messageFactory(fileName,directory)	
	
	# create the mailBox
	mailBox <- new("MailBox",folderName=message[["portfolioName"]])
	setup(x=mailBox,y=postOffice)

	# create the message on the disk
	ok <- file.create(file.path(sys[["homeDir"]],"archive","processed","accepted",message[["fileName"]])) 
	
	newAdviceFileName <- paste(paste(getMessageDate_time_from(message),"newAdvice","_"),message[["fileExtension"]],sep=".")
	stringMessage <- paste("Your advice '",newAdviceFileName,"' has been accepted for execution.\n",sep="")
	
	result <- sendEmail_preComplianceResult(message) 
	checkEquals(result,"Email was sent successfully!")
	
	# clean
	unlink(file.path(sys[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(sys[["homeDir"]],"archive"),recursive=TRUE)
	setwd(sys[["sourceCodeDir"]])
	
}
