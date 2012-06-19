# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldSendPreComplianceResultNo <- function() {
	
	# identify a new order
	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_preComplianceResult_no.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.mail")
	
	# define the adivisors
	advisors <- new("Advisors")
	advisors[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="claudio.ortelli@gmail.com")
	
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	# create the mailBox
	mailBox <- new("MailBox",advisor=advisors[["GhidossiGlobalEquity"]])
	setup(x=mailBox,y=postOffice)
	
	# identify the messageType
	message <- messageFactory(fileName,directory,advisors)
	
	# create the message on the disk
	ok <- file.create(file.path(systemOptions[["homeDir"]],"postOffice",advisors[["GhidossiGlobalEquity"]]@folderName,"pending",message[["fileName"]])) 
	
	newAdviceFileName <- paste(paste(getMessageDate_time_from(message),"newAdvice","_"),message[["fileExtension"]],sep=".")
	stringMessage <- paste("Your advice '",newAdviceFileName,"' has been rejected because of a negative pre-compliance.\n",sep="")
	subject <- "Advice refused"
	
	result <- sendEmail_preComplianceResult(message) 
	checkEquals(result,"Email was sent successfully!")
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)

}


test.shouldSendPreComplianceResultOk <- function() {
	
	# identify a new order
	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_preComplianceResult_ok.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.mail")
	
	# define the adivisors
	advisors <- new("Advisors")
	advisors[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="claudio.ortelli@gmail.com")
	
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	# create the mailBox
	mailBox <- new("MailBox",advisor=advisors[["GhidossiGlobalEquity"]])
	setup(x=mailBox,y=postOffice)
	
	# identify the messageType
	message <- messageFactory(fileName,directory,advisors)
	
	# create the message on the disk
	ok <- file.create(file.path(systemOptions[["homeDir"]],"postOffice",advisors[["GhidossiGlobalEquity"]]@folderName,"pending",message[["fileName"]])) 
	
	newAdviceFileName <- paste(paste(getMessageDate_time_from(message),"newAdvice","_"),message[["fileExtension"]],sep=".")
	stringMessage <- paste("Your advice '",newAdviceFileName,"' has been accepted for execution.\n",sep="")
	
	result <- sendEmail_preComplianceResult(message) 
	checkEquals(result,"Email was sent successfully!")
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	
}
