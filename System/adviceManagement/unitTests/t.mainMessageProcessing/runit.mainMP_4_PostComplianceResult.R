# TODO: Add comment
# 
# Author: Claudio
###############################################################################

test.shouldProcessPostComplianceResultMessage_0 <- function() {
# this method assumes that the preComplianceResult zip file has been 
# 1) moved from postOffice/portfolio/pending to postOffice/incoming folder
# 2) copied to the archive/processed/accepted or rejected folder

# create the archive
create_archive(sys[["homeDir"]])

# identify the preCompliance result file to copy
fileName <- "2012-06-19_14-27-47_Ortelli_globalEconomy_postComplianceResult_1.zip"
directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.mainMessageProcessing")
fullFileNameFrom <- file.path(directory,fileName)

# create the postOffice
absolutePath <- sys[["homeDir"]]
postOffice <- new("PostOffice",absolutePath=absolutePath)
setup(postOffice)

mailBox <- new("MailBox",folderName="globalEconomy")
setup(x=mailBox,y=postOffice)

# create file
fullFileNameTo <- file.path(sys[["homeDir"]],"postOffice","inbox",fileName) 
isOk <- file.copy(fullFileNameFrom,fullFileNameTo)
fullFileNameTo <- file.path(sys[["homeDir"]],"archive","processed","accepted",fileName) 
isOk <- file.copy(fullFileNameFrom,fullFileNameTo)

# identify the messageType
directory <- file.path(sys[["homeDir"]],"postOffice","inbox")
message <- messageFactory(fileName,directory)

# lock
ok <- lock(message)

result <- mainMessageProcessing(message)
checkEquals(result,1)

directory <- file.path(sys[["homeDir"]],"postOffice","inbox")
exists <- file.exists(file.path(directory,message[["fileName"]]))
checkEquals(exists,FALSE)

# check that the lock is removed
exists <- file.exists(file.path(sys[["homeDir"]],"postOffice",message[["portfolioName"]],"lock"))
checkEquals(exists,FALSE)
	
# clean
unlink(file.path(sys[["homeDir"]],"postOffice"),recursive=TRUE)
unlink(file.path(sys[["homeDir"]],"archive"),recursive=TRUE)	

}


test.shouldProcessPostComplianceResultMessage_0 <- function() {
	# this method assumes that the preComplianceResult zip file has been 
	# 1) moved from postOffice/portfolio/pending to postOffice/incoming folder
	# 2) copied to the archive/processed/accepted or rejected folder
	
	# create the archive
	create_archive(sys[["homeDir"]])
	
	# identify the preCompliance result file to copy
	fileName <- "2012-06-19_14-27-47_Ortelli_globalEconomy_postComplianceResult_0.zip"
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.mainMessageProcessing")
	fullFileNameFrom <- file.path(directory,fileName)
	
	# create the postOffice
	absolutePath <- sys[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	mailBox <- new("MailBox",folderName="globalEconomy")
	setup(x=mailBox,y=postOffice)
	
	# create file
	fullFileNameTo <- file.path(sys[["homeDir"]],"postOffice","inbox",fileName) 
	isOk <- file.copy(fullFileNameFrom,fullFileNameTo)
	fullFileNameTo <- file.path(sys[["homeDir"]],"archive","processed","rejected",fileName) 
	isOk <- file.copy(fullFileNameFrom,fullFileNameTo)
	
	# identify the messageType
	directory <- file.path(sys[["homeDir"]],"postOffice","inbox")
	message <- messageFactory(fileName,directory)
	
	# lock
	ok <- lock(message)
	
	result <- mainMessageProcessing(message)
	checkEquals(result,0)
	
	directory <- file.path(sys[["homeDir"]],"postOffice","inbox")
	exists <- file.exists(file.path(directory,message[["fileName"]]))
	checkEquals(exists,FALSE)
	
	# check that the lock is removed
	exists <- file.exists(file.path(sys[["homeDir"]],"postOffice",message[["portfolioName"]],"lock"))
	checkEquals(exists,FALSE)
	
	# clean
	unlink(file.path(sys[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(sys[["homeDir"]],"archive"),recursive=TRUE)	
	
}
