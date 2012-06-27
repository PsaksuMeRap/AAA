# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldProcessOrder <- function() {
	
	# setup the directories and messages	
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	mailBox <- new("MailBox",folderName=message[["portfolioName"]])
	setup(x=mailBox,y=postOffice)
	
	# copy the file with the new orders
	fileName <- "2012-06-19_14-27-47_Ortelli_globalEconomy_newAdvice.csv"
	from <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.newOrderProcessing",fileName)
	to <- file.path(systemOptions[["homeDir"]],"postOffice","globalEconomy","pending",fileName)
	file.copy(from,to)
	
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
}
