# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldProcessOrder <- function() {
	# setup the directories and messages
	
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	advisor <-  new("Advisor",name="Ghidossi Reto",folderName="GhidossiGlobalEconomy",email="claudio.ortelli@gmail.com")
	mailBox <- new("MailBox",advisor=advisor)
	setup(x=mailBox,y=postOffice)
	
	# copy the file with the new orders
	fileName <- "2012-06-19_14-27-47_GhidossiGlobalEconomy_newAdvice.csv"
	from <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.newOrderProcessing",fileName)
	to <- file.path(systemOptions[["homeDir"]],"postOffice","GhidossiGlobalEconomy","pending",fileName)
	file.copy(from,to)
	
	
	
	unlink("postOffice",recursive=TRUE)
}
