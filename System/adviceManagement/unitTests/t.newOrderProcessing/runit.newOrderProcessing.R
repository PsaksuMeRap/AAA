# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldProcessOrder <- function() {
	
	# setup the directories and messages	
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	advisors[["Ortelli"]] <- new("Advisor",name="Ortelli Claudio",folderName="Ortelli",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	setup(x=mailBox,y=postOffice)
	
	# copy the file with the new orders
	fileName <- "2012-06-19_14-27-47_Ortelli_globalEconomy_newAdvice.csv"
	from <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.newOrderProcessing",fileName)
	to <- file.path(systemOptions[["homeDir"]],"postOffice","OrtelliGlobalEconomy","pending",fileName)
	file.copy(from,to)
	
	
	
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
}
