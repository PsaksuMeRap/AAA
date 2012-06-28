# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldProcessOrder <- function() {
	# copy the data directory
	from <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data")
	to <- file.path(systemOptions[["homeDir"]])
	isOk <- file.copy(from,to,recursive=TRUE)
	
	# setup the directories and messages	
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	mailBox <- new("MailBox",folderName="globalEconomy")
	setup(x=mailBox,y=postOffice)
	
	# copy the file with the new orders
	fileName <- "2012-06-19_14-27-47_Ortelli_globalEconomy_newAdvice.csv"
	from <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.newOrderProcessing",fileName)
	to <- file.path(systemOptions[["homeDir"]],"postOffice","globalEconomy","pending",fileName)
	isOk <- file.copy(from,to)
	
	test <- function() {
		source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","newOrderProcessing.R"))
	}
	
	unlink(file.path(systemOptions[["homeDir"]],"data"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
}
