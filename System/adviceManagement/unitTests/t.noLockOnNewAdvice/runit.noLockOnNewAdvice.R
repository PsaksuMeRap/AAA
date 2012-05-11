# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldNoLockOnNewAdvice <- function() {
	
	originalWorkingDirectory <- getwd()
	
	advisors <- new("Advisors")
	advisors[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="claudio.ortelli@gmail.com")
	
	#create postOffice
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	# copy the simulated incoming message 
	from <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files","2012-05-09_14-22-24_GhidossiGlobalEquity_newAdvice.csv")
	to <- file.path(systemOptions[["homeDir"]],"postOffice","inbox")
	file.copy(from,to)
	
	messageFrom <- advisors[[1]]@name
	
	postOffice@mailBoxes[length(postOffice@mailBoxes)+1] <- messageFrom
	mailbox <- new("MailBox",advisor=advisors[[messageFrom]])
	setup(x=mailbox,y=postOffice)

	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_newAdvice.csv"
	message <- messageFactory(fileName,advisors)
	
	# run the test				
	allPID <- noLockOnNewAdvice(message)
	checkEquals(length(allPID)>0,TRUE)
	
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	
	setwd(originalWorkingDirectory)
	
}
