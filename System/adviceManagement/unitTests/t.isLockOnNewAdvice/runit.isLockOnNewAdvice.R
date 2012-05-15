# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldRunIsLockOnNewAdvice <- function(message) {
	
	originalWorkingDirectory <- getwd()
	
	create_archive(systemOptions[["homeDir"]])
	
	advisors <- new("Advisors")
	advisors[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="claudio.ortelli@gmail.com")
	
	#create postOffice
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	# copy incoming message
	from <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files","2012-05-09_14-22-24_GhidossiGlobalEquity_newAdvice.csv")
	to <- file.path(systemOptions[["homeDir"]],"postOffice","inbox")
	isOk <- file.copy(from,to)
	
	messageFrom <- advisors[[1]]@name
	
	postOffice@mailBoxes[length(postOffice@mailBoxes)+1] <- messageFrom
	mailbox <- new("MailBox",advisor=advisors[[messageFrom]])
	setup(x=mailbox,y=postOffice)
	
	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files")
	message <- messageFactory(fileName,directory,advisors)
	
	# lock the GhidossiGlobalEquity mailBox
	isOk <- file.create(file.path(systemOptions[["homeDir"]],"postOffice","GhidossiGlobalEquity","lock"))
	# run the test				
	
	result <- isLockOnNewAdvice(message)
	checkEquals(result,0)
	checkEquals(file.exists(file.path(systemOptions[["homeDir"]],"archive","deleted",fileName)),TRUE)
	
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"archive"),recursive=TRUE)
	setwd(originalWorkingDirectory)
	
}
