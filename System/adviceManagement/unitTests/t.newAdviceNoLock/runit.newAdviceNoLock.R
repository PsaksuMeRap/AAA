# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldNewAdviceWithoutLock <- function() {
	
	# copy the data directory
	from <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data")
	to <- file.path(systemOptions[["homeDir"]])
	isOk <- file.copy(from,to,recursive=TRUE)

	
	# create the archive
	create_archive(systemOptions[["homeDir"]])
	
	#create postOffice
	postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
	setup(postOffice)
	
	# copy the simulated incoming message in the inbox
	from <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.newAdviceNoLock","2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv")
	to <- file.path(systemOptions[["homeDir"]],"postOffice","inbox")
	file.copy(from,to)

	csvTradesFileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.newAdviceNoLock")
	
	message <- messageFactory(csvTradesFileName,directory)
	
	mailbox <- new("MailBox",folderName=message[["portfolioName"]])
	setup(x=mailbox,y=postOffice)
	
	
	# run the test				
	allPID <- newAdviceNoLock(message)
	checkEquals(length(allPID)>0,TRUE)
	
	Sys.sleep(15)
	
	# check the existence of a rejected file in archive/processed/rejected
	rejectedFiles <- list.files(file.path(systemOptions[["homeDir"]],"archive","processed","rejected"))
	checkEquals(length(rejectedFiles)>0,TRUE)
	
	unlink(file.path(systemOptions[["homeDir"]],"data"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"archive"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"log"),recursive=TRUE)
	
}
