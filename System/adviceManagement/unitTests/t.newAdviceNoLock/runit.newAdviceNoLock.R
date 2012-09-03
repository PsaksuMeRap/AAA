# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldNewAdviceWithoutLock <- function() {
	
	# copy the data directory
	from <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data")
	to <- file.path(sys[["homeDir"]])
	isOk <- file.copy(from,to,recursive=TRUE)

	
	# create the archive
	create_archive(sys[["homeDir"]])
	
	#create postOffice
	postOffice <- new("PostOffice",absolutePath=sys[["homeDir"]])
	setup(postOffice)
	
	# copy the simulated incoming message in the inbox
	from <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.newAdviceNoLock","2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv")
	to <- file.path(sys[["homeDir"]],"postOffice","inbox")
	file.copy(from,to)

	csvTradesFileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv"
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.newAdviceNoLock")
	
	message <- messageFactory(csvTradesFileName,directory)
	
	mailbox <- new("MailBox",folderName=message[["portfolioName"]])
	setup(x=mailbox,y=postOffice)
	
	
	# run the test				
	allPID <- newAdviceNoLock(message)
	checkEquals(length(allPID)>0,TRUE)
	
	Sys.sleep(15)
	
	# check the existence of a rejected file in archive/processed/rejected
	acceptedFiles <- list.files(file.path(sys[["homeDir"]],"archive","processed","accepted"))
	checkEquals(length(acceptedFiles)>0,TRUE)
	
	unlink(file.path(sys[["homeDir"]],"data"),recursive=TRUE)
	unlink(file.path(sys[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(sys[["homeDir"]],"archive"),recursive=TRUE)
	unlink(file.path(sys[["homeDir"]],"log"),recursive=TRUE)
	
}
