# TODO: Add comment
# 
# Author: Claudio
###############################################################################

test.shouldProcessConfirmationWithLock <- function() {
	
	# copy the data directory
	from <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data")
	to <- file.path(sys[["homeDir"]])
	isOk <- file.copy(from,to,recursive=TRUE)
	
	create_archive(sys[["homeDir"]])
	
	#create postOffice
	postOffice <- new("PostOffice",absolutePath=sys[["homeDir"]])
	setup(postOffice)
	
	# define the file
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_confirmation.csv"
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.confirmationWithLock")
	
	# copy incoming message	
	from <- file.path(directory,fileName)
	to <- file.path(sys[["homeDir"]],"postOffice","inbox")
	isOk <- file.copy(from,to)
	
	message <- messageFactory(fileName,directory)	
	
	mailbox <- new("MailBox",name=message[["portfolioName"]],
			folderName=message[["portfolioName"]])
	setup(x=mailbox,y=postOffice)
	
	# lock the globalEquity mailBox
	isOk <- file.create(file.path(sys[["homeDir"]],"postOffice","globalEquity","lock"))
	
	# run the test				
	allPID <- confirmationWithLock(message)
	checkEquals(length(allPID)>0,TRUE)
	
	Sys.sleep(15)
	
	checkEquals(file.exists(file.path(sys[["homeDir"]],"archive","processed","rejected","2012-05-09_14-22-24_Ortelli_globalEquity_postComplianceResult_0.zip")),TRUE)
	
	unlink(file.path(sys[["homeDir"]],"data"),recursive=TRUE)
	unlink(file.path(sys[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(sys[["homeDir"]],"archive"),recursive=TRUE)
	unlink(file.path(sys[["homeDir"]],"log"),recursive=TRUE)

}

