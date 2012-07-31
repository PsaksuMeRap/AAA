# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldNewAdviceWithLock <- function(message) {
	
	create_archive(sys[["homeDir"]])
	
	#create postOffice
	postOffice <- new("PostOffice",absolutePath=sys[["homeDir"]])
	setup(postOffice)
	
	# define the file
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv"
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.newAdviceWithLock")
	
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
	result <- newAdviceWithLock(message)
	checkEquals(result,0)
	checkEquals(file.exists(file.path(sys[["homeDir"]],"archive","deleted",fileName)),TRUE)
	
	unlink(file.path(sys[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(sys[["homeDir"]],"archive"),recursive=TRUE)

	
}
