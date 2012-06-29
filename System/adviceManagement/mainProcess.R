
# library("tcltk")
# library("stringr")

# if(.Platform$OS.type=="windows") {
#	library("rJava")
# 	library("Rbbg")
# }

source("./adviceManagement/lib/initialSetup.R")

# set the sleeping time in seconds
sleepTime <- 10

# set the homeDir variable
homeDir <- systemOptions[["homeDir"]]

# create the log file
logFileName <- paste(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"),"riskman_log.txt",sep="_")
invisible(create_logger(fileName=logFileName))

# create the PostOffice
logger("Initializing PostOffice ...")
postOffice <- new("PostOffice",absolutePath=homeDir)
setup(postOffice)
loggerDone()

# setup the archive
logger("Initializing archive ...")
create_archive(systemOptions[["homeDir"]])
loggerDone()

# setup the data directory
logger("Initializing the data directory ...")
from <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data")
to <- file.path(systemOptions[["homeDir"]])
isOk <- file.copy(from,to,recursive=TRUE)
loggerDone()

# check that no R processes are running and identify the PID of this program
PID <- get_PID(imageName="eclipse.exe")
#PID <- get_PID(imageName="Rterm.exe")
PID <- get_PID(imageName="Rgui.exe")
if (length(PID)>1) {
	msg <- "Please close all running R/Rterm processes before starting a new one!"
	ok <- tkmessageBox(message=msg,icon="warning",type="ok")
	logger("Multiple Rterm processes detected. The program will close.")
	# remove the postOffice
	logger("Removing postOffice ...")
	unlink("postOffice",recursive=TRUE)
	loggerDone()
	quit(save="no")
}

# create the data.frame with the activeOrders
activeOrders <- data.frame(name="main",startTime=Sys.time(),fileName="",orderName="",PID=PID)

# start monitoring input directory

T <- Sys.time()+500
while(Sys.time()<T) {
	logger("Looking for new files ...")
	existingFiles <- list.files(path=file.path(homeDir,"postOffice","inbox"))
	loggerDone()
	
	if (length(existingFiles>0)) {
		# sort the files
		existingFiles <- sort(existingFiles)
		
		# log the arrival of the new files
		msg <- "The following files have arrived:\n"
		for (fileName in existingFiles) {
			msg <- paste(msg,"- ",fileName,"\n",sep="")
		}
		logger(msg,noOk="\n")
		
		for (fileName in existingFiles) {
			#identify messageTye between: newAdvice,adviceConfirmation,preComplianceResult,postComplianceResult
			directory <- file.path(systemOptions[["homeDir"]],"postOffice","inbox")
			message <- messageFactory(fileName,directory)
			
			# identify the advisor (filename="2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv")
			messageFrom <- message[["from"]]
			
			# identify the involved portfolio
			portfolioName <- message[["portfolioName"]]
			
			if (is.element(message[["messageType"]],c("newAdvice","adviceConfirmation"))) {
				# check if the corresponding mailBox is available
				mailBoxExists <- is.element(portfolioName, dir(file.path(postOffice@absolutePath,"postOffice")))
				
				if (!mailBoxExists) {
					# if the mailBox does not exists create the mailbox
					logger(paste("Creating mailBox for portfolio",portfolioName,"from",messageFrom,"..."))
					mailbox <- new("MailBox",folderName=portfolioName)
					setup(x=mailbox,y=postOffice)
					loggerDone()
				}
				
				# is the mailbox locked? 
				isLocked <- file.exists(file.path(homeDir,"postOffice",portfolioName,"lock"))
				
				# if locked send an e-mail and move the file into the removed folder. Log all actions
				if (isLocked) {
					isOk <- isLockOnNewAdvice(message)
				} else {
					# if the file is not locked move the advice in the mailbox_xxx/pending and start processing
					PID <- noLockOnNewAdvice(message)
				}
			}
			
			if (is.element(message[["messageType"]],c("preComplianceResult","postComplianceResult"))) {
				postProcessing <- function(fileName) {
					# is the pre-compliance ok?
					if (isPrecomplianceOk) {
						lockOnNewOrder(fileName)
						# send e-mail ok
						
						# cp result in a specified folder for user
						# mv from outbox and archive the result
						
						
						# log actions
					} else {
						# send e-mail with attachment & warning
						# log it
						
						
					}
				}
				
				for (fileName in processedFiles) {
					postProcessing(fileName)
					
				}
			}
		} # end for (fileName in existingFiles)
		
	} # end if (length(existingFiles>0))
	
	logger(paste("Sleep",sleepTime,"seconds ..."))
	Sys.sleep(sleepTime)
	loggerDone()
}


