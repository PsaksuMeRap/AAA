
# create the log file
source(file.path(sourceCodeDir,"adviceManagement","lib","logger.R"))
logFileName <- paste(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"),"riskman_log.txt",sep="_")
invisible(create_logger(fileName=logFileName,directory=file.path(homeDir,"log")))
logger("Logger successfully created.\n")

# source the code
logger("Starting initialSetup ...")
source(file.path(sourceCodeDir,"adviceManagement","lib","initialSetup.R"))
loggerDone()

# set the sleeping time in seconds
sleepTime <- 10

# create the PostOffice
logger("Initializing PostOffice ...")
postOffice <- new("PostOffice",absolutePath=systemOptions[["homeDir"]])
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
#PID <- get_PID(imageName="eclipse.exe")
#PID <- get_PID(imageName="Rterm.exe")
#PID <- get_PID(imageName="Rgui.exe")
PID <- get_PID(imageName="eclipse")
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
	existingFiles <- list.files(path=file.path(systemOptions[["homeDir"]],"postOffice","inbox"))
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
			logger(paste("Construct and identify message",fileName,"..."))
			message <- messageFactory(fileName,directory)
			loggerDone()
			
			# process the message
			logger(paste("Process message",fileName))
			mainMessageProcessing(message)
			
		} # end for (fileName in existingFiles)
		
	} # end if (length(existingFiles>0))
	
	logger(paste("Sleep",sleepTime,"seconds ..."))
	Sys.sleep(sleepTime)
	loggerDone()
}


