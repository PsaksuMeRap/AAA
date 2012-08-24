
# import the code for the creation of the log file
source(file.path(sourceCodeDir,"adviceManagement","lib","logger.R"))

# create the log file
logFileName <- paste(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"),"riskman_log.txt",sep="_")
invisible(create_logger(fileName=logFileName,directory=file.path(homeDir,"log")))
logger("Logger successfully created.\n")

# source the code
logger("Starting initialSetup ...")
source(file.path(sourceCodeDir,"adviceManagement","lib","initialSetup.R"))
loggerDone()

# create the PostOffice
logger("Initializing PostOffice ...")
postOffice <- new("PostOffice",absolutePath=sys[["homeDir"]])
setup(postOffice)
loggerDone()

# setup the archive
logger("Initializing archive ...")
create_archive(sys[["homeDir"]])
loggerDone()

# setup the data directory
logger("Initializing the data directory ...")
from <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data")
to <- file.path(sys[["homeDir"]])
isOk <- file.copy(from,to,recursive=TRUE)
loggerDone()

# start monitoring input directory
dayString <- format(Sys.time(), "%Y-%m-%d")
stopHour <- "17:45:00"
end <- strptime(paste(dayString,stopHour),format="%Y-%m-%d %H:%M:%S")
		
# end <- Sys.time()+360
while(Sys.time()<end) {
	logger("Looking for new files ...")
	existingFiles <- list.files(path=file.path(sys[["homeDir"]],"postOffice","inbox"))
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
		
		# cycle on each file 
		for (fileName in existingFiles) {
			
			# log the processing of the file
			directory <- file.path(sys[["homeDir"]],"postOffice","inbox")
			logger(paste("Construct and identify message",fileName,"..."))
			
			#identify messageTye between: newAdvice,adviceConfirmation,preComplianceResult,postComplianceResult
			message <- messageFactory(fileName,directory)
			loggerDone()
			
			# process the message
			logger(paste("Process message",fileName))			
			mainMessageProcessing(message,postOffice)
			
			# log the end of the processing
			logger(paste("file",fileName,"terminated"))
			
		} # end for (fileName in existingFiles)
		
	} # end if (length(existingFiles>0))
	
	logger(paste("Sleep",sys[["sleepTime"]] ,"seconds ..."))
	Sys.sleep(sys[["sleepTime"]] )
	loggerDone()
}
logger(paste("Sys.time() is larger than",end,"\n\n"))
logger("Closing program")
sink()
# quit(save="no")

#print("In base importa i futuri tenendo conto del delivery date e price come in adviceManagement")
#print("rimuovi postOffice dal setup del mailBox ...")
#print("Modificare 'Future EQ' in Future EquityIndex")
#print("fare in modo che quando viene inserita una nuova azione o opzione su azioni/indici o nuovo futuro viene aggiornato il dbEquity")
#print("createEquitySecurityFromIsin: se la tabella DBEquity contiene stesso ISIN ma valute diverse funzione blocca tutto")