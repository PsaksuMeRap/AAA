
# import the code for the creation of the log file
source(file.path(sourceCodeDir,"adviceManagement","lib","logger.R"))

# create the log file
logFileName <- paste(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"),"riskman_log.txt",sep="_")
invisible(create_logger(fileName=logFileName,directory=file.path(homeDir,"log")))
logger("Logger successfully created.\n")

# setup the data directory
# check if the data directory exists
dataDirExists <- file.exists(file.path(homeDir,"data"))
if (!dataDirExists) {
	logger("Initializing entire data directory ...")
	from <- file.path(sourceCodeDir,"applicationData","data")
	to <- file.path(homeDir)
	isOk <- file.copy(from,to,recursive=TRUE)
	loggerDone()
	rm(dataDirExists,from,to,isOk)
} else {
# chech if the bloomberg directory exists
	bloombergDirExists <- file.exists(file.path(homeDir,"data","bloomberg"))
	if (!bloombergDirExists) {
		logger("Data directory exists. Initializing bloomberg directory ...")
		from <- file.path(sourceCodeDir,"applicationData","data","bloomberg")
		to <- file.path(homeDir,"data")
		isOk <- file.copy(from,to,recursive=TRUE)
		loggerDone()
	}
	
	checkFilesDirExists <- file.exists(file.path(homeDir,"data","checkFiles"))
	if (!checkFilesDirExists) {
		logger("Data directory exists. Initializing checkFile directory ...")
		from <- file.path(sourceCodeDir,"applicationData","data","checkFiles")
		to <- file.path(homeDir,"data")
		isOk <- file.copy(from,to,recursive=TRUE)
		loggerDone()
	}	
	
	DBEquitiesDirExists <- file.exists(file.path(homeDir,"data","DBEquities"))
	if (!DBEquitiesDirExists) {
		logger("Data directory exists. Initializing DBEquities directory ...")
		from <- file.path(sourceCodeDir,"applicationData","data","DBEquities")
		to <- file.path(homeDir,"data")
		isOk <- file.copy(from,to,recursive=TRUE)
		loggerDone()
	}		
	
	exchangeRatesDirExists <- file.exists(file.path(homeDir,"data","exchangeRates"))
	if (!exchangeRatesDirExists) {
		logger("Data directory exists. Initializing exchangeRates directory ...")
		from <- file.path(sourceCodeDir,"applicationData","data","exchangeRates")
		to <- file.path(homeDir,"data")
		isOk <- file.copy(from,to,recursive=TRUE)
		loggerDone()
	}
	
	instrumentsDirExists <- file.exists(file.path(homeDir,"data","instruments"))
	if (!instrumentsDirExists) {
		logger("Data directory exists. Initializing instruments directory ...")
		from <- file.path(sourceCodeDir,"applicationData","data","instruments")
		to <- file.path(homeDir,"data")
		isOk <- file.copy(from,to,recursive=TRUE)
		loggerDone()
	}
	
	portfoliosDirExists <- file.exists(file.path(homeDir,"data","portfolios"))
	if (!portfoliosDirExists) {
		logger("Data directory exists. Initializing portfolios directory ...")
		from <- file.path(sourceCodeDir,"applicationData","data","portfolios")
		to <- file.path(homeDir,"data")
		isOk <- file.copy(from,to,recursive=TRUE)
		loggerDone()
	}
	rm(checkFilesDirExists,DBEquitiesDirExists,exchangeRatesDirExists,instrumentsDirExists,portfoliosDirExists)
	rm(dataDirExists,from,to,isOk)
}


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

#print("In base importa i futuri tenendo conto del delivery date e price come in adviceManagement")
#print("rimuovi postOffice dal setup del mailBox ...")
#print("Modificare 'Future EQ' in Future EquityIndex")
#print("fare in modo che quando viene inserita una nuova azione o opzione su azioni/indici o nuovo futuro viene aggiornato il dbEquity")
#print("createEquitySecurityFromIsin: se la tabella DBEquity contiene stesso ISIN ma valute diverse funzione blocca tutto")