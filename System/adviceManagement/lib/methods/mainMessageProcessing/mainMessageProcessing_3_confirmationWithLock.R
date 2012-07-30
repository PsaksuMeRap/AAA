# TODO: Add comment
# 
# Author: ortellic
###############################################################################



confirmationWithLock <- function(message) {

	messageFrom <- message[["from"]]
	csvTradesFileName <- message[["fileName"]]
	portfolioName <- message[["portfolioName"]]
	
	# if the file is locked move the advice in the pending folder and start processing
	logger(paste("Lock detected for confirmation of",portfolioName,"from",messageFrom),noOk="\n")
	
	# send an e-mail
	mail <- new("Mail",
			from=.secrets[["Riskmanager"]][["emailAddress"]],
			to=message@advisor@email,
			subject="Processing order",
			message=paste("Your confirmation '",csvTradesFileName,"' is being processed",sep="")
	)
	logger(paste("\nSending e-mail:\n",as.character(mail),sep=""))
	sendEMail(mail)
	loggerDone("\n [ok]\n\n")
	
	# move file
	fileFrom <- file.path(systemOptions[["homeDir"]],"postOffice","inbox",csvTradesFileName) 
	fileTo <- file.path(systemOptions[["homeDir"]],"postOffice",portfolioName,"pending",csvTradesFileName)
	copyOk <- file.copy(fileFrom,fileTo)
	
	if (copyOk) {
		logger(paste("File ",csvTradesFileName," successfully moved to ",portfolioName,"/pending",sep=""))
		logger("Removing incoming file ...")
		removeOk <- file.remove(fileFrom)
		if (removeOk) {
			loggerDone()
		} else {
			logger(paste("Error: impossible to remove file",fileFrom))
			# send e-mail to system administrator for manual remove
		}
	}
	

	# define the name of the file to process and costruct the command
	fullFileNameToExecute <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","subConfirmationProcessing.R")
	command <- "R CMD BATCH --slave --no-restore-history --no-timing --no-save "
	command <- paste(command,"\"--args csvTradesFileName='",csvTradesFileName,
			"' sourceCodeDir='",systemOptions[["sourceCodeDir"]],
			"' homeDir='"       ,systemOptions[["homeDir"]],"'\" ",
			fullFileNameToExecute," out.txt",sep="")
	
	# save the current working directory
	currentWorkingDirectory <- getwd()
	
	setwd(file.path(systemOptions[["homeDir"]],"postOffice",portfolioName,"pending"))
	logger(paste("Launching system command\n",command))
	system(command,wait=FALSE)
	loggerDone("\n[ok]")
	
	# restore the working directory
	setwd(currentWorkingDirectory)
	
	logger(paste("Started BATCH process for",csvTradesFileName))
	Sys.sleep(0.30)
	if (.Platform$OS.type=="windows") return(get_PID("Rterm.exe")) else return(get_PID("R"))
}

