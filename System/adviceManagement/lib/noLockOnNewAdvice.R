# TODO: Add comment
# 
# Author: ortellic
###############################################################################


noLockOnNewAdvice <- function(message) {
	
	messageFrom <- message[["from"]]
	fileName <- message[["fileName"]]
	
	# if the file is not locked move the advice in the outbox and start processing
	logger(paste("no lock detected for orders from",messageFrom))
	
	# send an e-mail
	mail <- new("Mail",
			from="claudio.ortelli@usi.ch",
			to=message@advisor@email,
			subject="Processing order",
			message=paste("Your advice '",fileName,"' is being processed",sep="")
	)
	sendEMail(mail)
	logger(paste("\nMail sent:\n",as.character(mail),sep=""))
	
	# move file (da migliorare: file non devono essere sovrascritti)
	fileFrom <- file.path(systemOptions[["homeDir"]],"postOffice","inbox",fileName) 
	fileTo <- file.path(systemOptions[["homeDir"]],"postOffice",messageFrom,"pending",fileName)
	copyOk <- file.copy(fileFrom,fileTo)
	
	if (copyOk) {
		logger(paste("File to process",fileName,"successfully copied to outbox"))
		removeOk <- file.remove(fileFrom)
		if (removeOk) {
			logger(paste("file",fileFrom,"removed"))
		} else {
			logger(paste("error: impossible to remove file",fileFrom))
			# send e-mail to system administrator for manual remove
		}
	}
	
	lockFile <- file.create(file.path(systemOptions[["homeDir"]],"postOffice",messageFrom,"lock"))
	logger(paste("successfully locked mailBox of",messageFrom))
	
	# start batch process for processing
	# command <- "c:\\Progra~1\\R\\R-2.14.2\\bin\\R CMD BATCH --slave --no-restore-history --no-timing --no-save "
	
	# define the name of the file to process and costruct the command
	fileToExecute <- "newOrderProcessing.R"
	command <- "R CMD BATCH --slave --no-restore-history --no-timing --no-save "
	command <- paste(command,"\"--args messageFrom='",messageFrom,"'\" ",fileToExecute,sep="")
	
	# save the current working directory
	currentWorkingDirectory <- getwd()
	
	setwd(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib"))

	system(command,wait=FALSE)
	
	# restore the working directory
	setwd(currentWorkingDirectory)
	
	logger(paste("Started BATCH process for file",fileName,"from",messageFrom))
	Sys.sleep(0.30)
	if (.Platform$OS.type=="windows") return(get_PID("Rterm.exe")) else return(get_PID("R"))
}

