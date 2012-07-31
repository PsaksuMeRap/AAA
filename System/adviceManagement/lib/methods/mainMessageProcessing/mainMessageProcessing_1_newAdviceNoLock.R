# TODO: Add comment
# 
# Author: ortellic
###############################################################################



newAdviceNoLock <- function(message) {
	
	messageFrom <- message[["from"]]
	csvTradesFileName <- message[["fileName"]]
	portfolioName <- message[["portfolioName"]]
	
	# if the file is not locked move the advice in the pending folder and start processing
	logger(paste("No lock detected for newAdvice of",portfolioName,"from",messageFrom),noOk="\n")
	
	# send an e-mail
	mail <- new("Mail",
			from=.secrets[["Riskmanager"]][["emailAddress"]],
			to=message@advisor@email,
			subject="Processing order",
			message=paste("Your advice '",csvTradesFileName,"' is being processed",sep="")
	)
	logger(paste("\nSending e-mail:\n",as.character(mail),sep=""))
	sendEMail(mail)
	loggerDone("\n [ok]\n\n")
	
	# move csvTradesFileName (da migliorare: file non devono essere sovrascritti)
	fileFrom <- file.path(sys[["homeDir"]],"postOffice","inbox",csvTradesFileName) 
	fileTo <- file.path(sys[["homeDir"]],"postOffice",portfolioName,"pending",csvTradesFileName)
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
	} else {
		logger(paste("Error: impossible to copy ",csvTradesFileName," to ",portfolioName,"/pending. Sending warning e-mail ...",sep=""))
		# send an e-mail
		mail <- new("Mail",
				from=.secrets[["Riskmanager"]][["emailAddress"]],
				to=message@advisor@email,
				subject="Processing order",
				message=paste("Error when copying file'",csvTradesFileName,"' in the pending folder",sep="")
		)
		logger(paste("\nSending e-mail:\n",as.character(mail),sep=""))
		sendEMail(mail)
		loggerDone("\n [ok]\n\n")
		
		return(0)
	}
	
	logger(paste("Locking mailBox of",portfolioName,"..."))
	lockFile <- file.create(file.path(sys[["homeDir"]],"postOffice",portfolioName,"lock"))
	if (lockFile) loggerDone() else logger("Error: impossible to lock the mailbox ... why?")
	
	# start batch process for processing
	# command <- "c:\\Progra~1\\R\\R-2.14.2\\bin\\R CMD BATCH --slave --no-restore-history --no-timing --no-save "
	
	# define the name of the file to process and costruct the command
	fullFileNameToExecute <- file.path(sys[["sourceCodeDir"]],"adviceManagement","lib","subNewAdviceProcessing.R")
	command <- "R CMD BATCH --slave --no-restore-history --no-timing --no-save "
	command <- paste(command,"\"--args csvTradesFileName='",csvTradesFileName,
			"' sourceCodeDir='",sys[["sourceCodeDir"]],
			"' homeDir='"       ,sys[["homeDir"]],"'\" ",
			fullFileNameToExecute," out.txt",sep="")
	
	# save the current working directory
	currentWorkingDirectory <- getwd()
	
	setwd(file.path(sys[["homeDir"]],"postOffice",portfolioName,"pending"))
	logger(paste("Launching system command\n",command))
	system(command,wait=FALSE)
	loggerDone("\n[ok]")
	
	# restore the working directory
	setwd(currentWorkingDirectory)
	
	logger(paste("Started BATCH process for",csvTradesFileName))
	Sys.sleep(0.30)
	if (.Platform$OS.type=="windows") return(get_PID("Rterm.exe")) else return(get_PID("R"))
}

