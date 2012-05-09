# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldNoLockOnNewAdvice <- function() {
	
	originalWorkingDirectory <- getwd()
	testRootDir <- file.path(rootDir,"adviceManagement","unitTests","directories")
	fileRoot <- testRootDir
	
	advisors <- new("Advisors")
	advisors@.Data[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="claudio.ortelli@gmail.com")
	
	#create postOffice
	postOffice <- new("PostOffice",absolutePath=testRootDir)
	setup(postOffice)
	
	# copy incoming message
	from <- file.path(originalWorkingDirectory,"adviceManagement","unitTests","files","2012-05-09_14-22-24_GhidossiGlobalEquity_newAdvice.csv")
	to <- file.path(testRootDir,"postOffice","inbox")
	file.copy(from,to)
	
	postOffice@mailBoxes[length(postOffice@mailBoxes)+1] <- messageFrom
	mailbox <- new("MailBox",advisor=advisors[[messageFrom]])
	setup(x=mailbox,y=postOffice)

	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_newAdvice.csv"
	message <- messageFactory(fileName)
	processName <- "main"
	
	
	noLockOnNewAdvice <- function(message) {
			
		messageFrom <- message[["from"]]
		fileName <- message[["fileName"]]
		
		# if the file is not locked move the advice in the outbox and start processing
		logger(paste("no lock detected for orders from",messageFrom))
		# send e-mail
		advisor <- advisors[[messageFrom]]
		mail <- new("Mail",
				from="claudio.ortelli@usi.ch",
				to=advisor@email,
				subject="Processing order",
				message=paste("Your advice '",fileName,"' is being processed",sep="")
		)
		sendEMail(mail)
		logger(paste("\nMail sent:\n",as.character(mail),sep=""))
		
		# move file (da migliorare: file non devono essere sovrascritti)
		fileFrom <- file.path(fileRoot,"postOffice","inbox",fileName) 
		fileTo <- file.path(fileRoot,"postOffice",messageFrom,"pending",fileName)
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
		
		lockFile <- file.create(file.path(fileRoot,"postOffice",messageFrom,"lock"))
		logger(paste("successfully locked mailBox of",messageFrom))
		
		# start batch process for processing
		# command <- "c:\\Progra~1\\R\\R-2.14.2\\bin\\R CMD BATCH --slave --no-restore-history --no-timing --no-save "
		command <- "c:/Progra~1/R/R-2.14.2/bin/R CMD BATCH --slave --no-restore-history --no-timing --no-save "
		filetoExecute <- file.path(originalWorkingDirectory,"adviceManagement","unitTests","t.noLockOnNewAdvice","orderProcessing.R")
		command <- paste(command,"\"--args messageFrom='",messageFrom,"'\" ",filetoExecute,sep="")
		system(command,wait=FALSE)
		logger(paste("Started BATCH process for file",fileName,"from",messageFrom))
		return(get_PID("Rterm.exe"))
	}
				
	allPID <- noLockOnNewAdvice(message)
	checkEquals(length(allPID)>0,TRUE)
	
	unlink(file.path(testRootDir,"postOffice"),recursive=TRUE)
	
	setwd(originalWorkingDirectory)
	
