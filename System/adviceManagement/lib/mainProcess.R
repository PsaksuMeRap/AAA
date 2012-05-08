# TODO: Add comment
# 
# Author: Claudio
###############################################################################

library("RODBC")
library("RUnit")
library("tcltk")
library("stringr")

# set the name of the process
name <- "main"
# set the root directory
rootDir <- "c:/riskman"
setwd(rootDir)
# set the sleeping time in seconds
sleepTime <- 10

# define the adivisors
advisors <- new("Advisors")
advisors@.Data[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="reto.ghidossi@opencapital.ch")
advisors@.Data[["MaggiDynamic"]] <- new("Advisor",name="MaggiDynamic",folderName="MaggiDynamic",email="maggi.sandro@")

logger <- function(message) {
	cat(paste(Sys.time()," from ",name,": ",message,"\n",sep=""))
}


# create the log file
sink(file="riskman_log.txt")

# inizializza PostOffice
postOffice <- new("PostOffice",absolutePath=rootDir)
setup(postOffice)
logger("PostOffice created.")

# check that no R processes are running and identify the PID of this program
PID <- get_PID(imageName="Rterm.exe")
if (length(PID)>1) {
	msg <- "Please close all running R/Rterm processes before starting a new one!"
	ok <- tkmessageBox(message=msg,icon="warning",type="ok")
	logger("Multiple Rterm processes detected. Stop this application ...")
	# remove the postOffice
	logger("Removing postOffice")
	unlink("postOffice",recursive=TRUE)
	logger("postOffice removed")
	quit(save="no")
}

# create the data.frame with the activeOrders
activeOrders <- data.frame(name="main",startTime=Sys.time(),fileName="",orderName="",PID=PID)

# start monitoring input and output directories

existingFiles <- list.files(path=file.path(rootDir,"postOffice","input"))
processedFiles <- list.files(path=file.path(rootDir,"postOffice","outbox"))

T <- sys.time()+240
while(Sys.time()<T) {
	sleepTime <- 10
	if (length(existingFiles>0)) {
		# sort the files
		existingFiles <- sort(existingFiles)
		# log the arrival of the new files
		msg <- "The following files have arrived:\n"
		for (fileName in existingFiles) {
			msg <- paste(msg,"- ",fileName,"\n",sep="")
		}
		logger(msg)
		
		for (fileName in existingFiles) {
			# identify the advisor (filename="yyyy-mm-dd hh:mm nome.csv")
			messageFrom <- substring(fileName,17,nchar(fileName)-4)
			
			# check if the corresponding mailBox is available
			mailBoxExists <- is.element(messageFrom,postOffice@mailBoxes)
			
			# if exists the mailbox check for a lock
			
			if (mailBoxExists) {
				# is the mailbox locked? 
				isLocked <- file.exists(file.path(rootDir,"postOffice",messageFrom,"lock"))
				# if locked send an e-mail and move the file into the removed folder
				# and log the actions
				if (isLocked) {
					logger(paste("lock detected for orders from",messageFrom))
					# send e-mail
					x <- advisors[[messageFrom]]
					mail <- new("Mail",
							from="claudio.ortelli@usi.ch",
							to=x@email,
							subject="Order refused",
							message=paste("Your advice '",filename,"' has been deleted because of a pending order.\nPlease resend a new advice after execution and confirmation of the current one.",sep="")
					)
					sendEmail(mail)
					logger(paste("Mail sent:\n",as.character(mail),sep=""))
					
					# move file (da migliorare: file non devono essere sovrascritti
					fileFrom <- file.path(rootDir,"postOffice","inbox",fileName) 
					fileTo <- file.path(rootDir,"postOffice","trash",fileName)
					copyOk <- file.copy(fileFrom,fileTo)
					
					if (copyOk) {
						logger(paste("File",fileName,"successfully copied to postOffice/trash"))
						removeOk <- file.remove(fileFrom)
						if (removedOk) {
							logger(paste("file",fileFrom,"removed"))
						} else {
							logger(paste("error: impossible to remove file",fileFrom))
							# send e-mail to system administrator for manual remove
						}
					}
				} else {
					# if the file is not locked move the advice in the outbox and start processing
					logger(paste("no lock detected for orders from",messageFrom))
					# send e-mail
					x <- advisors[[messageFrom]]
					mail <- new("Mail",
							from="claudio.ortelli@usi.ch",
							to=x@email,
							subject="Processing order",
							message=paste("Your advice '",filename,"' is being processed",sep="")
					)
					sendEmail(mail)
					logger(paste("Mail sent:\n",as.character(mail),sep=""))
					
					# move file (da migliorare: file non devono essere sovrascritti
					fileFrom <- file.path(rootDir,"postOffice","inbox",fileName) 
					fileTo <- file.path(rootDir,"postOffice",messageFrom,"pending",fileName)
					copyOk <- file.copy(fileFrom,fileTo)
					
					if (copyOk) {
						logger(paste("File to process",fileName,"successfully copied to outbox"))
						removeOk <- file.remove(fileFrom)
						if (removedOk) {
							logger(paste("file",fileFrom,"removed"))
						} else {
							logger(paste("error: impossible to remove file",fileFrom))
							# send e-mail to system administrator for manual remove
						}
					}
					# start batch process for processing
				}
			} else { 
				# if the mailBox does not exists create the mailbox and
				# start the pre-compliance check
				postOffice@mailBoxes[length(postOffice@mailBoxes)+1] <- messageFrom
				mailbox <- new("MailBox",advisor=advisors[[messageFrom]])
				setup(x=mailbox,y=postOffice)
				logger(paste("created mailBox for advisor",messageFrom))
				
				# start pre-compliance
				
				logger("pre-compliance for file",fileName,"started")
				
				
			}
			sleepTime <- sleepTime - 5
		} # end for (fileName in existingFiles)
		
	}
	
	if (length(processedFiles>0)) {
		# sort the files
		processedFiles <- sort(processedFiles)
		# log the arrival of the new processed files
		msg <- "The following output files are available:\n"
		for (fileName in processedFiles) {
			msg <- paste(msg,"- ",fileName,"\n",sep="")
		}
		logger(msg)
		
		
		
		
		
		
		
		
		
		sleepTime <- sleepTime - 5
	}
	logger("sleep",sleepTime,"seconds ...")
	Sys.sleep(sleepTime)
}

# terminate the output
sink()