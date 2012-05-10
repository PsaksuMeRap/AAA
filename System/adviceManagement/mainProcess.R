# TODO: Add comment
# 
# Author: Claudio
###############################################################################
rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
library("tcltk")
library("stringr")

stringsAsFactors = FALSE

source("./base/lib/library.R")

# set the directory where the source code is installed (i.e. folders adviceManagement, ayrton, base, riskman)
sourceCodeDir <- getwd()

# import the adviceManagement library
source(file.path(sourceCodeDir,"adviceManagement","lib","library.R"))

# set the name of the process
name <- "main"

# set the root directory
## homeDir <- "c:/riskman"
homeDir <- file.path(sourceCodeDir,"adviceManagement","unitTests","directories")
setwd(homeDir)

# set the sleeping time in seconds
sleepTime <- 10

# define the adivisors
advisors <- new("Advisors")
advisors[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="reto.ghidossi@opencapital.ch")
advisors[["MaggiDynamic"]] <- new("Advisor",name="MaggiDynamic",folderName="MaggiDynamic",email="maggi.sandro@")


# create the log file
sink(file="riskman_log.txt")

# inizializza PostOffice
postOffice <- new("PostOffice",absolutePath=homeDir)
setup(postOffice)
logger("PostOffice created.")

# check that no R processes are running and identify the PID of this program
PID <- get_PID(imageName="Rterm.exe")
PID <- get_PID(imageName="eclipse.exe")
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

existingFiles <- list.files(path=file.path(homeDir,"postOffice","input"))

T <- Sys.time()+240
while(Sys.time()<T) {
print("sleep 10 seconds")
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
			#identify messageTye between: newAdvice,adviceConfirmation,preComplianceResult,postComplianceResult
			messageType <- messageFactory(fileName,advisors)
			
			# identify the advisor (filename="yyyy-mm-dd hh:mm nome.csv")
			messageFrom <- messageType[["From"]]
			
			# check if the corresponding mailBox is available
			mailBoxExists <- is.element(messageFrom,postOffice@mailBoxes)
			
			# if exists the mailbox check for a lock
			
			if (mailBoxExists) {
				# is the mailbox locked? 
				isLocked <- file.exists(file.path(homeDir,"postOffice",messageFrom,"lock"))
				# if locked send an e-mail and move the file into the removed folder
				# and log the actions
				if (isLocked) {
adadf
				} else {
					# if the file is not locked move the advice in the outbox and start processing
					PID <- noLockOnNewAdvice(message)
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
		
		
		
		
		
		sleepTime <- sleepTime - 5
	}
	logger(paste("sleep",sleepTime,"seconds ..."))
	Sys.sleep(sleepTime)
}

# terminate the output
sink()