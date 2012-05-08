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
advisors@.Data[[1]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="reto.ghidossi@opencapital.ch")
advisors@.Data[[2]] <- new("Advisor",name="MaggiDynamic",folderName="MaggiDynamic",email="maggi.sandro@")

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

# start monitoring input directory

existingFiles <- list.files(path=file.path(rootDir,"postOffice","input"))
if (length(existingFiles>0)) {
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
			# guarda se è loccato 
			# if (isLocked) {
			# se è loccato manda e-mail e scarta il file nel trash
			# } else {
			# se non è loccato avvia il processo di analisi
			# }
		} else { 
			# if the mailBox does not exists create the mailbox and
			# start the pre-compliance check
			
		}
	} 
} else {
	Sys.sleep(sleepTime)
}

# terminate the output
sink()