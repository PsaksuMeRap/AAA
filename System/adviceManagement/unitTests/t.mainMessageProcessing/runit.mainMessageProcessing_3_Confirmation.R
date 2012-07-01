# TODO: Add comment
# 
# Author: Claudio
###############################################################################

test.shouldProcessConfirmationMessage <- function() {

	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	# create the mailBox
	mailBox <- new("MailBox",folderName="globalEquity")
	setup(x=mailBox,y=postOffice)
	
	# copy the confirmation message
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_confirmation.csv"
	fromDir <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.messageProcessing")
	toDir <- file.path(systemOptions[["homeDir"]],"postOffice","globalEquity","pending")
	toFile <- file.path(toDir,fileName)
	
	# identify the messageType
	ok <- file.copy(from=file.path(fromDir,fileName),to=toFile)
	message <- messageFactory(fileName,toDir,advisors)
	
	# lock
	ok <- lock(message)
	
	setMethod("messageProcessing",signature(message="Confirmation"),
			function(message) {
				# this method assumes that the confirmation file has been moved
				# from postOffice/inbox to postOffice/mailBox_xyz/pending folder
				
				# update the portfolio
				# a) carica l'ultimo portafoglio in database yyyy-mm-dd_hh-mm-ss_portafoglio.RData
				# b) aggiorna il portafoglio
				# c) salva il nuovo portafoglio nel db
			
				updatePortfolio(message)
				logger(paste("Updated portfolio with confirmation message",message[["fileName"]]))
			
				
			}
	
	)
	
	checkEquals(1,0)
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)	
}
