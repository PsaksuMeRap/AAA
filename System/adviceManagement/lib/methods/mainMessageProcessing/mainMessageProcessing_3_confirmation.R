# TODO: Add comment
# 
# Author: ortellic
###############################################################################



setMethod("mainMessageProcessing",signature(message="Confirmation"),
		function(message) {
			# identify the advisor (filename="2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv")
			messageFrom <- message[["from"]]
			
			# identify the involved portfolio
			portfolioName <- message[["portfolioName"]]
			
			# check if the corresponding mailBox is available
			mailBoxExists <- is.element(portfolioName, dir(file.path(postOffice@absolutePath,"postOffice")))
			
			if (!mailBoxExists) {
				# if the mailBox does not exists create the mailbox
				logger(paste("Creating mailBox for portfolio",portfolioName,"from",messageFrom,"..."))
				mailbox <- new("MailBox",folderName=portfolioName)
				setup(x=mailbox,y=postOffice)
				loggerDone()
			}
			
			# is the mailbox locked? 
			isLocked <- file.exists(file.path(systemOptions[["homeDir"]],"postOffice",portfolioName,"lock"))
			
			# if locked move the advice in the mailbox_xxx/pending and start processing
			if (isLocked) {
				PID <- confirmationWithLock(message)
			} else {
				# if the file is not locked send an e-mail and move the file into the removed folder
				isOk <- confirmationNoLock(message)
			}
			
			
		}
)

