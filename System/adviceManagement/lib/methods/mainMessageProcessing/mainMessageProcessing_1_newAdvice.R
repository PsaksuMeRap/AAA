# TODO: Add comment
# 
# Author: ortellic
###############################################################################


setMethod("mainMessageProcessing",signature(message="NewAdvice"),
		function(message,postOffice) {
			# identify the advisor (i.e. fileName="2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv")
			messageFrom <- message[["from"]]
			
			# identify the involved portfolio
			portfolioName <- message[["portfolioName"]]
			
			# check if the corresponding mailBox is available
			mailBoxExists <- is.element(portfolioName, dir(file.path(postOffice@absolutePath,"postOffice")))
			
			if (!mailBoxExists) {
				# log the creation of the mailBox
				logger(paste("Creating mailBox for portfolio",portfolioName,"from",messageFrom,"..."))
				
				# if the mailBox does not exists create the mailbox
				mailbox <- new("MailBox",folderName=portfolioName)
				setup(x=mailbox,y=postOffice)
				
				loggerDone()
			}
			
			# is the mailbox locked? 
			isLocked <- file.exists(file.path(sys[["homeDir"]],"postOffice",portfolioName,"lock"))
			
			if (isLocked) {
				# log the existence of the lock
				logger(paste("Lock detected for order",message[["fileName"]], "from",message[["from"]]))
				# if locked send an e-mail and move the file into the removed folder
				isOk <- newAdviceWithLock(message)
			} else {
				# if the mailBox is not locked start processing the newAdvice
				PID <- newAdviceNoLock(message)
			}
		}
)

