# TODO: Add comment
# 
# Author: claudio
###############################################################################


setMethod("mainMessageProcessing",signature(message="PreComplianceResult"),
		function(message) {
			# this method assumes that the preComplianceResult file has been moved
			# from postOffice/portfolio/pending to postOffice/incoming folder
			
			# remove the file in the postOffice/incoming folder
			fullFileName <- file.path(systemOptions[["homeDir"]],"postOffice","inbox",message[["fileName"]])
			logger(paste("Removing file",fullFileName,"... "))
			cat(file.remove(fullFileName))
			
			# sendEmail
			logger(paste("Sending e-mail with",fullFileName,"...\n"))
			result <- sendEmail_preComplianceResult(message)
			loggerDone(paste(result,"\n"))
			
			# remove the lock
			logger(paste("Removing lock for portfolio",message[["portfolioName"]],"... "))
			cat(unlock(message))
			
			# show pop up
			if (message[["testResult"]]=="0") {
				messageString <- paste("Result '",message[["fileName"]],"' \nhas been rejected because of a negative pre-compliance.\n",sep="") 
				ok <- tkmessageBox(message=messageString,icon="warning")
				messageString <- paste("Result '",message[["fileName"]],"' has been rejected because of a negative pre-compliance.\n",sep="")
			} else {
				messageString <- paste("Result '",message[["fileName"]],"' \nhas been accepted.",sep="") 
				ok <- tkmessageBox(message=messageString,icon="warning")
				messageString <- paste("Result '",message[["fileName"]],"' has been accepted.",sep="") 
			}

			logger(paste("The following message:\n\n", messageString,"\nhas been read from user",sep=""))
			
			return()
		}
)
