# TODO: Add comment
# 
# Author: claudio
###############################################################################


setMethod("mainMessageProcessing",signature(message="PreComplianceResult"),
		function(message) {
			# this method assumes that the preComplianceResult file has been moved
			# from postOffice/incoming to postOffice/mailBox/pending folder
			
			# identify the success or failure ot the test
			if (message[["testResult"]]=="no") {
				
				newAdviceFileName <- paste(paste(getMessageDate_time_from(message),"newAdvice",sep="_"),message[["fileExtension"]],sep=".")
				
				# a) sendEmail
				result <- sendEmail_preComplianceResult(message)
				
				# b) move newAdvice from mailBox/pending to archive/processed/rejected,
				fromDir <- file.path(systemOptions[["homeDir"]],"postOffice",message[["portfolioName"]],"pending")
				toDir <- file.path(systemOptions[["homeDir"]],"archive","processed","rejected")
				ok <- file.move(newAdviceFileName,fromDir,toDir)
				if (ok) {
					messageString <- paste("file",newAdviceFileName,"successfully moved to the pending folder")
				} else {
					messageString <- paste("Error: impossible to move",newAdviceFileName,"to the pending folder")
				}
				logger(messageString)					
				
				#c) move PreComplianceResult da postOffice/pending a archive/processed/rejected
				ok <- file.move(message[["fileName"]],fromDir,toDir)
				if (ok) {
					messageString <- paste("file",message[["fileName"]],"successfully moved to the pending folder")
				} else {
					messageString <- paste("Error: impossible to move",message[["fileName"]],"to the pending folder")
				}				
				logger(messageString)
				
				#d) show pop up
				messageString <- paste("Advice '",newAdviceFileName,"' \nhas been rejected because of a negative pre-compliance.\n",sep="") 
				ok <- tkmessageBox(message=messageString,icon="warning")
				logger(paste("The message: ", messageString,"\nhas been read from user",sep=""))
				
				#e) rimuovi il lock
				ok <- unlock(message)
				logger(paste("lock removed from postOffice/",message[["portfolioName"]],sep=""))
				return()
			}
			
			if (message[["testResult"]]=="ok") {
				# on positive result maintain the lock
				
				#a) sendEmail
				result <- sendEmail_preComplianceResult(message)
				
				#b) the newAdvice and the PreComplianceResult files remain in the postOffice/mailBox/pending folder
				#c) show pop up
				newAdviceFileName <- paste(paste(getMessageDate_time_from(message),"newAdvice",sep="_"),message[["fileExtension"]],sep=".")
				messageString <- paste("Advice '",newAdviceFileName,"' \nsatisfies pre-compliance requirements.\n",sep="") 
				ok <- tkmessageBox(message=messageString,icon="warning")
				logger(paste("The message: ", messageString,"\nhas been read from user",sep=""))
				return()
			}
		}
)

if (is.element(message[["messageType"]],c("preComplianceResult","postComplianceResult"))) {
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
}