# TODO: Add comment
# 
# Author: ortellic
###############################################################################


setGeneric("messageProcessing",def=function(message) standardGeneric("messageProcessing"))

setMethod("messageProcessing",signature(message="PreComplianceResult"),
		function(message) {
			# this method assumes that the preComplianceResult file has been moved
			# from postOffice/incoming to postOffice/mailBox/pending folder
			
			# identify the success or failure ot the test
			if (message[["testResult"]]=="no") {
				
				newAdviceFileName <- paste(paste(getMessageDate_time_from(message),"newAdvice",sep="_"),message[["fileExtension"]],sep=".")
				
				#a) sendEmail
				result <- sendEmail_preComplianceResult(message)
				
				#b) move newAdvice da mailBox/pending a archive/processed/rejected,
				fromDir <- file.path(systemOptions[["homeDir"]],"postOffice",message@advisor@folderName,"pending")
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
				logger(paste("message:\n", messageString,"\nhas been read from user",sep=""))
				
				#e) rimuovi il lock
				ok <- unlock(message)
				logger(paste("lock removed from postOffice/",message@advisor@folderName,sep=""))
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
				logger(paste("message:\n", messageString,"\nhas been read from user",sep=""))
				return()
			}
		}
)
