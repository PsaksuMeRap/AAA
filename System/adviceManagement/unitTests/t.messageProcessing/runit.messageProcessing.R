# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldProcessNewAdviceMessage <- function() {

}


test.shouldProcessAdviceConfirmationMessage <- function() {
	
}


test.shouldProcessPreComplianceResultMessage <- function() {
	
	# create the archive
	create_archive(systemOptions[["homeDir"]])
	
	# identify a new order
	fileName <- "2012-05-09_11-48-16_GhidossiGlobalEquity_preComplianceResult_ok.csv"
	
	# define the adivisors
	advisors <- new("Advisors")
	advisors[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="claudio.ortelli@gmail.com")
	
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	# create the mailBox
	mailBox <- new("MailBox",advisor=advisors[["GhidossiGlobalEquity"]])
	setup(x=mailBox,y=postOffice)
	
	# identify the messageType
	message <- messageFactory(fileName,advisors)
	
	# lock
	ok <- lock(message)
	
	ok <- file.create(file.path(systemOptions[["homeDir"]],"postOffice",advisors[["GhidossiGlobalEquity"]]@folderName,"pending",message[["fileName"]])) 
	
	setMethod("messageProcessing",signature(message="PreComplianceResult"),
			function(message) {
				# this method assumes that the preComplianceResult file has been moved
				# from postOffice/incoming to postOffice/mailBox/pending folder
				
				# identify the success or failure ot the test
				if (message[["testResult"]]=="no") {
					workdir <- getwd()
					setwd(file.path(systemOptions[["sourceCodeDir"]],"postOffice",message@advisor@folderName,"pending"))
					#a) sendEmail
					newAdviceFileName <- paste(paste(getMessageDate_time_from(message),"newAdvice","_"),message[["fileExtension"]],sep=".")
					mail <- new("Mail",
							from="claudio.ortelli@usi.ch",
							to=message@advisor@email,
							subject="Advice refused",
							message=paste("Your advice '",newAdviceFileName,"' has been rejected because of a negative pre-compliance.\n",sep=""),
							attachments=message[["fileName"]]
					)
					sendEMail(mail)
					logger(paste("Mail sent:\n",as.character(mail),sep=""))
					setwd(workdir)
					
					#b) move newAdvice da mailBox/pending a archive/processed/rejected,
					fromDir <- file.path(systemOptions[["sourceCodeDir"]],"postOffice",message@advisor@folderName,"pending")
					toDir <- file.path(systemOptions[["sourceCodeDir"]],"archive","processed","rejected")
					ok <- file.move(newAdviceFileName,fromDir,toDir)
					if (ok) {
						message <- paste("file",newAdviceFileName,"successfully moved to the pending folder")
					} else {
						message <- paste("Error: impossible to move",newAdviceFileName,"to the pending folder")
					}
					logger(message)
					
					
					#c) move PreComplianceResult da postOffice/pending a archive/processed/rejected 
					ok <- file.move(message[["fileName"]],fromDir,toDir)
					if (ok) {
						message <- paste("file",message[["fileName"]],"successfully moved to the pending folder")
					} else {
						message <- paste("Error: impossible to move",message[["fileName"]],"to the pending folder")
					}				
					logger(message)
					
					#d) show pop up 
					message <- paste("Advice '",newAdviceFileName,"' \nhas been rejected because of a negative pre-compliance.\n",sep="") 
					ok <- tkmessageBox(message=message,icon="warning")
					logger(paste("message:\n", message,"\nhas been read from user",sep=""))
					
					#e) rimuovi il lock
					ok <- unlock(message)
					logger(paste("lock removed from postOffice/",message@advisor@folderName,sep=""))
					return()
				}
				
				if (message[["testResult"]]=="ok") {
					# on positive result maintain the lock
	
					# change directory to send the attachment
					workdir <- getwd()
					setwd(file.path(systemOptions[["sourceCodeDir"]],"postOffice",message@advisor@folderName,"pending"))
					
					#a) sendEmail
					newAdviceFileName <- paste(paste(getMessageDate_time_from(message),"newAdvice","_"),message[["fileExtension"]],sep=".")
					mail <- new("Mail",
							from="claudio.ortelli@usi.ch",
							to=message@advisor@email,
							subject="Advice accepted",
							message=paste("Your advice '",newAdviceFileName,"' has been accepted for execution.\n",sep=""),
							attachments=message[["fileName"]]
					)
					sendEMail(mail)
					logger(paste("Mail sent:\n",as.character(mail),sep=""))
					setwd(workdir)
					
					#b) the newAdvice and the PreComplianceResult files remain in the postOffice/mailBox/pending folder
					#c) show pop up 
					message <- paste("Advice '",newAdviceFileName,"' \nsatisfies pre-compliance requirements.\n",sep="") 
					ok <- tkmessageBox(message=message,icon="warning")
					logger(paste("message:\n", message,"\nhas been read from user",sep=""))
					return()
				}
			}
	)
	
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	setwd(workingDirectory)
	
}

test.shouldProcessPostComplianceResultMessage <- function() {
	
}