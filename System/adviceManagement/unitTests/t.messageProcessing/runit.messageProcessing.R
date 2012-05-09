# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldProcessNewAdviceMessage <- function() {
	testRootDir <- file.path(rootDir,"adviceManagement","unitTests","directories")

	advisors <- new("Advisors")
	advisors@.Data[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="reto.ghidossi@opencapital.ch")
	
	postOffice <- new("PostOffice",absolutePath=testRootDir)
	setup(postOffice)
	
	fileName <- "2012-05-09_11-48-16_GhidossiGlobalEquity_newAdvice.csv"
	message <- messageFactory(fileName)


	setGeneric("messageProcessing", def=function(message) standardGeneric("messageProcessing"))
	
	setMethod("messageProcessing",signature(message="NewAdvice"),
			function(message) {

				# identify the advisor and fund
				messageFrom <- message[["From"]]

				# check if the corresponding mailBox is available
				mailBoxExists <- is.element(messageFrom,postOffice@mailBoxes)
				
				# if exists the mailbox check for a lock
				
				if (mailBoxExists) {
					
	!!! newAdviceOnMailBoxExists 
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
				
				
			}
	)
