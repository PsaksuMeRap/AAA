# TODO: Add comment
# 
# Author: ortellic
###############################################################################


confirmationNoLock <- function(message) {
	
	rootDir <- systemOptions[["homeDir"]]
	fileName <- message[["fileName"]]
	
	logger(paste("No lock detected for order",fileName, "from",message[["from"]]))
	
	# send an e-mail
	mail <- new("Mail",
			from=.secrets[["Riskmanager"]][["emailAddress"]],
			to=message@advisor@email,
			subject="Confirmation advice refused",
			message=paste("Your confirmation '",fileName,"' has been deleted because of no pending order.\nPlease send a new advice first or contact the riskmanager.",sep="")
	)
	sendEMail(mail)
	logger(paste("Mail sent:\n",as.character(mail),sep=""))
	
	# move file in the archive/deleted folder
	fileFrom <- file.path(rootDir,"postOffice","inbox",fileName) 
	fileTo <- file.path(rootDir,"archive","deleted",fileName)
	logger(paste("Copying file",fileName,"to directory archive/deleted ... "))
	copyOk <- file.copy(fileFrom,fileTo)
	cat(copyOk)
	
	if (copyOk) {
		logger(paste("Removing",fileFrom,"... "))
		removedOk <- file.remove(fileFrom)
		cat(removedOk)
		if (removedOk) {
			return(0)
		} else {
			logger(paste("Error: impossible to remove file",fileFrom))
			# send e-mail to system administrator for manual remove
			mail <- new("Mail",
					from=.secrets[["Riskmanager"]][["emailAddress"]],
					to=message@advisor@email,
					subject="Serious error",
					message=paste("Impossible to remove the incoming confirmation '",fileName,"'.\nA manual intervention is required.\nThe file must be removed from the ./archive/deleted folder",sep="")
			)
			sendEMail(mail)
			logger(paste("Mail sent:\n",as.character(mail),sep=""))
			return(2)
		}
	} else { # if copyOk is FALSE
		logger(paste("Error: impossible to copy the file '",fileName,"' to ./archive/deleted.\n A manual intervention is required.\nThe file must be moved to the ./archive/deleted folder",sep=""))
		# send e-mail to system administrator for manual remove
		mail <- new("Mail",
				from=.secrets[["Riskmanager"]][["emailAddress"]],
				to=message@advisor@email,
				subject="Serious error",
				message=paste("Impossible to copy the incoming confirmation '",fileName,"' to archive/deleted.\nA manual intervention is required.",sep="")
		)
		sendEMail(mail)
		logger(paste("Mail sent:\n",as.character(mail),sep=""))
		return(1)
	}
}
