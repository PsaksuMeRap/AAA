# TODO: Add comment
# 
# Author: ortellic
###############################################################################


isLockOnNewAdvice <- function(message) {
	
	rootDir <- systemOptions[["homeDir"]]
	fileName <- message[["fileName"]]
	
	logger(paste("lock detected for order",fileName, "from",message[["from"]]))
	
	# send an e-mail
	mail <- new("Mail",
			from=.secrets[["Riskmanager"]][["emailAddress"]],
			to=message@advisor@email,
			subject="Advice refused",
			message=paste("Your advice '",fileName,"' has been deleted because of a pending order.\nPlease resend a new advice after execution and confirmation of the current one.",sep="")
	)
	sendEMail(mail)
	logger(paste("Mail sent:\n",as.character(mail),sep=""))

	# move file in the archive/deleted folder (da migliorare: file non devono essere sovrascritti)
	fileFrom <- file.path(rootDir,"postOffice","inbox",fileName) 
	fileTo <- file.path(rootDir,"archive","deleted",fileName)
	copyOk <- file.copy(fileFrom,fileTo)
	
	if (copyOk) {
		logger(paste("File",fileName,"successfully copied to directory archive/deleted"))
		removedOk <- file.remove(fileFrom)
		if (removedOk) {
			logger(paste("file",fileFrom,"successfully moved to directory archive/deleted"))
			return(0)
		} else {
			logger(paste("error: impossible to remove file",fileFrom))
			# send e-mail to system administrator for manual remove
			mail <- new("Mail",
					from=.secrets[["Riskmanager"]][["emailAddress"]],
					to=message@advisor@email,
					subject="Serious error",
					message=paste("Impossible to remove the incoming advice '",fileName,"'.\nA manual intervention is required.\nThe file must be removed to the ./archive/deleted folder",sep="")
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
				message=paste("Impossible to copy the incoming advice '",fileName,"' to archive/deleted.\nA manual intervention is required.",sep="")
		)
		sendEMail(mail)
		logger(paste("Mail sent:\n",as.character(mail),sep=""))
		return(1)
	}
}
