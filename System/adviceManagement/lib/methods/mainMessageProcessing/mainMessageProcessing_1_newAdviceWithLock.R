# TODO: Add comment
# 
# Author: ortellic
###############################################################################


newAdviceWithLock <- function(message) {
	
	fileName <- message[["fileName"]]
	
	# send an e-mail
	sendEmail_newAdviceDeleted(fileName,message)
	
	# move file in the archive/deleted folder
	fileFrom <- file.path(sys[["homeDir"]],"postOffice","inbox",fileName) 
	fileTo <- file.path(sys[["homeDir"]],"archive","deleted",fileName)
	copyOk <- file.copy(fileFrom,fileTo)
	
	if (copyOk) {
		logger(paste("File",fileName,"successfully copied to directory", file.path(sys[["homeDir"]],"archive","deleted") ))
		removedOk <- file.remove(fileFrom)
		if (removedOk) {
			logger(paste("File",fileFrom,"successfully moved to directory", file.path(sys[["homeDir"]],"archive","deleted") ))
			return(0)
		} else {
			logger(paste("Error: impossible to remove file",fileFrom,"from", file.path(sys[["homeDir"]],"postOffice","inbox") ))
			# send e-mail to system administrator for manual remove
			mail <- new("Mail",
					from=.secrets[["Riskmanager"]][["emailAddress"]],
					to=message@advisor@email,
					subject="Serious error",
					message=paste("Impossible to remove the incoming advice '",fileName,"' from ",file.path(sys[["homeDir"]],"postOffice","inbox"),".\n",
							"Please remove the file manually.",sep="")
			)
			sendEMail(mail)
			logger(paste("Mail sent:\n",as.character(mail),sep=""))
			return(2)
		}
	} else { # if copyOk is FALSE
		logger(paste("Error: impossible to copy the file '",fileName,"' to\n",file.path(sys[["homeDir"]],"archive","deleted"),". A manual intervention is required.",sep=""))
		# send e-mail to system administrator for manual remove
		mail <- new("Mail",
				from=.secrets[["Riskmanager"]][["emailAddress"]],
				to=message@advisor@email,
				subject="Serious error",
				message=paste("Impossible to move the incoming advice '",fileName,"' from ",file.path(sys[["homeDir"]],"postOffice","inbox")," to\n",
						file.path(sys[["homeDir"]],"archive","deleted"),"\n",
						"A manual intervention is required.\nThe file must be moved to the ./archive/deleted folder",sep="")
		)
		sendEMail(mail)
		logger(paste("Mail sent:\n",as.character(mail),sep=""))
		return(1)
	}
}
