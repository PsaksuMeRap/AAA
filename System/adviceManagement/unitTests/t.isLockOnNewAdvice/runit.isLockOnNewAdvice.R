# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldRunIsLockOnNewAdvice <- function(message) {
	
	isLockOnNewAdvice <- function(message) {
		
		messageFrom <- message[["from"]]
		fileName <- message[["fileName"]]
		
		logger(paste("lock detected for order",fileName, "from",messageFrom))
		
		# send an e-mail
		mail <- new("Mail",
				from="claudio.ortelli@usi.ch",
				to=message@advisor@email,
				subject="Order refused",
				message=paste("Your advice '",fileName,"' has been deleted because of a pending order.\nPlease resend a new advice after execution and confirmation of the current one.",sep="")
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
	}
}
