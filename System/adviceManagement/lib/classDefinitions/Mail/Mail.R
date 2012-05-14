# TODO: Add comment
# 
# Author: Claudio
###############################################################################


setClass("Mail",
		representation(
				from="character",
				to="character",
				subject="character",
				message="character",
				attachments="character",
				username="character",
				password="character",
				server="character"
		)
)

sendEMail <- function(mail) {
	command <- paste("sendEmail",
			"-f",mail@from,
			"-t",mail@to,
			"-u",shQuote(mail@subject),
			"-m",shQuote(mail@message),
			"-s mail.usi.ch",
			"-xu ortellic",
			"-xp tega=01"
	)
	
	if (length(mail@attachments)>0) {
		command <- paste(command,"-a",paste(mail@attachments,collapse=" "))
	}
	
	
	result <- system(command,intern=TRUE,wait=TRUE)
	# check that the e-mail was sent successfully
	# nchar(result)<28 means an error occured
	nbChar <- nchar(result)
	if (nbChar<28) return(result)
	subStringToCheck <- substr(result,nbChar-28+1,nbChar)
	isOk <- subStringToCheck == "Email was sent successfully!"
	if (isOk) return(subStringToCheck) else return(result)
}

setMethod("as.character","Mail", 
		function(x) {
			nbAttachments <- length(x@attachments)
			string <- rep("",4+nbAttachments)
			string[1] <- paste("from:",x@from)
			string[2] <- paste("to:",x@to)
			string[3] <- paste("subject:",x@subject)
			string[4] <- paste("message:",x@message)
			i <- 4
			for (attachment in x@attachments) {
				i <- i + 1
				string[i] <- paste("attachments:",attachment)			
			}
			return(paste(string,collapse="\n"))
		}
)


sendEmail_preComplianceResult <- function(message) {
	workdir <- getwd()
	setwd(file.path(systemOptions[["homeDir"]],"postOffice",message@advisor@folderName,"pending"))

	newAdviceFileName <- paste(paste(getMessageDate_time_from(message),"newAdvice",sep="_"),message[["fileExtension"]],sep=".")
	
	if (message[["testResult"]]=="no") {
		stringMessage <- paste("Your advice '",newAdviceFileName,"' has been rejected because of a negative pre-compliance.\n",sep="")
		subject <- "Advice rejected"
	} else {
		stringMessage <- paste("Your advice '",newAdviceFileName,"' has been accepted.\n",sep="")
		subject <- "Advice accepted"
	}
	
	# sendEmail
	mail <- new("Mail",
			from="claudio.ortelli@usi.ch",
			to=message@advisor@email,
			subject=subject,
			message=stringMessage,
			attachments=message[["fileName"]]
	)
	resultMessage <- sendEMail(mail)
	logger(paste("Mail sent:\n",as.character(mail),sep=""))
	setwd(workdir)
	return(resultMessage)
}
