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
	
	source(file.path(sys[["sourceCodeDir"]],"adviceManagement","lib","secrets.R"))
	
	command <- paste("sendEmail",
			"-f",mail@from,
			"-t",mail@to,
			"-u",shQuote(mail@subject),
			"-m",shQuote(mail@message),
			"-s",.secrets[["Riskmanager"]][["server"]],
			"-xu",.secrets[["Riskmanager"]][["username"]],
			"-xp",.secrets[["Riskmanager"]][["password"]]
	)
	
	if (length(mail@attachments)>0) {
		command <- paste(command,"-a",paste(mail@attachments,collapse=" "))
	}
	
	
	result <- system(command,intern=TRUE,wait=TRUE)
	
	# check that the e-mail was sent successfully
	# nchar(result)<28 means an error occured
	nbChar <- nchar(result[1])
	if (nbChar<28) return(result)
	subStringToCheck <- substr(result[1],nbChar-28+1,nbChar)

	isOk <- subStringToCheck == "Email was sent successfully!"
	if (length(isOk)==1 & isOk[1]) return(subStringToCheck) else return(result)
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

sendEmail_newAdviceDeleted <- function(fileName,message) {
	
	messageText <- paste(
			"The advice\n",
			"\n",
			as.character(message),
			"\n\n",
			"has been deleted because of a pending advice. Resend a new advice\n",
			"after execution and confirmation of the pending one.",
			sep=""
	)
	
	# create the e-mail
	mail <- new("Mail",
			from="claudio.ortelli@usi.ch",
			to=message@advisor@email,
			subject="Advice refused",
			message=messageText
	)
	sendEMail(mail)
	logger(paste("Mail sent:\n\n",as.character(mail),sep=""))
	
}

sendEmail_preComplianceResult <- function(message) {
	
	isTestOk <- message[["testResult"]] == "1"
	
	workingDirectoryOrig <- getwd()
	
	if (isTestOk) {
		setwd(file.path(sys[["homeDir"]],"archive","processed","accepted"))
	} else {
		setwd(file.path(sys[["homeDir"]],"archive","processed","rejected"))
	}
	
	if (isTestOk) {
		stringMessage <- paste(
				"The advice\n",
				"\n",
				as.character(message),
				"\n\n",
				"has been accepted for execution.\n",
				"See the attached zip file for more information.",
				sep=""
		)
		subject <- "New advice accepted"
	} else {
		stringMessage <- paste(
				"The advice\n",
				"\n",
				as.character(message),
				"\n\n",
				"has been rejected because of a negative pre-compliance.\n",
				"See the attached zip file for more information.",sep="")
		subject <- "New advice rejected"
	}
	
	# sendEmail
	mail <- new("Mail",
			from=.secrets[["Riskmanager"]][["username"]],
			to=message@advisor@email,
			subject=subject,
			message=stringMessage,
			attachments=message[["fileName"]]
	)
	resultMessage <- sendEMail(mail)
	
	setwd(workingDirectoryOrig)
	return(resultMessage)
}

sendEmail_postComplianceResult <- function(message) {
	
	isTestOk <- message[["testResult"]] == "1"
	
	workingDirectoryOrig <- getwd()
	
	if (isTestOk) {
		setwd(file.path(sys[["homeDir"]],"archive","processed","accepted"))
	} else {
		setwd(file.path(sys[["homeDir"]],"archive","processed","rejected"))
	}
	
	if (isTestOk) {
		stringMessage <- paste(
				"The advice\n",
				"\n",
				as.character(message),
				"\n\n",
				"has been accepted.\n",
				"See attached zip file for more information.",
				sep=""
		)
		subject <- "Confirmation advice accepted"
	} else {
		stringMessage <- paste(
				"the advice\n",
				"\n",
				as.character(message),
				"\n\n",
				"has been rejected because of a negative post-compliance.\n",
				"You have to rebalance the portfolio in order to fulfill all constraints and investment limits.\n",
				"See the attached zip file for more information.",
				sep=""
		)
		subject <- "Confirmation advice rejected"
	}
	
	# sendEmail
	mail <- new("Mail",
			from=.secrets[["Riskmanager"]][["username"]],
			to=message@advisor@email,
			subject=subject,
			message=stringMessage,
			attachments=message[["fileName"]]
	)
	resultMessage <- sendEMail(mail)
	
	setwd(workingDirectoryOrig)
	return(resultMessage)
}
