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
			"-s",mail@server,
			"-xu ortellic",
			"-xp tega=01",
			"-o message-charset=utf-8"
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


