# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldSendEmailWithoutAttachment <- function() {
	mail <- new("Mail")
	mail@from <- "claudio.ortelli@usi.ch"
	mail@to <- "claudio.ortelli@gmail.com"
	mail@subject <- "Test email da R"
	mail@message <- "Questo e' un test di trasmissione da R senza allegato."
	mail@server <- "mail.usi.ch"

	tmp <- getwd()
	setwd(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.mail"))
	source("secrets.R")
	
	mail@username <- username
	mail@password <- password
	rm(username,password,inherits=TRUE)
	
	result <- sendEMail(mail)
	# result <- "Email was sent successfully!"
	checkEquals(result,"Email was sent successfully!")
	
	setwd(tmp)
}


test.shouldSendEmailWithAttachment <- function() {
	mail <- new("Mail")
	mail@from <- "claudio.ortelli@usi.ch"
	mail@to <- "claudio.ortelli@gmail.com"
	mail@subject <- "Test di trasmissione da R"
	mail@message <- "Questo è un test di trasmissione da R con allegati."
	mail@server <- "mail.usi.ch"
	mail@attachments <- c("file1.txt","file2_da_allegare.R")

	tmp <- getwd()
	setwd(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.mail"))
	source("secrets.R")
	
	mail@username <- username
	mail@password <- password
	rm(username,password,inherits=TRUE)
	
	result <- sendEMail(mail)
	checkEquals(result,"Email was sent successfully!")
	
	setwd(tmp)
}


test.shouldAsCharacterMail <- function() {
	mail <- new("Mail")
	mail@from <- "claudio.ortelli@usi.ch"
	mail@to <- "claudio.ortelli@gmail.com"
	mail@subject <- "Test di trasmissione da R"
	mail@message <- "Questo e' un test di trasmissione da R con allegati."
	mail@server <- "mail.usi.ch"
	mail@attachments <- c("file1.txt","file2_da_allegare.R")
	
	string <- paste("from:",mail@from)
	string[2] <- paste("to:",mail@to)
	string[3] <- paste("subject:",mail@subject)
	string[4] <- paste("message:",mail@message)
	string[5] <- paste("attachments:",mail@attachments[1])
	string[6] <- paste("attachments:",mail@attachments[2])
	
	should <- paste(string,collapse="\n")
	result <- as.character(mail)
	
	checkEquals(result,should)
	
}