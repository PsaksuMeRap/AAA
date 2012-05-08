# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldSendEmailWithoutAttachment <- function() {
	mail <- new("Mail")
	mail@from <- "claudio.ortelli@usi.ch"
	mail@to <- "claudio.ortelli@gmail.com"
	mail@subject <- "Test email da R"
	mail@message <- "Questo è un test di trasmissione da R senza allegato."
	mail@server <- "mail.usi.ch"

	tmp <- getwd()
	mySetwd(file.path("adviceManagement","unitTests","t.mail"))
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
	mySetwd(file.path("adviceManagement","unitTests","t.mail"))
	source("secrets.R")
	
	mail@username <- username
	mail@password <- password
	rm(username,password,inherits=TRUE)
	
	result <- sendEMail(mail)
	# result <- "Email was sent successfully!"
	checkEquals(result,"Email was sent successfully!")
	
	setwd(tmp)
}
