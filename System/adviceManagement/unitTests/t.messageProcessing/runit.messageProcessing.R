# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldProcessNewAdviceMessage <- function() {


}


test.shouldProcessPreComplianceResultMessageNo <- function() {
	
	# create the archive
	create_archive(systemOptions[["homeDir"]])
	
	# identify a new order
	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_preComplianceResult_no.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files")
	
	# define the adivisors
	advisors[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="claudio.ortelli@gmail.com")
	advisors <- new("Advisors")
	
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	# create the mailBox
	mailBox <- new("MailBox",advisor=advisors[["GhidossiGlobalEquity"]])
	setup(x=mailBox,y=postOffice)
	
	# identify the messageType
	message <- messageFactory(fileName,directory,advisors)
	
	# lock
	ok <- lock(message)
	
	# create file
	ok <- file.create(file.path(systemOptions[["homeDir"]],"postOffice",advisors[["GhidossiGlobalEquity"]]@folderName,"pending",message[["fileName"]])) 
	
	messageProcessing(message)
	exists <- file.exists(file.path(systemOptions[["homeDir"]],"archive","processed","rejected",message[["fileName"]]))
	checkEquals(exists,TRUE)
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"archive"),recursive=TRUE)	
	
}


test.shouldProcessPreComplianceResultMessageOk <- function() {
	
	# create the archive
	create_archive(systemOptions[["homeDir"]])
	
	# identify a new order
	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_preComplianceResult_ok.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files")
	
	# define the adivisors
	advisors <- new("Advisors")
	advisors[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="claudio.ortelli@gmail.com")
	
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	# create the mailBox
	mailBox <- new("MailBox",advisor=advisors[["GhidossiGlobalEquity"]])
	setup(x=mailBox,y=postOffice)
	
	# identify the messageType
	message <- messageFactory(fileName,directory,advisors)
	
	# lock
	ok <- lock(message)
	
	ok <- file.create(file.path(systemOptions[["homeDir"]],"postOffice",advisors[["GhidossiGlobalEquity"]]@folderName,"pending",message[["fileName"]])) 
	
	messageProcessing(message)
	exists <- file.exists(file.path(systemOptions[["homeDir"]],"archive","processed","rejected",message[["fileName"]]))
	checkEquals(exists,FALSE)
	exists <- file.exists(file.path(systemOptions[["homeDir"]],"postOffice",advisors[["GhidossiGlobalEquity"]]@folderName,"pending",message[["fileName"]]))
	checkEquals(exists,TRUE)
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	unlink(file.path(systemOptions[["homeDir"]],"archive"),recursive=TRUE)	
	
}


test.shouldProcessConfirmationMessage <- function() {
	# define the adivisors
	advisors <- new("Advisors")
	advisors[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="claudio.ortelli@gmail.com")
	
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	# create the mailBox
	mailBox <- new("MailBox",advisor=advisors[["GhidossiGlobalEquity"]])
	setup(x=mailBox,y=postOffice)
	
	# copy the confirmation message
	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_confirmation.csv"
	fromDir <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files")
	toDir <- file.path(systemOptions[["homeDir"]],"postOffice",advisors[["GhidossiGlobalEquity"]]@folderName,"pending")
	toFile <- file.path(toDir,fileName)
	
	# identify the messageType
	ok <- file.copy(from=file.path(fromDir,fileName),to=toFile)
	message <- messageFactory(fileName,toDir,advisors)
	
	# lock
	ok <- lock(message)
	
	setMethod("messageProcessing",signature(message="Confirmation"),
			function(message) {
				# this method assumes that the confirmation file has been moved
				# from postOffice/inbox to postOffice/mailBox/pending folder
				
				# update the portfolio
				# a) carica l'ultimo portafoglio in database yyyy-mm-dd_hh-mm-ss_nomePortafoglio
				# b) aggiorna il portafoglio
				# c) salva il nuovo portafoglio nel db
			
				updatePortfolio(message)
				logger(paste("Updated portfolio with confirmation message",message[["fileName"]]))
			
				
			}
	
	)
	
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)	
}


test.shouldProcessPostComplianceResultMessage <- function() {
	
}