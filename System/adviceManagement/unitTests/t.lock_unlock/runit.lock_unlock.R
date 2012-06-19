# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shoudLock <- function() {
	workingDirectory <- getwd()
	
	# identify a new order
	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.lock_unlock")
			
	# define the adivisors
	advisors <- new("Advisors")
	advisors[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="reto.ghidossi@opencapital.ch")
	
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
		
	# check
	lockExists <- file.exists(file.path(systemOptions[["homeDir"]],"postOffice",message@advisor@folderName,"lock"))
	checkEquals(lockExists,TRUE)
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	setwd(workingDirectory)
}


test.shoudUnLock <- function() {
	workingDirectory <- getwd()
	
	# identify a new order
	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.lock_unlock")
	
	# define the adivisors
	advisors <- new("Advisors")
	advisors[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="reto.ghidossi@opencapital.ch")
	
	# create the postOffice
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	# create the mailBox
	mailBox <- new("MailBox",advisor=advisors[["GhidossiGlobalEquity"]])
	setup(x=mailBox,y=postOffice)
	
	# identify the messageType
	message <- messageFactory(fileName,directory,advisors)
	
	# create the lock (previously tested)
	ok <- lock(message)
	lockExists <- file.exists(file.path(systemOptions[["homeDir"]],"postOffice",message@advisor@folderName,"lock"))
	checkEquals(lockExists,TRUE)
	
	# unlock
	ok <- unlock(message)
	lockExists <- file.exists(file.path(systemOptions[["homeDir"]],"postOffice",message@advisor@folderName,"lock"))
	
	# check
	checkEquals(lockExists,FALSE)
	
	# clean
	unlink(file.path(systemOptions[["homeDir"]],"postOffice"),recursive=TRUE)
	setwd(workingDirectory)
}
