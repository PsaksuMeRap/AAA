# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldSetupPostOffice <- function() {
	
	absolutePath <- file.path(getwd(),"adviceManagement","unitTests","directories")
	postOffice <- new("PostOffice",absolutePath=absolutePath)
		
	setup(postOffice)
	
	checkEquals(dir(path=absolutePath),"postOffice")
	checkEquals(dir(file.path(absolutePath,"postOffice")),c("inbox","outbox","sent"))
	unlink(file.path(absolutePath,"postOffice"),recursive=TRUE)
}

test.shouldSetupMailBox <- function() {
	
	absolutePath <- file.path(getwd(),"adviceManagement","unitTests","directories")
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	
	
	setup(x=mailBox,y=postOffice)
	
	directoriesInPostOffice <- dir(path=file.path(absolutePath,"postOffice"))
	checkEquals(is.element(advisor@folderName,directoriesInPostOffice),TRUE)
	
	unlink(file.path(absolutePath,"postOffice"),recursive=TRUE)
	
}