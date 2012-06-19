# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldSetupPostOffice <- function() {

	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
		
	setup(postOffice)
	
	checkEquals(is.element("postOffice",dir(path=absolutePath)),TRUE)
	checkEquals(dir(file.path(absolutePath,"postOffice")),c("inbox","pending"))
	
	# remove the directory postOffice
	tmp <- getwd()
	setwd(absolutePath)
	unlink("postOffice",recursive=TRUE)
	setwd(tmp)
}

test.shouldSetupMailBox <- function() {
	
	absolutePath <- systemOptions[["homeDir"]]
	postOffice <- new("PostOffice",absolutePath=absolutePath)
	setup(postOffice)
	
	advisor <- new("Advisor",name="testAdvisor1",folderName="testAdvisor1",email="claudio.ortelli@usi.ch")
	mailBox <- new("MailBox",advisor=advisor)
	
	
	setup(x=mailBox,y=postOffice)
	
	directoriesInPostOffice <- dir(path=file.path(systemOptions[["homeDir"]],"postOffice"))
	checkEquals(is.element(advisor@folderName,directoriesInPostOffice),TRUE)
	
	tmp <- getwd()
	setwd(systemOptions[["homeDir"]])
	unlink("postOffice",recursive=TRUE)
	setwd(tmp)
}