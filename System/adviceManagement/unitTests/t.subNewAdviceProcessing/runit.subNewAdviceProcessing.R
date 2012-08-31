# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldProcessOrder <- function() {
	# copy the data directory
	from <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data")
	to <- file.path(sys[["homeDir"]])
	isOk <- file.copy(from,to,recursive=TRUE)
	
	# setup the archive
	create_archive(sys[["homeDir"]])
					
	# setup the directories and messages	
	postOffice <- new("PostOffice",absolutePath=sys[["homeDir"]])
	setup(postOffice)
	
	mailBox <- new("MailBox",folderName="globalEconomy")
	setup(x=mailBox,y=postOffice)
	
	# copy the file with the new orders
	fileName <- "2012-06-19_14-27-47_Ortelli_globalEconomy_newAdvice.csv"
	from <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.subNewAdviceProcessing",fileName)
	to <- file.path(sys[["homeDir"]],"postOffice","globalEconomy","pending",fileName)
	isOk <- file.copy(from,to)
	
	test <- function() {
		source(file.path(sys[["sourceCodeDir"]],"adviceManagement","lib","subNewAdviceProcessing.R"))
	}
	
	setwd(sys[["sourceCodeDir"]])
	test()
	
	setwd(sys[["sourceCodeDir"]])
	
	# check the
	file <- list.files(file.path(sys[["homeDir"]],"log"))
	checkEquals(substr(file,32,nchar(file)),"2012-06-19_14-27-47_Ortelli_globalEconomy_newAdvice_log.txt")
	
	file <- list.files(file.path(sys[["homeDir"]],"postOffice","inbox"))
	checkEquals(file,"2012-06-19_14-27-47_Ortelli_globalEconomy_preComplianceResult_0.zip")	
	
	file <- list.files(file.path(sys[["homeDir"]],"postOffice","globalEconomy"))
	checkEquals(file,"pending")	
	
	file <- list.files(file.path(sys[["homeDir"]],"postOffice","globalEconomy","pending"))
	checkEquals(file,character(0))	
	
	file <- list.files(file.path(sys[["homeDir"]],"archive","processed","rejected"))
	checkEquals(file,"2012-06-19_14-27-47_Ortelli_globalEconomy_preComplianceResult_0.zip")
	
	unlink(file.path(sys[["homeDir"]],"archive"),recursive=TRUE)
	unlink(file.path(sys[["homeDir"]],"log"),recursive=TRUE)
	unlink(file.path(sys[["homeDir"]],"data"),recursive=TRUE)
	unlink(file.path(sys[["homeDir"]],"postOffice"),recursive=TRUE)
}
