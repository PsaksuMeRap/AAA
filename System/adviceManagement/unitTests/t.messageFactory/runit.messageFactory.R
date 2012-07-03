# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldCreateMessage <- function() {
	# identify a new order
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.messageFactory")
	
	result <- messageFactory(messageFileName,directory)
	
	checkEquals(result[["fileExtension"]],"csv")
	checkEquals(result[["date"]],"2012-05-09")
	checkEquals(result[["time"]],"14-22-24")	
	checkEquals(result[["from"]],"Ortelli")
	checkEquals(result[["portfolioName"]],"globalEquity")	
	checkEquals(result[["messageType"]],"newAdvice")
	checkEquals(result[["fileName"]],"2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv")
	checkEquals(result@advisor,advisors[["Ortelli"]])
	checkEquals(is(result,"NewAdvice"),TRUE)
	checkEquals(result@trades[[1]]$securityID,"RocheGA")
	
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_confirmation.csv"
	messageFileName <- messageFileNameFactory(fileName)
	result <- messageFactory(messageFileName,directory)
	checkEquals(is(result,"Confirmation"),TRUE)
	checkEquals(result@trades[[2]]$securityID,"SMI")
	
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_preComplianceResult_1.csv"
	messageFileName <- messageFileNameFactory(fileName)
	result <- messageFactory(messageFileName,directory)
	checkEquals(is(result,"PreComplianceResult"),TRUE)
	
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_postComplianceResult_0.csv"
	messageFileName <- messageFileNameFactory(fileName)
	result <- messageFactory(messageFileName,directory)
	checkEquals(is(result,"PostComplianceResult"),TRUE)
	
}


test.shouldTestGetMessageDate_time_from <- function() {
	# identify a new order
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv"
	messageFileName <- messageFileNameFactory(fileName)
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.messageFactory")
	
	message <- messageFactory(messageFileName,directory)
	
	string <- getMessageDate_time_from(message)
	checkEquals(string,"2012-05-09_14-22-24_Ortelli_globalEquity")
	
}
