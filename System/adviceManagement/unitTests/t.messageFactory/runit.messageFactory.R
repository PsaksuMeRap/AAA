# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldCreateMessage <- function() {
	# identify a new order
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.messageFactory")
	
	result <- messageFactory(fileName,directory,advisors)
	
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
	result <- messageFactory(fileName,directory,advisors)
	checkEquals(is(result,"Confirmation"),TRUE)
	checkEquals(result@trades[[2]]$securityID,"SMI")
	
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_preComplianceResult_1.csv"
	result <- messageFactory(fileName,directory,advisors)
	checkEquals(is(result,"PreComplianceResult"),TRUE)
	
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_postComplianceResult_0.csv"
	result <- messageFactory(fileName,directory,advisors)
	checkEquals(is(result,"PostComplianceResult"),TRUE)
	
}


test.shouldTestGetMessageDate_time_from <- function() {
	# identify a new order
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.messageFactory")
	
	message <- messageFactory(fileName,directory,advisors)
	
	string <- getMessageDate_time_from(message)
	checkEquals(string,"2012-05-09_14-22-24_Ortelli_globalEquity")
	
}

test.shouldFailWithInvalidAdisor <- function() {
	# identify a new order
	fileName <- "2012-05-09_14-22-24_Xxxx_globalEquity_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.messageFactory")
	
	checkException(messageFactory(fileName,directory,advisors))
	
	
}
