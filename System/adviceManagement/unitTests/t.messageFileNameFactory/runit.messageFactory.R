# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldCreateMessageFileName <- function() {
	# identify a new order
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv"
	
	result <- messageFileNameFactory(fileName)
	
	checkEquals(result[["fileExtension"]],"csv")
	checkEquals(result[["date"]],"2012-05-09")
	checkEquals(result[["time"]],"14-22-24")	
	checkEquals(result[["from"]],"Ortelli")
	checkEquals(result[["portfolioName"]],"globalEquity")	
	checkEquals(result[["messageType"]],"newAdvice")
	checkEquals(result[["fileName"]],"2012-05-09_14-22-24_Ortelli_globalEquity_newAdvice.csv")
	
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_confirmation.csv"
	result <- messageFileNameFactory(fileName)
	checkEquals(result[["messageType"]],"confirmation")

	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_preComplianceResult_1.csv"
	result <- messageFactory(fileName,directory,advisors)
	checkEquals(result[["messageType"]],"preComplianceResult")
	checkEquals(result[["testResult"]],"1")	
	
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEquity_postComplianceResult_0.csv"
	result <- messageFactory(fileName,directory,advisors)
	checkEquals(result[["testResult"]],"0")	
	
}


test.shouldFailWithInvalidAdvisor <- function() {

	fileName <- "2012-05-09_14-22-24_Xxxx_globalEquity_newAdvice.csv"
	checkException(messageFileNameFactory(fileName))

}
