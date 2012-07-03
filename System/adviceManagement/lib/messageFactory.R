# TODO: Add comment
# 
# Author: Claudio
###############################################################################


messageFactory <- function(fileName,directory) {
	# identify the main part of the fileName and the extension

	messageFileName <- messageFileNameFactory(fileName)
	
	advisor <- advisors[[messageFileName[["from"]]]]
	message <- new("Message",advisor=advisor,messageFileName)
	messageType <- messageFileName[["messageType"]]
	if (messageType == "newAdvice") {
		trades <- tradesFactory(messageFileName,directory)
		return(new("NewAdvice",trades=trades,message))
	}
	if (messageType == "confirmation") {
		trades <- tradesFactory(messageFileName,directory)
		return(new("Confirmation",trades=trades,message))
	}	
	if (messageType == "preComplianceResult") {
		return(new("PreComplianceResult",message))
	}	
	if (messageType == "postComplianceResult") {
		return(new("PostComplianceResult",message))
	}
}
