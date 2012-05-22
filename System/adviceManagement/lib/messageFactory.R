# TODO: Add comment
# 
# Author: Claudio
###############################################################################


messageFactory <- function(fileName,directory,advisors) {
	# identify the main part of the fileName and the extension

	step1 <- strsplit(fileName,"\\.")
	mainPart <- step1[[1]][1]
	fileExtension <- step1[[1]][2]

	# identify the date, time, person name, fund and message type
	step2 <- c(fileName,fileExtension,strsplit(mainPart,"_")[[1]])
	step2 <- as.list(step2)
	if (is.element(step2[[6]],c("preComplianceResult","postComplianceResult"))) {
		names(step2) <- c("fileName","fileExtension","date","time","from","messageType","testResult")
	} else {
		names(step2) <- c("fileName","fileExtension","date","time","from","messageType")
	}
	advisor <- advisors[[step2[["from"]]]]
	message <- new("Message",advisor=advisor,step2)
	messageType <- step2[["messageType"]]
	if (messageType == "newAdvice") {
		trades <- tradesFactory(fileName,directory)
		return(new("NewAdvice",trades=trades,message))
	}
	if (messageType == "confirmation") {
		trades <- tradesFactory(fileName,directory)
		return(new("Confirmation",trades=trades,message))
	}	
	if (messageType == "preComplianceResult") {
		return(new("PreComplianceResult",message))
	}	
	if (messageType == "postComplianceResult") {
		return(new("PostComplianceResult",message))
	}	

}