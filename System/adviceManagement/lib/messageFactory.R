# TODO: Add comment
# 
# Author: Claudio
###############################################################################


messageFactory <- function(fileName,advisors) {
	# identify the main part of the fileName and the extension
	step1 <- strsplit(fileName,"\\.")
	mainPart <- step1[[1]][1]
	extension <- step1[[1]][2]

	# identify the date, time, person name, fund and message type
	step2 <- c(extension,strsplit(mainPart,"_")[[1]],fileName)
	step2 <- as.list(step2)
	names(step2) <- c("fileExtension","date","time","from","messageType","fileName")
	
	advisor <- advisors[[step2[["from"]]]]
	message <- new("Message",advisor=advisor,step2)
	messageType <- step2[["messageType"]]
	if (messageType =="newAdvice") {
		return(new("NewAdvice",message))
	}
	if (messageType =="adviceConfirmation") {
		return(new("AdviceConfirmation",message))
	}	
	if (messageType =="preComplianceResult") {
		return(new("PreComplianceResult",message))
	}	
	if (messageType =="postComplianceResult") {
		return(new("PostComplianceResult",message))
	}	

}
