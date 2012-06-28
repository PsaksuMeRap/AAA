# TODO: Add comment
# 
# Author: Claudio
###############################################################################


messageFileNameFactory <- function(fileName) {
	# identify the main part of the fileName and the extension
	
	step1 <- strsplit(fileName,"\\.")
	mainPart <- step1[[1]][1]
	fileExtension <- step1[[1]][2]
	
	# identify the date, time, person name, fund and message type
	step2 <- c(fileName,fileExtension,strsplit(mainPart,"_")[[1]])
	step2 <- as.list(step2)
	if (is.element(step2[[7]],c("preComplianceResult","postComplianceResult"))) {
		names(step2) <- c("fileName","fileExtension","date","time","from","portfolioName","messageType","testResult")
	} else {
		names(step2) <- c("fileName","fileExtension","date","time","from","portfolioName","messageType")
	}
	
	if (!is.element(step2[["from"]],names(advisors))) {
		message <- paste("Error: the advisor",step2[["from"]], "is not in the defined list of available advisors:\n")
		advisorsNames <- paste(names(advisors),collapse="\n ")
		stop(paste(message,advisorsNames))
	}
	
	messageFileName <- new("MessageFileName",step2)

}

