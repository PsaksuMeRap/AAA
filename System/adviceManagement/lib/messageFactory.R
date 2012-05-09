# TODO: Add comment
# 
# Author: Claudio
###############################################################################


messageFactory <- function(fileName) {
	# identify the main part of the fileName and the extension
	step1 <- strsplit(fileName,"\\.")
	mainPart <- step1[[1]][1]
	extension <- step1[[1]][2]

	# identify the date, time, person name, fund and message type
	step2 <- c(extension,strsplit(mainPart,"_")[[1]],fileName)
	step2 <- as.list(step2)
	
	if (step2[[5]]=="newAdvice") tmp <- new("NewAdvice",step2)
	if (step2[[5]]=="adviceConfirmation") tmp <- new("AdviceConfirmation",step2)
	if (step2[[5]]=="preComplianceResult") tmp <- new("PreComplianceResult",step2)
	if (step2[[5]]=="postComplianceResult") tmp <- new("PostComplianceResult",step2)
	
	names(tmp@.Data) <- c("fileExtension","date","time","from","messageType","fileName")
	
	return(tmp)
}
