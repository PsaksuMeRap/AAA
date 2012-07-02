# TODO: Add comment
# 
# Author: Claudio
###############################################################################


zipResults <- function(message,testResult,fromDir,toDir) {
	
	if (message[["messageType"]]=="newAdvice") messageType="preComplianceResult"
	if (message[["messageType"]]=="confirmation") messageType="postComplianceResult"
	
	# construct the fileName as date_time_advisor_portfolioName_messageType.zip
	fileName <- paste(message[["date"]],message[["time"]],message[["from"]],
			message[["portfolioName"]],messageType,as.numeric(testResult),sep="_")
	fileName <- paste(fileName,"zip",sep=".")
	
	# zip all results in a single file
	
	zipfile <- file.path(toDir,fileName)
	filesToZip <- list.files(fromDir,full.names=TRUE)
	zip(zipfile,files=filesToZip,flags="-jr9X")
	return(zipfile)
}
