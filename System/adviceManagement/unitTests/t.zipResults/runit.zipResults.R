# TODO: Add comment
# 
# Author: Claudio
###############################################################################



test.shouldZipFiles <- function() {
	
	message <-list(date="2012-11-11",time="03-34-12",from="Ortelli",
			portfolioName="globalFixedIncome",messageType="newAdvice")
	
	toDir <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement",
			"unitTests","t.zipResults")
	fromDir <-  file.path(systemOptions[["sourceCodeDir"]],"adviceManagement",
			"unitTests","t.zipResults","tmp")
	
	# test a newAdvice messageType
	zipResults(message,TRUE,fromDir,toDir)
	fileName <- "2012-11-11_03-34-12_Ortelli_globalFixedIncome_preComplianceResult_1.zip"
	isOk <- file.exists(file.path(toDir,fileName))
	checkEquals(isOk,TRUE)
	unlink(file.path(toDir,fileName))
	
	# test a confirmation messageType
	message[["messageType"]] <- "confirmation"
	zipResults(message,FALSE,fromDir,toDir)
	fileName <- "2012-11-11_03-34-12_Ortelli_globalFixedIncome_postComplianceResult_0.zip"
	isOk <- file.exists(file.path(toDir,fileName))
	checkEquals(isOk,TRUE)
	
	unlink(file.path(toDir,fileName))
}