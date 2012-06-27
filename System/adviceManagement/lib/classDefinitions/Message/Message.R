# TODO: Add comment
# 
# Author: Claudio
###############################################################################


setClass("Message",representation(advisor="Advisor"),contains="namedList")

# the namedList contains the following fields:
# "fileName","fileExtension","date","time","from","messageType","testResult"
# "testResult" is optional, only for messages of type "preComplianceResult" and "postComplianceResult" 

setClass("Confirmation",representation(trades="Trades"),contains="Message")
setClass("NewAdvice",representation(trades="Trades"),contains="Message")
setClass("PreComplianceResult",contains="Message")
setClass("PostComplianceResult",contains="Message")


getMessageDate_time_from <- function(message) {
	string <- paste(message[["date"]],message[["time"]],message[["from"]],message[["portfolioName"]],sep="_")
	return(string)
}