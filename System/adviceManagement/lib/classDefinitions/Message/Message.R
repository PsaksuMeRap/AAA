# TODO: Add comment
# 
# Author: Claudio
###############################################################################


setClass("Message",representation(advisor="Advisor"),contains="MessageFileName")

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

setMethod("as.character","Message", 
		function(x) {
			string <- paste("Time:",x$time)
			string[2] <- paste("Date:",x$date)
			string[3] <- paste("Advisor:",x$from)
			string[4] <- paste("Portfolio:",x$portfolioName)
			string[5] <- paste("Message type:",x$messageType)
 
			return(paste(string,collapse="\n"))
		}
)
