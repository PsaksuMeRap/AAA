# TODO: Add comment
# 
# Author: claudio
###############################################################################



create_finTsLevel <- function(name,data=NA) {
	
	
	finTsLevel <- new.env()
	class(finTsLevel) <- c("finTsLevel","finTimeseries")
	
	finTsLevel$name <- name
	finTsLevel$data <- data
	
	finTsLevel$verifyPositivity <- function(showErrorMessage=FALSE) {
		## this function verify that the imported prices are larger than 0
		## dataFrame is a data.frame with only data (no dates!)
		
		nome <- finTsLevel$name
		values <- finTsLevel$data[,"value",drop=TRUE]
		isAvailable <- !is.na(values)
		if (sum(isAvailable) < 3) {
			if (showErrorMessage) {
				string = paste("La serie '",nome,"' ha meno di 3 valori validi!",sep="")
				tkmessageBox(message=string,icon="error")
			}
			return(1)     
		}
		largerThenZero <- values[isAvailable] > 0
		if (!all(largerThenZero)) {
			if (showErrorMessage) {		
				string = paste("La serie '",nome,"' ha valori negativi o uguali a zero!",sep="")
				tkmessageBox(message=string,icon="error")
			}
			return(1)
		}
		return(0)
	}
	
	return(finTsLevel)
}
