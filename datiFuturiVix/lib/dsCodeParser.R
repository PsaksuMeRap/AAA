# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_dsCodeParser <- function() {
	parser <- new.env()
	class(parser) <- "dsCodeParser"
	
	
	parser$extractContractName <- function(dsCode) {
		nameWithoutType <- substr(dsCode,1,7) 
		month <- substr(dsCode,4,5) 
		year  <- substr(dsCode,6,7)
		dataType <- substr(dsCode,9,10)
		
		attributes <- list(nameWithoutType=nameWithoutType,
				dataType=dataType,
				year=paste("20",year,sep=""),
				month=month)
		return(attributes)
	}
	
	parser$extractNameWithoutType <- function(code) {
		
		attributes <- parser$extractContractName(code)
		nameWithoutType <- attributes$nameWithoutType
		return(nameWithoutType)
	}
	
	return(parser)
}
