# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_contract <- function(name=NA_character_,settlementDate=NA_character_,
					lastTradeDate=NA_character_, data=NA) {
	contract <- new.env()
	class(contract) <- "Future"

	contract$name <- name
	contract$settlementDate <- settlementDate
	contract$lastTradeDate <- lastTradeDate
	contract$data <- data
	
	return(contract)
	
}
