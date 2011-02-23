# TODO: Add comment
# 
# Author: claudio
###############################################################################


extractPriceAtDate <- function(contract,dateType,dataType="settlement") {
	# dateType: "settlementDate" or"lastTradeDate"
	
	date <- contract[[dateType]]
	exists <- date == rownames(contract$data)
	if (any(exists)) {
		result <- contract$data[date, dataType]
		return(result)
	}
	
	return(NA)
	
}
