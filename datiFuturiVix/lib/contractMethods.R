# TODO: Add comment
# 
# Author: claudio
###############################################################################


extractPriceAtSettlementOrLastTradeDate <- function(contract,dateType,dataType="settlement") {
	# dateType: "settlementDate" or"lastTradeDate"
	
	date <- contract[[dateType]]
	exists <- date == rownames(contract$data)
	if (any(exists)) {
		result <- contract$data[date, dataType]
		return(result)
	}
	
	return(NA)
}

extractPriceAtSpecificDate <- function(contract,date,dataType="settlement") {
	# date: "2010-05-12"
	
	exists <- date == rownames(contract$data)
	if (any(exists)) {
		result <- contract$data[date, dataType]
		return(result)
	}
	
	return(NA)
}


extractDatePreviousToSettlement <- function(contract,nbPeriods) {
	if (nbPeriods < 0) return(NA)
	isSettlementDate <- rownames(contract$data) == contract$settlementDate
	
	if (any(isSettlementDate)) {
		position <- (1:length(isSettlementDate))[isSettlementDate]
		positionDesiredDate <- position - nbPeriods
		if (positionDesiredDate>0) {
			result <- rownames(contract$data)[positionDesiredDate]
		} else {
			result <- NA
		}
	} else {
		result <- NA
	}
	
	return(result)
}

extractPricePreviousToSettlement <- function(contract,nbPeriods,dataType) {
	
	if (nbPeriods < 0) return(NA)
	
	isSettlementDate <- rownames(contract$data) == contract$settlementDate
	
	if (any(isSettlementDate)) {
		position <- (1:length(isSettlementDate))[isSettlementDate]
		positionDesiredPrice <- position - nbPeriods
		if (positionDesiredPrice>0) {
			result <- contract$data[positionDesiredPrice,dataType]
		} else {
			result <- NA
		}
	} else {
		result <- NA
	}
	
	return(result)
}
