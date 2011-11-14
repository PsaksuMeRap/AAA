# TODO: Add comment
# 
# Author: claudio
###############################################################################


extractPriceAtSettlementDate <- function(contract,dataType="settlement") {
	
	date <- contract[["settlementDate"]]
	exists <- date == rownames(contract$data)
	if (any(exists)) {
		result <- contract$data[date, dataType]
		return(result)
	}
	
	return(NA_real_)
}

extractPriceAtLastTradeDate <- function(contract,dataType="settlement") {
	
	date <- contract[["lastTradeDate"]]
	exists <- date == rownames(contract$data)
	if (any(exists)) {
		result <- contract$data[date, dataType]
		return(result)
	}
	
	return(NA_real_)
}


extractPriceAtSpecificDate <- function(contract,date,dataType="settlement") {
	# date: "2010-05-12"
	
	exists <- date == rownames(contract$data)
	if (any(exists)) {
		result <- contract$data[date, dataType]
		return(result)
	}
	
	return(NA_real_)
}


extractDatePreviousToSettlement <- function(contract,nbPeriods,dateLimit) {
	# contract: il contratto da cui estrarre la data
	# nbPeriods: quanti periodi prima del settlement?
	# dateLimit: se la settlement date del contratto è dopo dateLimit non 
	#            considerare il contratto
	
	if (nbPeriods < 0) return(NA)
	
	if (!missing(dateLimit)) {
		if (as.Date(dateLimit)<as.Date(contract$settlementDate)) {
			return(NA)
		}
	}
	dates <- sort(unique(c(rownames(contract$data),contract$settlementDate)),decreasing=TRUE)
	if (nbPeriods >= length(dates)) return(NA)
	return(dates[nbPeriods+1])
}

extractPriceAndDatePreviousToSettlement <- function(contract,nbPeriods,dateLimit,dataType="settlement") {
	# contract: il contratto da cui estrarre la data
	# nbPeriods: quanti periodi prima del settlement?
	# dateLimit: se la settlement date del contratto è dopo dateLimit non 
	#            considerare il contratto
	
	if (nbPeriods < 0) return(list(date=NA,price=NA))
	
	if (!missing(dateLimit)) {
		if (as.Date(dateLimit)<as.Date(contract$settlementDate)) {
			return(list(date=NA,price=NA))
		}
	}
	
	dates <- sort(unique(c(rownames(contract$data),contract$settlementDate)),decreasing=TRUE)
	if (nbPeriods >= length(dates)) return(list(date=NA,price=NA))
	
	desiredDate <- dates[nbPeriods+1]
	desiredPrice <- contract$data[desiredDate,dataType]
	
	return(list(date=desiredDate,price=desiredPrice))
}


extractPriceAndDatePreviousToSettlement.df <- function(contracts,nbPeriods,dateLimit,dataType="settlement") {
	
	if (missing(dateLimit)) {
		datesAndPrices <- lapply(contracts,extractPriceAndDatePreviousToSettlement,nbPeriods,dataType)
	} else {
		datesAndPrices <- lapply(contracts,extractPriceAndDatePreviousToSettlement,nbPeriods,dateLimit,dataType)
	}
	datesAndPrices <- datesAndPrices[sapply(datesAndPrices,function(x){return(!is.na(x$date))})]
	tmp <- data.frame(price=vector(mode = "numeric"))
	date <- vector(mode="character",length=length(datesAndPrices))
	for (i in 1:length(datesAndPrices)) {
		date[i] <- datesAndPrices[[i]]$date
		tmp <- rbind(tmp,data.frame(price=datesAndPrices[[i]]$price))
	}
	rownames(tmp) <- date
	result <- list(name=paste("Prezzo",nbPeriods, "periodi prima del settlement"),data=tmp)
	class(result) <- "dsTimeseries"
	return(result)
}


sortContractsBySettlementDate <- function(contracts) {
	
	settlementDates <- extractFromList(contracts,fieldName="settlementDate")
	orderSettlementDates <- order(as.Date(settlementDates))
	contracts <- contracts[orderSettlementDates]
	return(contracts)
	
}


extractContractWithYM <- function(contracts,desiredYM) {
	## desiredYM = "2004-12"
	
	settlementDates <- extractFromList(origin=contracts,"settlementDate") 
	areOk <- substr(settlementDates,1,7)==desiredYM
	contract <- contracts[areOk]
	
	return(contract) # may be of length > 1!
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

extractPriceOfNextContract <- function(desiredContractYM,contracts,desiredDate,dataType="settlement") {
	## desiredContractYM <- "2004-04"
	## desiredDate <- "2004-04-14"
	## dataType <- "uno"

	year <- as.numeric(substr(desiredContractYM,1,4))
	month <- as.numeric(substr(desiredContractYM,6,7))
	
	if (month==12) {
		year <- year+1
		dateString <- paste(year,"01",sep="-")
	}
	if (month<=8) {
		dateString <- paste(year,"-0",month+1,sep="")
	}
	if (month>=9 & month <= 11) {
		dateString <- paste(year,"-",month+1,sep="")
	}
	
	contract <- extractContractWithYM(contracts,desiredYM=dateString)
	if (length(contract)>=1) {
		price <- extractPriceAtSpecificDate(contract[[1]],date=desiredDate,dataType=dataType)
	} else {
		price <- NA
	}

	return(price)
	
}

computeDaysToSettlement <- function(date,contract) {
	
	days <- as.Date(contract$settlementDate) - as.Date(date)
	return(as.integer(days))
}
