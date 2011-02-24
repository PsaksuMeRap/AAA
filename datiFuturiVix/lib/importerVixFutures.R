# TODO: Add comment
# 
# Author: claudio
###############################################################################

create_importerVixFutures <- function() {
	importer <- create_importer()
	class(importer) <- "importerExcelDataStreamSheet"
	
	importer$file <- "./unitTests/data/serie.csv"
	importer$settlementFile <- "./unitTests/data/scadenze.csv"

	importer$knownDataTypes <- c(PO="open",
			PS="settlement",PH="high",PL="low",
			OI="openInterest")
	
	importer$readSettlementDates <- function() {
		settDates <- read.csv(file = importer$settlementFile, header = TRUE,
				stringsAsFactors = FALSE,skip=1)
		colnames(settDates)[1] <- "Last Trade Date"
		colnames(settDates)[2] <- "Settlement Date"
		
		importer$repository$settlementDates <<- settDates
	}
		
	
	importer$getSettlementDate <- function(period) {
		
		if (!exists("settlementDates",where=importer$repository,inherits=FALSE)) {
			importer$readSettlementDates()
		}
		
		desired <- substr(importer$repository$settlementDates[,"Settlement Date"],1,7) == period
		desiredDate <- importer$repository$settlementDates[desired,"Settlement Date"]
		return(desiredDate)
	}
	
	
	importer$getLastTradeDate <- function(period) {
		
		settlementDates <- importer$readSettlementDates()
		
		desired <- substr(settlementDates[,"Last Trade Date"],1,7) == period
		desiredDate <- settlementDates[desired,"Last Trade Date"]
		return(desiredDate)
	}
	
	importer$getListOfContractNames <- function(dsCodes.v) {
		
		parser <- create_dsCodeParser()
		
		namesWithoutType <- sapply(dsCodes.v,
				parser$extractNameWithoutType)
		
		namesWithoutType <- unique(namesWithoutType)
		
		return(namesWithoutType)
	}
	
	importer$extractSingleContract <- function(contractName) {
		year <- paste("20",substr(contractName,6,7),sep="")
		month <- substr(contractName,4,5)
		period <- paste(year,"-",month,sep="")
		rm(year,month)
		
		if (!exists("data",where=importer$repository,inherits = FALSE)) {
			importer$getData()
		}
		data <- importer$repository$data
		
		if (!exists("dsCodes",where=importer$repository,inherits = FALSE)) {
			importer$readDsCodes()
		}	
		codes <- importer$repository$dsCodes
		
		
		desired <- substr(codes,1,7)==contractName
		data <- data[,desired]

		# parse the dsCodes and rename the timeseries headers accordingly
		parser <- create_dsCodeParser()
		dsCodes <- colnames(data)
		attributes <- lapply(dsCodes,parser$extractContractName)

		values <- extractLists(attributes,fieldName="dataType") 
		colnames(data) <- importer$knownDataTypes[values]
		
		# construct the contract object
		settlementDate <- importer$getSettlementDate(period)

		lastTradeDate <- importer$getLastTradeDate(period)
		contract <- create_contract(name=contractName,settlementDate=settlementDate,
				lastTradeDate=lastTradeDate,data=data)
		return(contract)
	}
	
	importer$extractAllContracts <- function() {

		dsCodes <- importer$readDsCodes()
		contractNames <- importer$getListOfContractNames(dsCodes)
			
		contracts <- lapply(contractNames,importer$extractSingleContract)
		return(contracts)
	}
	
	
	return(importer)

}
