# TODO: Add comment
# 
# Author: claudio
###############################################################################

create_importerVixFutures <- function() {
	importer <- new.env()
	class(importer) <- "importerExcelDataStreamSheet"
	
	importer$file <- "./unitTests/data/serie.csv"
	importer$settlementFile <- "./unitTests/data/scadenze.csv"

	importer$readSettlementDates <- function() {
		settDates <- read.csv(file = importer$settlementFile, header = TRUE,
				stringsAsFactors = FALSE,skip=1)
		colnames(settDates)[1] <- "Last Trade Date"
		colnames(settDates)[2] <- "Settlement Date"
		
		return(settDates)
	}
	
	importer$readStartEndFrequency <- function() {
		
		input <- read.csv(file = importer$file, header = FALSE,
				stringsAsFactors = FALSE,nrows = 3)
		
		startDate = input[1,2]
		endDate = input[2,2]
		frequency = input[3,2]
		
		attributes <- list(startDate=startDate,endDate=endDate,
				frequency=frequency)
		return(attributes)
	}
	
	importer$readDsNames <- function() {
		
		input <- read.csv(file = importer$file, header = FALSE,
				stringsAsFactors = FALSE,skip=3,nrows = 1)
		
		names = as.vector(input[1,-1],mode="character")
		return(names)
	}
	
	importer$readDsCodes <- function() {
		
		input <- read.csv(file = importer$file, header = FALSE,
				stringsAsFactors = FALSE,skip=4,nrows = 1)
		
		codes = as.vector(input[1,-1],mode="character")
		
		return(codes)
	}
	
	importer$getData <- function() {
		
		input <- read.csv(file = importer$file, header = FALSE,
				stringsAsFactors = FALSE,skip=5)
		
		dates <- input[,1]
		data <- input[,-1,drop=FALSE]
		rownames(data) <- dates
		
		colnames(data) <- importer$readDsCodes()
		return(data)
	}
	
	
	importer$getSettlementDate <- function(period) {
		
		settlementDates <- importer$readSettlementDates()
		
		desired <- substr(settlementDates[,"Settlement Date"],1,7) == period
		desiredDate <- settlementDates[desired,"Settlement Date"]
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
		
		data <- importer$getData()
		codes <- importer$readDsCodes()
		desired <- substr(codes,1,7)==contractName
		data <- data[,desired]
		
		settlementDate <- importer$getSettlementDate(period)
		lastTradeDate <- importer$getLastTradeDate(period)
		contract <- create_contract(name=contractName,settlementDate=settlementDate,
				lastTradeDate=lastTradeDate,data=data)
		return(contract)
	}
	
	return(importer)

}
