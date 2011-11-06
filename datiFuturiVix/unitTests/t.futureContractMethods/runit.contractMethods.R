# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.extractPriceAtSettlementDate <- function() {
	
	dataType = "low"

	data <- data.frame(open=c(1.2,3.4,1),low=c(1.2,3.3,22))
	rownames(data) <- c("2004-06-13","2004-06-14","2004-06-15")

	contract <- create_contract(name="test",
			settlementDate="2004-06-15",
			lastTradeDate="2004-06-14",
			data=data)
	
	result <- extractPriceAtSettlementDate(contract,dataType)
	checkEquals(result,22)

	dataType = "open"
	contract$settlementDate <- "2001-06-15"
	result <- extractPriceAtSettlementDate(contract,dataType)
	checkEquals(result,NA_real_)
}


test.extractPriceAtLastTradeDate <- function() {
	
	dataType = "low"
	
	data <- data.frame(open=c(1.2,3.4,1),low=c(1.2,3.3,22))
	rownames(data) <- c("2004-06-13","2004-06-14","2004-06-15")
	
	contract <- create_contract(name="test",
			settlementDate="2004-06-15",
			lastTradeDate="2004-06-14",
			data=data)
	
	result <- extractPriceAtLastTradeDate(contract,dataType)
	checkEquals(result,3.3)
	
	contract$lastTradeDate <- "2001-06-15"
	result <- extractPriceAtLastTradeDate(contract,dataType)
	checkEquals(result,NA_real_)
}



test.extractPriceAtSpecificDate <- function() {
	date = "2011-01-28"
	dataType = "open"

	importer <- create_importerVixFutures()
	
	desiredContract <- importer$extractSingleContract("CVX0311")
	result <- extractPriceAtSpecificDate (desiredContract,date,dataType)
	checkEquals(result,19.38)

	dataType = "high"
	result <- extractPriceAtSpecificDate (desiredContract,date,dataType)
	checkEquals(result,20.9)
	
	date <- "2000-06-15"
	result <-  extractPriceAtSpecificDate (desiredContract,date,dataType)
	checkEquals(result,NA_real_)
}


test.extractDatePreviousToSettlement <- function() {
	nbPeriods <- 5
	
	importer <- create_importerVixFutures()
	
	contract <- importer$extractSingleContract("CVX0504")
	
	result <- extractDatePreviousToSettlement(contract,nbPeriods)
	
	checkEquals(result,"2004-05-12")
	
	nbPeriods <- 1000000
	result <- extractDatePreviousToSettlement(contract,nbPeriods)
	checkEquals(result,NA)
	
	nbPeriods <- 3
	dateLast <- "2004-03-12"
	result <- extractDatePreviousToSettlement(contract,nbPeriods,dateLast)
	checkEquals(result,NA)	

}


test.extractPriceAndDatePreviousToSettlement <- function() {
	
	importer <- create_importerVixFutures()
	contract <- importer$extractSingleContract("CVX0504")
	nbPeriods <- 5
	
	result <- extractPriceAndDatePreviousToSettlement(contract,nbPeriods)
	
	checkEquals(result,list(date="2004-05-12",price=18.2))
	
}


test.extractPricePreviousToSettlement <- function() {
	nbPeriods <- 5
	dataType="settlement"
	importer <- create_importerVixFutures()
	
	contract <- importer$extractSingleContract("CVX0504")
	
	result <- extractPricePreviousToSettlement(contract,nbPeriods,dataType)
	
	checkEquals(result,18.2)
	
	nbPeriods <- 1000000
	result <- extractPricePreviousToSettlement(contract,nbPeriods)
	checkEquals(result,NA)
}


test.sortContractsBySettlementDate <- function() {
	contract1 <- create_contract(name="test1",
			settlementDate="2004-06-15",
			lastTradeDate="2004-06-14",
			data=data.frame())
	contract2 <- create_contract(name="test2",
			settlementDate="2004-03-15",
			lastTradeDate="2004-03-14",
			data=data.frame())
	
	contracts <- list(contract1,contract2)
	
	contracts <- sortContractsBySettlementDate(contracts)
	
	checkEquals(contracts,list(contract2,contract1))
	
}


test.extractContractWithYM <- function() {
	data <- data.frame(uno=c(0,-1,-4),tre=c(-0.3,10000.0,200000))
	rownames(data) <- c("2004-04-13","2004-04-14","2004-04-15")		
	contract1 <- create_contract(name="test1",
			settlementDate="2004-06-15",
			lastTradeDate="2004-06-14",
			data=data.frame())
	
	data <- data.frame(uno=c(3,1,4),tre=c(0.3,0.3,0.2))
	rownames(data) <- c("2004-04-13","2004-04-14","2004-04-15")	
	contract2 <- create_contract(name="test2",
			settlementDate="2004-05-15",
			lastTradeDate="2004-05-14",
			data=data)
	
	contracts <- list(contract1,contract2)
	
	desiredYM <- "2004-05"
	contract <- extractContractWithYM(contracts,desiredYM)
	
	checkEquals(contract[[1]],contract2)
} 


test.extractPriceOfNextContract <- function() {
	
	data <- data.frame(uno=c(0,-1,-4),tre=c(-0.3,10000.0,200000))
	rownames(data) <- c("2004-04-13","2004-04-14","2004-04-15")		
	contract1 <- create_contract(name="test1",
			settlementDate="2004-06-15",
			lastTradeDate="2004-06-14",
			data=data.frame())
	
	data <- data.frame(uno=c(3,1.123,4),tre=c(0.3,0.3,0.2))
	rownames(data) <- c("2004-04-13","2004-04-14","2004-04-15")	
	contract2 <- create_contract(name="test2",
			settlementDate="2004-05-15",
			lastTradeDate="2004-05-14",
			data=data)
	
	data <- data.frame(uno=c(1,2,3),tre=c(4,5,6))
	rownames(data) <- c("2004-04-14","2004-04-15","2004-04-16")
	contract3 <- create_contract(name="test3",
			settlementDate="2004-04-16",
			lastTradeDate="2004-04-15",
			data=data)
	
	contracts <- list(contract2,contract1,contract3)
	desiredContractYM <- "2004-04"
	desiredDate <- "2004-04-14"
	dataType <- "uno"

	price <- extractPriceOfNextContract(desiredContractYM,contracts,desiredDate,dataType)
	
	checkEquals(price,1.123)
		
}


test.computeDaysToSettlement <- function() {
	contract <- create_contract(name="test",
			settlementDate="2004-04-16",
			lastTradeDate="2004-04-15",
			data=data)
	
	date <- "2004-04-14"
	nbDays <- computeDaysToSettlement(date,contract)
	checkEquals(nbDays,2)
}


test.computeDaysToSettlement <- function() {
	contract <- create_contract(name="test",
			settlementDate="2004-04-16",
			lastTradeDate="2004-04-15",
			data=data)
	
	date <- "2004-04-14"
	nbDays <- computeDaysToSettlement(date,contract)
	checkEquals(nbDays,2)
}

