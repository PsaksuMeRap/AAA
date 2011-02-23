# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.extractPriceAtDate <- function() {
	
	dateType = "settlementDate"
	dataType = "low"

	data <- data.frame(open=c(1.2,3.4,1),low=c(1.2,3.3,22))
	rownames(data) <- c("2004-06-13","2004-06-14","2004-06-15")

	contract <- create_contract(name="test",
			settlementDate="2004-06-15",
			lastTradeDate="2004-06-14",
			data=data)
	
	result <- extractPriceAtDate(contract,dateType,dataType)
	checkEquals(result,22)
	
	contract$settlementDate <- "2001-06-15"
	result <- extractPriceAtDate(contract,dateType,dataType)
	checkEquals(result,NA)
}

