# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_importerVixFutures <- function() {
	
	importer <- create_importerVixFutures()
	
	checkEquals(class(importer),"importerExcelDataStreamSheet")
	
}

test.importer_readSettlementDates <- function()
{
	importer <- create_importerVixFutures()
	input <- importer$readSettlementDates()
	
	checkEquals(input[1,1],"2011-03-15")
	checkEquals(input[2,2],"2011-04-20")
}

test.importer_readStartEndFrequency <- function() {
	
	importer <- create_importerVixFutures()
	attributes <- importer$readStartEndFrequency() 
	checkEquals(attributes$startDate,"2004-03-26")
	checkEquals(attributes$endDate,"2011-02-21")
	checkEquals(attributes$frequency,"D")
	
}

test.importer_readDsNames <- function() {
	importer <- create_importerVixFutures()
	names <- importer$readDsNames() 
	
	checkEquals(names[2],"CFE-VIX INDEX MAR 2011 - SETT. PRICE")
	
}

test.importer_readDsCodes <- function() {
	importer <- create_importerVixFutures()
	codes <- importer$readDsCodes() 
	
	checkEquals(codes[3],"CVX0311(PH)")
}

test.importer_getData <- function() {
	importer <- create_importerVixFutures()
	data <- importer$getData()
	
	checkEquals(rownames(data)[1],"2004-03-26")
	checkEquals(rownames(data)[nrow(data)],"2011-02-21")
	checkEquals(nrow(data),1802)
	checkEquals(data[1,1],NA_real_)
}


test.importer_getSettlementDate <- function() {
	period <- "2011-05"
	importer <- create_importerVixFutures()
	
	desiredDate <- importer$getSettlementDate(period)
	checkEquals(desiredDate,"2011-05-18")
}


test.importer_getLastTradeDate <- function() {
	period <- "2011-05"
	importer <- create_importerVixFutures()
	
	desiredDate <- importer$getLastTradeDate(period)
	
	checkEquals(desiredDate,"2011-05-17")
}


test.importer_getListOfContractNames <- function() {
	importer <- create_importerVixFutures()

	dsCodes.v <- c("CVX0311(PO)","CVX0311(PS)","CVX0311(OI)",
			"CVX0311(PH)","CVX0311(PL)","CVX0411(PO)",
			"CVX0411(PS)","CVX0411(OI)")
	
	namesWithoutType <- importer$getListOfContractNames(dsCodes.v)
	
	checkEquals(namesWithoutType,c("CVX0311","CVX0411"))
}

test.importer_extractSingleContract <- function() {
	contractName <- "CVX0510"
	importer <- create_importerVixFutures()
	
	contract <- importer$extractSingleContract(contractName)
	
	checkEquals(contract$name,"CVX0510")
	checkEquals(contract$settlementDate,"2010-05-19")
	checkEquals(contract$lastTradeDate,"2010-05-18")
	checkEquals(colnames(contract$data)[1:2],c("CVX0510(PO)","CVX0510(PS)"))
}

test.importer_extractAllContracts <- function() {

	importer <- create_importerVixFutures()
	dsCodes <- importer$readDsCodes()
	contractNames <- importer$getListOfContractNames(dsCodes)
	
	contracts <- lapply(contractNames,importer$extractSingleContract)
	
	checkEquals(length(contracts),85)
}
