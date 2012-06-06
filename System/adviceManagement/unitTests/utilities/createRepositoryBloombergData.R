# TODO: Add comment
# 
# Author: claudio
###############################################################################


createRepositoryBloombergData <- function() {
	
	blData <- new("BloombergData")
	
	# create the BloombergData for Equity trade
	blDataEntryEquity <- new("BloombergDataEntry",blId="UBSN VX Equity",fieldId="LAST_PRICE",value=11.08,dateLastUpdate=Sys.time())
	
	blData <- add(blDataEntryEquity,blData)
	
	# create the BloombergData for Future on equity index trade
	blDataEntryFutureEquityIndex1 <- new("BloombergDataEntry",blId="SMM2 Index",fieldId="LAST_PRICE",value=5864,dateLastUpdate=Sys.time())
	blDataEntryFutureEquityIndex2 <- new("BloombergDataEntry",blId="SMM2 Index",fieldId="FUT_VAL_PT",value=10.0,dateLastUpdate=Sys.time())
	blDataEntryFutureEquityIndex3 <- new("BloombergDataEntry",blId="SMM2 Index",fieldId="UNDL_SPOT_TICKER",value="SMI",dateLastUpdate=Sys.time())
	blDataEntryFutureEquityIndex4 <- new("BloombergDataEntry",blId="SMM2 Index",fieldId="FUT_DLV_DT_FIRST",value="06/18/2012",dateLastUpdate=Sys.time())
	
	blData <- add(blDataEntryFutureEquityIndex1,blData)
	blData <- add(blDataEntryFutureEquityIndex2,blData)
	blData <- add(blDataEntryFutureEquityIndex3,blData)
	blData <- add(blDataEntryFutureEquityIndex4,blData)
	
	# create the BloombergData for FX Spot
	blDataEntryFXSpot <- new("BloombergDataEntry",blId="EURCHF Curncy",fieldId="LAST_PRICE",value=1.20072,dateLastUpdate=Sys.time())
	blData <- add(blDataEntryFXSpot,blData)
	return(blData)
	
}
