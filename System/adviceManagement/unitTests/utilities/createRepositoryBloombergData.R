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
	blDataEntryFutureEquityIndex2 <- new("BloombergDataEntry",blId="SMM2 Index",fieldId="FUT_TICK_VALUE",value=10,dateLastUpdate=Sys.time())
	blDataEntryFutureEquityIndex3 <- new("BloombergDataEntry",blId="SMM2 Index",fieldId="OPT_UNDL_TICKER",value=,dateLastUpdate=Sys.time())
	blDataEntryFutureEquityIndex3 <- new("BloombergDataEntry",blId="SMM2 Index",fieldId="FUT_DLV_DT_FIRST",value=,dateLastUpdate=Sys.time())
	
	blData <- add(blDataEntryEquity,blData)
	
	
	return(blData)
	
}
