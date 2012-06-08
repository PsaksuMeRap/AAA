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
	
	# create the BloombergData for Bond
	blDataEntryBond1 <- new("BloombergDataEntry",blId="eib 01/09/15 corp",fieldId="LAST_PRICE",value=102.284,dateLastUpdate=Sys.time())
	blDataEntryBond2 <- new("BloombergDataEntry",blId="eib 01/09/15 corp",fieldId="INT_ACC",value=1.7,dateLastUpdate=Sys.time())
	blDataEntryBond3 <- new("BloombergDataEntry",blId="eib 01/09/15 corp",fieldId="RTG_SP",value="AA",dateLastUpdate=Sys.time())
	blDataEntryBond4 <- new("BloombergDataEntry",blId="eib 01/09/15 corp",fieldId="MATURITY",value="05/08/2013",dateLastUpdate=Sys.time())
	
	blData <- add(blDataEntryBond1,blData)
	blData <- add(blDataEntryBond2,blData)
	blData <- add(blDataEntryBond3,blData)
	blData <- add(blDataEntryBond4,blData)
	
	
	# create the BloombergData for Bond
	blDataEntryOption1 <- new("BloombergDataEntry",blId="nesn sw 06/15/12 c55 equity",fieldId="LAST_PRICE",value=0.16,dateLastUpdate=Sys.time())
	blDataEntryOption2 <- new("BloombergDataEntry",blId="nesn sw 06/15/12 c55 equity",fieldId="OPT_UNDL_TICKER",value="NESN VX",dateLastUpdate=Sys.time())
	blDataEntryOption3 <- new("BloombergDataEntry",blId="nesn sw 06/15/12 c55 equity",fieldId="OPT_EXPIRE_DT",value="06/05/2012",dateLastUpdate=Sys.time())
	blDataEntryOption4 <- new("BloombergDataEntry",blId="nesn sw 06/15/12 c55 equity",fieldId="OPT_STRIKE_PX",value=55,dateLastUpdate=Sys.time())
	
	blData <- add(blDataEntryOption1,blData)
	blData <- add(blDataEntryOption2,blData)
	blData <- add(blDataEntryOption3,blData)
	blData <- add(blDataEntryOption4,blData)
	
	return(blData)
	
}