# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldCreateBloombergData <- function() {	
	
	blDataEntry <- new("BloombergDataEntry",blId="NOVN VX Equity",fieldId="Isin",value="10.4",dateLastUpdate=Sys.time())
	
	blData <- new("BloombergData")
	blData[[paste(blDataEntry@blId,blDataEntry@fieldId,sep="__")]] <- blDataEntry


	idBloomberg <- "NOVN VX Equity"
	idTicker <- "Isin"
	id <- paste(idBloomberg,idTicker,sep="__")
	checkEquals(blData[[id]]@value,"10.4")
}


test.shouldAddBlDataEntryToBlData <- function() {	
	
	blDataEntry <- new("BloombergDataEntry",blId="NOVN VX Equity",fieldId="Isin",value="10.4",dateLastUpdate=Sys.time())
	
	blData <- new("BloombergData")
	
	blData <- add(blDataEntry,blData)
	
	idBloomberg <- "NOVN VX Equity"
	idTicker <- "Isin"
	id <- paste(idBloomberg,idTicker,sep="__")
	checkEquals(blData[[id]]@value,"10.4")
}

