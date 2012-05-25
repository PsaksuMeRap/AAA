# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldCreateBloombergData <- function() {	
	
	blDataEntry <- new("BloombergDataEntry",blId="NOVN VX Equity",fieldId="ID_ISIN",value="CH0012005267",dateLastUpdate=Sys.time())
	
	blData <- new("BloombergData")
	blData[[paste(blDataEntry@blId,blDataEntry@fieldId,sep="__")]] <- blDataEntry


	idBloomberg <- "NOVN VX Equity"
	idTicker <- "ID_ISIN"
	id <- paste(idBloomberg,idTicker,sep="__")
	checkEquals(blData[[id]]@value,"CH0012005267")
}

test.shouldAddBlDataEntryToBlDataEntry <- function() {	
	
	blDataEntry1 <- new("BloombergDataEntry",blId="NOVN VX Equity",fieldId="ID_ISIN",value="CH0012005267",dateLastUpdate=Sys.time())
	blDataEntry2 <- new("BloombergDataEntry",blId="UBSN VX Equity",fieldId="ID_ISIN",value="CH0024899483",dateLastUpdate=Sys.time())
	
	
	blData <- add(blDataEntry1,blDataEntry2)
	
	idBloomberg <- "NOVN VX Equity"
	fieldId <- "ID_ISIN"
	id <- paste(idBloomberg,fieldId,sep="__")
	checkEquals(length(blData),2)
	checkEquals(class(blData)[[1]],"BloombergData")
	checkEquals(blData[[id]]@value,"CH0012005267")
	
}


test.shouldAddBlDataEntryToBlData <- function() {	
	
	blDataEntry <- new("BloombergDataEntry",blId="NOVN VX Equity",fieldId="ID_ISIN",value="CH0012005267",dateLastUpdate=Sys.time())
	
	blData <- new("BloombergData")
	
	blData <- add(blDataEntry,blData)
	
	idBloomberg <- "NOVN VX Equity"
	fieldId <- "ID_ISIN"
	id <- paste(idBloomberg,fieldId,sep="__")
	checkEquals(blData[[id]]@value,"CH0012005267")
	
	# add another blDataEntry
	blDataEntry <- new("BloombergDataEntry",blId="UBSN VX Equity",fieldId="ID_ISIN",value="CH0024899483",dateLastUpdate=Sys.time())
	blData <- add(blDataEntry,blData)
	checkEquals(length(blData),2)
	checkEquals(blData[["USBN VX Equity__ID_ISIN"]]@value,"CH0024899483")
	isTrue <- blData[["USBN VX Equity__ID_ISIN"]]@dateLastUpdate > blData[["NOVN VX Equity__ID_ISIN"]]@dateLastUpdate
	checkEquals(isTrue,TRUE)
}

test.shouldAddBlDataToBlData <- function() {	
	
	blDataEntry1 <- new("BloombergDataEntry",blId="NOVN VX Equity",fieldId="ID_ISIN",value="CH0012005267",dateLastUpdate=Sys.time())
	blDataEntry2 <- new("BloombergDataEntry",blId="UBSN VX Equity",fieldId="ID_ISIN",value="CH0024899483",dateLastUpdate=Sys.time())
	blDataEntry3 <- new("BloombergDataEntry",blId="HOLN VX Equity",fieldId="PX_LAST",value=53.25,dateLastUpdate=Sys.time())
	blDataEntry4 <- new("BloombergDataEntry",blId="UHR VX Equity",fieldId="PX_LAST",value=367.90,dateLastUpdate=Sys.time())
	blDataEntry5 <- new("BloombergDataEntry",blId="ADEN VX Equity",fieldId="PX_LAST",value=37.80,dateLastUpdate=Sys.time())
	
	blData1 <- new("BloombergData")
	blData2 <- new("BloombergData")
	
	blData1 <- add(blDataEntry1,blData1)
	blData1 <- add(blDataEntry2,blData1)
	
	blData2 <- add(blDataEntry3,blData2)
	blData2 <- add(blDataEntry4,blData2)
	blData2 <- add(blDataEntry5,blData2)
	
	blData <- add(blData1,blData2)
	
	idBloomberg <- "NOVN VX Equity"
	fieldId <- "ID_ISIN"
	id <- paste(idBloomberg,fieldId,sep="__")
	
	checkEquals(length(blData),5)
	checkEquals(blData[[id]]@value,"CH0012005267")
	idBloomberg <- "UHR VX Equity"
	fieldId <- "PX_LAST"
	id <- paste(idBloomberg,fieldId,sep="__")
	checkEquals(blData[[id]]@value,367.90)	
}

test.shouldAsCharacterBlDataEntry <- function() {	
	
	blDataEntry <- new("BloombergDataEntry",blId="NOVN VX Equity",fieldId="ID_ISIN",value="CH0012005267",dateLastUpdate=as.POSIXct("2012-05-25 13:34:02 CEST"))
	
    result <- as.character(blDataEntry)	
	should <- "NOVN VX Equity / ID_ISIN / CH0012005267 / 2012-05-25 13:34:02"
	
	checkEquals(result,should)
}
