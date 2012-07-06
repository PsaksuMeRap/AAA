# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shuldCollectBloombergRequest <- function() {
	
	blRequestHandler <- create_BloombergRequestHandler()
	
	blRequestHandler$collect("NOVN VX Equity","ID_ISIN")
	blRequestHandler$collect("UBSN VX Equity","PX_LAST")
	blRequestHandler$collect("UHR VX Equity","PX_LAST")
	
	checkEquals(length(blRequestHandler[["requests"]]),3)
	checkEquals(blRequestHandler[["requests"]][[2]]$blId,"UBSN VX Equity")
	checkEquals(blRequestHandler[["requests"]][[2]]$fieldId,"PX_LAST")
	checkEquals(blRequestHandler[["requests"]][[1]]$requestDateTime-Sys.time()<=0,TRUE)
}


test.shuldExecuteBloombergRequestCollection <- function() {
	
	blRequestHandler <- create_BloombergRequestHandler()
	
	checkEquals(blRequestHandler[["execute"]](),new("BloombergData"))
	
	blRequestHandler$collect("NOVN VX Equity","ID_ISIN")
	blRequestHandler$collect("UBSN VX Equity","ID_ISIN")
	blRequestHandler$collect("UBSN VX Equity","NAME")
	blRequestHandler$collect("UHR VX Equity","NAME")
	
	if (!is.element("package:Rbbg",search())) {
		checkEquals(TRUE,TRUE)
	} else {
		library("Rbbg")
		blData <- blRequestHandler[["execute"]]()
		checkEquals(length(blData),4)
		checkEquals(blData[["UBSN VX Equity__ID_ISIN"]]@blId,"UBSN VX Equity")
		checkEquals(blData[["UBSN VX Equity__ID_ISIN"]]@fieldId,"ID_ISIN")
		checkEquals(blData[["UBSN VX Equity__ID_ISIN"]]@value,"CH0024899483")
		checkEquals(blData[["UBSN VX Equity__ID_ISIN"]]@dateLastUpdate-Sys.time()<0,TRUE)
		checkEquals(blRequestHandler[["requests"]],list())
	}
}

test.shuldIdentifySomeRequestToRemove <- function() {
	
	blDataEntry1 <- new("BloombergDataEntry",blId="UBSN VX Equity",fieldId="ID_ISIN",value="CH0024899483",dateLastUpdate=Sys.time()-as.difftime(241, units = "mins"))
	blDataEntry2 <- new("BloombergDataEntry",blId="HOLN VX Equity",fieldId="PX_LAST",value=53.25,dateLastUpdate=Sys.time())
	blDataEntry3 <- new("BloombergDataEntry",blId="UHR VX Equity",fieldId="PX_LAST",value=367.90,dateLastUpdate=Sys.time()-as.difftime(500, units = "mins"))
	blDataEntry4 <- new("BloombergDataEntry",blId="ADEN VX Equity",fieldId="PX_LAST",value=37.80,dateLastUpdate=Sys.time()-as.difftime(10, units = "mins"))
	
	blData <- new("BloombergData")
	
	blData <- add(blDataEntry1,blData)
	blData <- add(blDataEntry2,blData)
	blData <- add(blDataEntry3,blData)
	blData <- add(blDataEntry4,blData)
	
	
	blRequestHandler <- create_BloombergRequestHandler()
	
	blRequestHandler$collect("NOVN VX Equity","ID_ISIN")
	blRequestHandler$collect("UBSN VX Equity","ID_ISIN")
	blRequestHandler$collect("UBSN VX Equity","NAME")
	blRequestHandler$collect("UHR VX Equity","PX_LAST")
	blRequestHandler$collect("ADEN VX Equity","PX_LAST")
	
	# for (i in blRequestHandler[["requests"]]) blRequestHandler[["isToRemove"]](i,blData)
	
	result <- lapply(blRequestHandler[["requests"]],blRequestHandler[["isToRemove"]],blData)
}


test.shuldExecute <- function() {
	
	ubsIsin <- "CH0024899483"
	uhrIsin <- "CH0012255151"
	adenIsin <- "CH0012138605"

	blDataEntry1 <- new("BloombergDataEntry",blId="UBSN VX Equity",fieldId="ID_ISIN",value="",dateLastUpdate=Sys.time()-as.difftime(241, units = "mins"))
	blDataEntry2 <- new("BloombergDataEntry",blId="HOLN VX Equity",fieldId="ID_ISIN",value="CH0012214059",dateLastUpdate=Sys.time())
	blDataEntry3 <- new("BloombergDataEntry",blId="UHR VX Equity",fieldId="ID_ISIN",value="",dateLastUpdate=Sys.time()-as.difftime(500, units = "mins"))

	
	blData <- new("BloombergData")
	
	blData <- add(blDataEntry1,blData)
	blData <- add(blDataEntry2,blData)
	blData <- add(blDataEntry3,blData)
	
	Sys.sleep(3)
	blRequestHandler <- create_BloombergRequestHandler()
	
	blRequestHandler$collect("NOVN VX Equity","ID_ISIN")
	blRequestHandler$collect("UBSN VX Equity","ID_ISIN")
	blRequestHandler$collect("UHR VX Equity","ID_ISIN")
	blRequestHandler$collect("ADEN VX Equity","ID_ISIN")
	

	result <- blRequestHandler[["execute"]](blData)
	checkEquals(result[["UHR VX Equity__ID_ISIN"]]@value,uhrIsin)
	checkEquals(result[["ADEN VX Equity__ID_ISIN"]]@value,adenIsin)
	checkEquals(length(result),5)
}
