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
	checkEquals(blRequestHandler[["requests"]][[1]]$requestDateTime < Sys.time(),TRUE)
}


test.shuldExecuteBloombergRequestCollection <- function() {
	
	blRequestHandler <- create_BloombergRequestHandler()
	
	checkEquals(blRequestHandler[["execute"]](),list())
	
	blRequestHandler$collect("NOVN VX Equity","ID_ISIN")
	blRequestHandler$collect("UBSN VX Equity","ID_ISIN")
	blRequestHandler$collect("UBSN VX Equity","NAME")
	blRequestHandler$collect("UHR VX Equity","NAME")
	
	if (!is.element("package:Rbloomberg",search())) {
		checkEquals(TRUE,TRUE)
	} else {
		
		blData <- blRequestHandler[["execute"]]()
		checkEquals(length(blData),4)
		checkEquals(blData[["UBSN VX Equity__ID_ISIN"]][["blId"]],"UBSN VX Equity")
		checkEquals(blData[["UBSN VX Equity__ID_ISIN"]][["fieldId"]],"ID_ISIN")
		checkEquals(blData[["UBSN VX Equity__ID_ISIN"]][["value"]],"")
		checkEquals(blData[["UBSN VX Equity__ID_ISIN"]][["dateLastUpdate"]]-Sys.time()<0,TRUE)
	}
}


