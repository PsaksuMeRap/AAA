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
	checkEquals(blRequestHandler[["requests"]][[2]]$blID,"UBSN VX Equity")
	checkEquals(blRequestHandler[["requests"]][[2]]$fieldId,"PX_LAST")
	checkEquals(blRequestHandler[["requests"]][[1]]$requestDateTime < Sys.time(),TRUE)
}


test.shuldExecuteBloombergRequestCollection <- function() {
	
	blRequestHandler <- create_BloombergRequestHandler()
	
	blRequestHandler$collect("NOVN VX Equity","ID_ISIN")
	blRequestHandler$collect("UBSN VX Equity","ID_ISIN")
	blRequestHandler$collect("UBSN VX Equity","NAME")
	blRequestHandler$collect("UHR VX Equity","NAME")
	
	if (!is.element("package:Rbloomberg",search())) {
		checkEquals(TRUE,TRUE)
	} else {
		
		
	}
}


