# TODO: Add comment
# 
# Author: Claudio
###############################################################################


tradeFactory <- function(trade.l) { 
	trade <- new("Trade",
			dateTime=as.POSIXct(trade.l[["dateTime"]], format = "%Y-%m-%d_%H-%M-%S"),
			owner=trade.l[["owner"]],
			securityID=trade.l[["securityID"]],
			exchange=trade.l[["exchange"]])
	return(trade)
}

