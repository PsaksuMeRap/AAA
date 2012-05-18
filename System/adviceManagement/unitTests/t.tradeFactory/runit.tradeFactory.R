# TODO: Add comment
# 
# Author: Claudio
###############################################################################



test.shouldCreateTrade <- function() {
	
	trade.l <- list(
			dateTime="2012-05-09_15-30-22",
			owner="Ghidossi",
			securityID="RocheGA",
			exchange="SWX")
	
	
	trade <- tradeFactory(trade.l)
	
	checkEquals(trade[["dateTime"]],trade.l[["dateTime"]])
	checkEquals(trade$owner,trade.l[["owner"]])
	checkEquals(trade$securityID,trade.l[["securityID"]])
	checkEquals(trade$exchange,trade.l[["exchange"]])
}